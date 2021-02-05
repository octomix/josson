package com.octomix.josson;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;
import com.octomix.josson.exception.UnresolvedDatasetException;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static com.octomix.josson.PatternMatcher.*;

class JossonCore {

    static final Mapper MAPPER = new Mapper();

    private enum FilterArrayReturn {
        FIRST_MATCHED,
        ALL_MATCHED
    }

    private enum RelationalOperator {
        EQ("="),
        NE("!="),
        GT(">"),
        GTE(">="),
        LT("<"),
        LTE("<=");

        final String symbol;

        RelationalOperator(String symbol) {
            this.symbol = symbol;
        }

        public static RelationalOperator findRelationalOperatorBySymbol(String symbol) {
            for (RelationalOperator operator : values()) {
                if (operator.symbol.equals(symbol)) {
                    return operator;
                }
            }
            throw new IllegalArgumentException(symbol);
        }
    }

    enum JoinOperator {
        INNER_JOIN_ONE,
        LEFT_JOIN_ONE,
        RIGHT_JOIN_ONE,
        LEFT_JOIN_MANY,
        RIGHT_JOIN_MANY
    }

    static String unquoteString(String quotedString) {
        int last = quotedString.length() - 1;
        if (last < 1 || quotedString.charAt(0) != '\'' || quotedString.charAt(last) != '\'') {
            throw new IllegalArgumentException("Argument is not a valid string literal: " + quotedString);
        }
        return quotedString.substring(1, last).replaceAll("''", "'");
    }

    static boolean anyIsBlank(String[] strings) {
        if (strings == null || strings.length == 0) {
            return true;
        }
        for (int i = strings.length - 1; i >= 0; i--) {
            if (StringUtils.isBlank(strings[i])) {
                return true;
            }
        }
        return false;
    }

    static ValueNode toValueNode(String literal) throws NumberFormatException {
        if (StringUtils.isEmpty(literal)) {
            return null;
        }
        if (literal.equalsIgnoreCase("null")) {
            return NullNode.getInstance();
        }
        if (literal.equalsIgnoreCase("true")) {
            return BooleanNode.TRUE;
        }
        if (literal.equalsIgnoreCase("false")) {
            return  BooleanNode.FALSE;
        }
        if (literal.charAt(0) == '\'') {
            return TextNode.valueOf(unquoteString(literal));
        }
        return new DoubleNode(Double.parseDouble(literal));
    }

    static JsonNode getNodeByKeys(JsonNode node, List<String> keys) {
        if (keys == null || keys.isEmpty()) {
            return node;
        }
        if (node == null || node.isNull()) {
            return null;
        }
        String key = keys.get(0);
        String[] tokens = matchFunctionAndArgument(key);
        if (tokens != null) {
            node = FuncDispatcher.dispatch(node, tokens[0], tokens[1]);
        } else if (node.isValueNode()) {
            return null;
        } else {
            tokens = matchArrayNodeQuery(key);
            if (tokens != null) {
                String field = tokens[0];
                FilterArrayReturn mode = tokens[2] == null
                        ? FilterArrayReturn.FIRST_MATCHED : FilterArrayReturn.ALL_MATCHED;
                node = filterArrayNode(field.isEmpty() ? node : node.get(field), tokens[1], mode);
                if (node != null && node.isArray()) {
                    keys.remove(0);
                    return forEachElement(node, keys, tokens[3], "@".equals(tokens[2]));
                }
            } else if (node.isArray()) {
                return forEachElement(node, keys, "", true);
            } else {
                node = node.get(key);
                if (node != null && node.isArray()) {
                    keys.remove(0);
                    return forEachElement(node, keys, "", true);
                }
            }
        }
        keys.remove(0);
        return getNodeByKeys(node, keys);
    }

    /**
     * Find an element or filtered array in an array node
     *
     * @param arrayNode A json array node
     * @param statement Multiple conditions combined with relational operator
     * @param mode      FIRST_MATCHED - return the 1st matched element
     *                  ALL_MATCHED - return all matched elements in array node
     * @return matched element node or matched elements in array node
     */
    static JsonNode filterArrayNode(JsonNode arrayNode, String statement, FilterArrayReturn mode) {
        if (arrayNode == null || !arrayNode.isArray() || arrayNode.size() == 0) {
            return null;
        }
        if (statement.isEmpty()) {
            if (FilterArrayReturn.FIRST_MATCHED == mode) {
                return arrayNode.get(0);
            }
            return arrayNode;
        }
        ArrayNode matchedNodes = null;
        if (FilterArrayReturn.FIRST_MATCHED != mode) {
            matchedNodes = MAPPER.createArrayNode();
        }
        try {
            if (FilterArrayReturn.FIRST_MATCHED == mode) {
                return arrayNode.get(Integer.parseInt(statement));
            }
            matchedNodes.add(arrayNode.get(Integer.parseInt(statement)));
            return matchedNodes;
        } catch (NumberFormatException e) {
            // continue
        }
        LogicalOpStack opStack = new LogicalOpStack(arrayNode);
        for (int i = 0; i < arrayNode.size(); i++) {
            opStack.clear();
            List<String[]> tokens = decomposeConditions(statement);
            for (String[] token : tokens) {
                try {
                    opStack.evaluate(token[0], token[1], i);
                } catch (IllegalArgumentException e) {
                    if (e.getMessage() == null) {
                        throw new IllegalArgumentException(statement);
                    }
                    throw new IllegalArgumentException("\"" + e.getMessage() + "\" in " + statement);
                }
            }
            JsonNode result = opStack.finalResult(i);
            if (result != null && result.asBoolean()) {
                if (FilterArrayReturn.FIRST_MATCHED == mode) {
                    return arrayNode.get(i);
                }
                matchedNodes.add(arrayNode.get(i));
            }
        }
        return matchedNodes;
    }

    static JsonNode evaluateExpression(String expression, Map<String, Josson> datasets)
            throws UnresolvedDatasetException {
        try {
            return toValueNode(expression);
        } catch (NumberFormatException e) {
            // continue
        }
        if (datasets.containsKey(expression)) {
            Josson josson = datasets.get(expression);
            if (josson == null) {
                return null;
            }
            return josson.getNode();
        }
        String[] tokens = matchJsonQuery(expression);
        if (tokens == null) {
            throw new UnresolvedDatasetException(expression);
        }
        if (!datasets.containsKey(tokens[0])) {
            throw new UnresolvedDatasetException(tokens[0]);
        }
        Josson josson = datasets.get(tokens[0]);
        if (josson == null) {
            return null;
        }
        JsonNode node = josson.getNode(tokens[1]);
        datasets.put(expression, node == null ? null : Josson.create(node));
        return node;
    }

    static BooleanNode relationalCompare(JsonNode leftNode, String operator, JsonNode rightNode) {
        RelationalOperator oper = RelationalOperator.findRelationalOperatorBySymbol(operator);
        if (leftNode == null) {
            leftNode = NullNode.getInstance();
        }
        if (rightNode == null) {
            rightNode = NullNode.getInstance();
        }
        if (rightNode.isTextual()) {
            if (leftNode.isTextual()) {
                int compareResult = leftNode.asText().compareTo(rightNode.asText());
                switch (oper) {
                    case EQ:
                        return BooleanNode.valueOf(compareResult == 0);
                    case NE:
                        return BooleanNode.valueOf(compareResult != 0);
                    case GT:
                        return BooleanNode.valueOf(compareResult > 0);
                    case GTE:
                        return BooleanNode.valueOf(compareResult >= 0);
                    case LT:
                        return BooleanNode.valueOf(compareResult < 0);
                    case LTE:
                        return BooleanNode.valueOf(compareResult <= 0);
                }
            }
            JsonNode swap = leftNode;
            leftNode = rightNode;
            rightNode = swap;
            switch (oper) {
                case GT:
                    oper = RelationalOperator.LT;
                    break;
                case GTE:
                    oper = RelationalOperator.LTE;
                    break;
                case LT:
                    oper = RelationalOperator.GT;
                    break;
                case LTE:
                    oper = RelationalOperator.GTE;
                    break;
            }
        }
        if (!leftNode.isContainerNode() && rightNode.isNumber()) {
            try {
                double value = leftNode.isNumber() ? leftNode.asDouble() : Double.parseDouble(leftNode.asText());
                switch (oper) {
                    case EQ:
                        return BooleanNode.valueOf(value == rightNode.asDouble());
                    case NE:
                        return BooleanNode.valueOf(value != rightNode.asDouble());
                    case GT:
                        return BooleanNode.valueOf(value > rightNode.asDouble());
                    case GTE:
                        return BooleanNode.valueOf(value >= rightNode.asDouble());
                    case LT:
                        return BooleanNode.valueOf(value < rightNode.asDouble());
                    case LTE:
                        return BooleanNode.valueOf(value <= rightNode.asDouble());
                }
            } catch (NumberFormatException e) {
                return BooleanNode.FALSE;
            }
        }
        if (!leftNode.isContainerNode() && rightNode.isBoolean()) {
            switch (oper) {
                case EQ:
                    return BooleanNode.valueOf(!leftNode.asBoolean() ^ rightNode.asBoolean());
                case NE:
                    return BooleanNode.valueOf(leftNode.asBoolean() ^ rightNode.asBoolean());
            }
        } else {
            switch (oper) {
                case EQ:
                    return BooleanNode.valueOf(leftNode.isNull() && rightNode.isNull());
                case NE:
                    return BooleanNode.valueOf(leftNode.isNull() ^ rightNode.isNull());
            }
        }
        return BooleanNode.FALSE;
    }

    static JsonNode forEachElement(JsonNode node, List<String> keys, String funcChain, boolean flattenArray) {
        List<String> functions = decomposeNestedResultFunctions(funcChain);
        ArrayNode matchedNodes = MAPPER.createArrayNode();
        for (int i = 0; i < node.size(); i++) {
            JsonNode tryNode = getNodeByKeys(node.get(i), new ArrayList<>(keys));
            if (tryNode != null) {
                if (tryNode.isArray() && flattenArray) {
                    matchedNodes.addAll((ArrayNode) tryNode);
                } else {
                    matchedNodes.add(tryNode);
                }
            }
        }
        return getNodeByKeys(matchedNodes, functions);
    }

    static JsonNode joinNodes(JsonNode leftNode, String[] leftKeys, String leftArrayName, JoinOperator operator,
                              JsonNode rightNode, String[] rightKeys, String rightArrayName) {
        String arrayName;
        if (operator == JoinOperator.RIGHT_JOIN_ONE || operator == JoinOperator.RIGHT_JOIN_MANY
                || (operator == JoinOperator.INNER_JOIN_ONE && !leftNode.isObject() && rightNode.isObject())) {
            JsonNode swapNode = leftNode;
            leftNode = rightNode;
            rightNode = swapNode;
            String[] swapKeys = leftKeys;
            leftKeys = rightKeys;
            rightKeys = swapKeys;
            if (operator == JoinOperator.RIGHT_JOIN_ONE) {
                operator = JoinOperator.LEFT_JOIN_ONE;
            } else if (operator == JoinOperator.RIGHT_JOIN_MANY) {
                operator = JoinOperator.LEFT_JOIN_MANY;
            }
            arrayName = leftArrayName;
        } else {
            arrayName = rightArrayName;
        }
        ArrayNode rightArray;
        if (rightNode.isArray()) {
            rightArray = (ArrayNode) rightNode;
        } else {
            rightArray = Josson.createArrayNode();
            rightArray.add(rightNode);
        }
        if (leftNode.isObject()) {
            return joinToObjectNode((ObjectNode) leftNode, leftKeys, operator, rightArray, rightKeys, arrayName);
        }
        ArrayNode joinedArray = Josson.createArrayNode();
        for (int i = 0; i < leftNode.size(); i++) {
            if (leftNode.get(i).isObject()) {
                ObjectNode joinedNode = joinToObjectNode(
                        (ObjectNode) leftNode.get(i), leftKeys, operator, rightArray, rightKeys, arrayName);
                if (joinedNode != null) {
                    joinedArray.add(joinedNode);
                }
            }
        }
        return joinedArray;
    }

    static ObjectNode joinToObjectNode(ObjectNode leftObject, String[] leftKeys, JoinOperator operator,
                                       ArrayNode rightArray, String[] rightKeys, String arrayName) {
        String[] conditions = new String[leftKeys.length];
        for (int j = leftKeys.length - 1; j >= 0; j--) {
            JsonNode leftValue = Josson.getNode(leftObject, leftKeys[j]);
            if (leftValue == null || !leftValue.isValueNode()) {
                return null;
            }
            conditions[j] = rightKeys[j]
                    + (leftValue.isTextual() ? "='" : "=") + leftValue.asText().replaceAll("'", "''")
                    + (leftValue.isTextual() ? "'" : "");
        }
        if (operator == JoinOperator.LEFT_JOIN_MANY) {
            JsonNode rightToJoin = Josson.getNode(
                    rightArray, "[" + StringUtils.join(conditions, " & ") + "]@");
            if (rightToJoin != null) {
                ObjectNode joinedNode = leftObject.deepCopy();
                joinedNode.set(arrayName, rightToJoin);
                return joinedNode;
            }
        } else {
            JsonNode rightToJoin = Josson.getNode(
                    rightArray, "[" + StringUtils.join(conditions, " & ") + "]");
            if (rightToJoin != null && rightToJoin.isObject()) {
                ObjectNode joinedNode = leftObject.deepCopy();
                joinedNode.setAll((ObjectNode) rightToJoin);
                return joinedNode;
            }
            if (operator == JoinOperator.INNER_JOIN_ONE) {
                return null;
            }
        }
        return leftObject;
    }
}

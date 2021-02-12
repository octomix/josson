package com.octomix.josson;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;
import com.octomix.josson.exception.UnresolvedDatasetException;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static com.octomix.josson.Josson.getNode;
import static com.octomix.josson.PatternMatcher.*;

class JossonCore {

    static final Mapper MAPPER = new Mapper();

    private enum FilterArrayReturn {
        FIRST_MATCHED,
        ALL_MATCHED
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
        if (literal.indexOf('.') < 0) {
            return IntNode.valueOf(Integer.parseInt(literal));
        }
        return DoubleNode.valueOf(Double.parseDouble(literal));
    }

    static JsonNode getIndexNode(int index, String functions) {
        List<String> keys = decomposePaths(functions);
        String indexType = keys.remove(0);
        switch (indexType) {
            case "#":
                break;
            case "##":
                index++;
                break;
            default:
                throw new IllegalArgumentException("Invalid syntax: " + indexType);
        }
        return getNodeByKeys(IntNode.valueOf(index), keys);
    }

    static boolean nodeHasValue(JsonNode node) {
        return node != null && !node.isNull() && node.isValueNode();
    }

    static Object valueAsObject(JsonNode node) {
        if (!nodeHasValue(node)) {
            return null;
        }
        if (node.isIntegralNumber()) {
            return node.asInt();
        }
        if (node.isNumber()) {
            return node.asDouble();
        }
        return node.asText();
    }

    static Object[] valuesAsObjects(JsonNode node, List<String> paramList, int index) {
        Object[] objects;
        int size = paramList.size();
        if (size == 0) {
            Object valueObject = valueAsObject(node);
            if (valueObject == null) {
                return null;
            }
            objects = new Object[1];
            objects[0] = valueObject;
        } else {
            objects = new Object[size];
            for (int i = 0; i < size; i++) {
                String param = paramList.get(i);
                Object valueObject;
                if ("?".equals(param)) {
                    valueObject = valueAsObject(node);
                } else if (param.charAt(0) == '#') {
                    valueObject = valueAsObject(getIndexNode(index, param));
                } else {
                    valueObject = valueAsObject(getNode(node, param));
                }
                if (valueObject == null) {
                    return null;
                }
                objects[i] = valueObject;
            }
        }
        return objects;
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
}

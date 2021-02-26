package com.octomix.josson;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;
import org.apache.commons.lang3.StringUtils;

import java.util.*;

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

    static String getNodeAsText(JsonNode node, String jossonPath) {
        node = getNode(node, jossonPath);
        return node == null ? "" : node.asText();
    }

    static JsonNode getIndexId(int index, String path) {
        List<String> keys = decomposePaths(path);
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
                    valueObject = valueAsObject(getIndexId(index, param));
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
            if (tokens[2] == null && node.isArray()) {
                ArrayNode array = MAPPER.createArrayNode();
                for (int i = 0; i < node.size(); i++) {
                    array.add(FuncDispatcher.dispatch(node.get(i), tokens[0], tokens[1]));
                }
                node = array;
            } else {
                node = FuncDispatcher.dispatch(node, tokens[0], tokens[1]);
            }
        } else if (node.isValueNode()) {
            return null;
        } else {
            tokens = matchFilterQuery(key);
            if (tokens != null) {
                FilterArrayReturn mode = tokens[2] == null ?
                        FilterArrayReturn.FIRST_MATCHED : FilterArrayReturn.ALL_MATCHED;
                if (tokens[0].isEmpty()) {
                    node = evaluateFilter(node, tokens[1], mode);
                } else {
                    JsonNode tryNode = node.get(tokens[0]);
                    if (tryNode == null || !tryNode.isArray()) {
                        tryNode = forEachElement(node, new ArrayList<>(Collections.singletonList(tokens[0])));
                    }
                    node = evaluateFilter(tryNode, tokens[1], mode);
                }
            } else if (node.isArray()) {
                return forEachElement(node, keys);
            } else {
                node = node.get(key);
            }
        }
        keys.remove(0);
        return getNodeByKeys(node, keys);
    }

    /**
     * Find an element or filter an array node
     *
     * @param node the Jackson JsonNode to be processed
     * @param statement multiple conditions combined with relational operator
     * @param mode {@code FIRST_MATCHED} | {@code ALL_MATCHED}
     * @return The 1st matched element for {@code FIRST_MATCHED} or
     * all matched elements in an array node for {@code ALL_MATCHED}
     */
    private static JsonNode evaluateFilter(JsonNode node, String statement, FilterArrayReturn mode) {
        if (node == null) {
            return null;
        }
        if (statement.isEmpty()) {
            return node;
        }
        if (node.isArray()) {
            if (node.size() == 0) {
                return null;
            }
        } else {
            JsonNode result = new LogicalOpStack(node).evaluate(statement, 0);
            if (result != null && result.asBoolean()) {
                return node;
            }
            return null;
        }
        ArrayNode matchedNodes = null;
        if (FilterArrayReturn.FIRST_MATCHED != mode) {
            matchedNodes = MAPPER.createArrayNode();
        }
        try {
            if (FilterArrayReturn.FIRST_MATCHED == mode) {
                return node.get(Integer.parseInt(statement));
            }
            matchedNodes.add(node.get(Integer.parseInt(statement)));
            return matchedNodes;
        } catch (NumberFormatException e) {
            // continue
        }
        LogicalOpStack opStack = new LogicalOpStack(node);
        for (int i = 0; i < node.size(); i++) {
            JsonNode result = opStack.evaluate(statement, i);
            if (result != null && result.asBoolean()) {
                if (FilterArrayReturn.FIRST_MATCHED == mode) {
                    return node.get(i);
                }
                matchedNodes.add(node.get(i));
            }
        }
        return matchedNodes;
    }

    private static JsonNode forEachElement(JsonNode node, List<String> keys) {
        List<String> keysAfterGrouping = new ArrayList<>();
        boolean isFlattenArray = true;
        for (int i = 0; i < keys.size(); i++) {
            boolean isGroupInArray = keys.get(i).startsWith("@");
            if (!isGroupInArray && i > 0) {
                String[] tokens = matchFilterQuery(keys.get(i));
                if (tokens != null) {
                    isGroupInArray = true;
                    isFlattenArray = !tokens[1].isEmpty();
                }
            }
            if (isGroupInArray) {
                if (!isFlattenArray) {
                    keys.remove(i);
                }
                while (i < keys.size()) {
                    keysAfterGrouping.add(keys.remove(i));
                }
                break;
            }
        }
        ArrayNode matchedNodes = MAPPER.createArrayNode();
        for (int i = 0; i < node.size(); i++) {
            JsonNode tryNode = getNodeByKeys(node.get(i), new ArrayList<>(keys));
            if (tryNode != null) {
                if (isFlattenArray && tryNode.isArray()) {
                    matchedNodes.addAll((ArrayNode) tryNode);
                } else {
                    matchedNodes.add(tryNode);
                }
            }
        }
        return getNodeByKeys(matchedNodes, keysAfterGrouping);
    }
}

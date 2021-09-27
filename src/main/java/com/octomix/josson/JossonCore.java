/*
 * Copyright 2020 Octomix Software Technology Limited
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.octomix.josson;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;
import com.octomix.josson.commons.StringUtils;

import java.util.*;

import static com.octomix.josson.Josson.getNode;
import static com.octomix.josson.PatternMatcher.*;

class JossonCore {

    static final Mapper MAPPER = new Mapper();

    private enum FilterMode {
        FIRST_MATCHED(null),
        ALL_MATCHED("*"),
        NESTED_ARRAY("@");

        final String indicator;

        FilterMode(String indicator) {
            this.indicator = indicator;
        }

        private static FilterMode fromIndicator(String indicator) {
            for (FilterMode mode : values()) {
                if (StringUtils.equals(mode.indicator, indicator)) {
                    return mode;
                }
            }
            return null;
        }
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
            return BooleanNode.FALSE;
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
        String key = keys.remove(0);
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
            return getNodeByKeys(node, keys);
        }
        if (node.isValueNode()) {
            return null;
        }
        tokens = matchFilterQuery(key);
        FilterMode mode = FilterMode.fromIndicator(tokens[2]);
        if (tokens[1] != null) {
            if (!tokens[0].isEmpty()) {
                if (node.isArray()) {
                    node = forEachElement((ArrayNode) node, tokens[0], FilterMode.ALL_MATCHED, null);
                } else {
                    node = node.get(tokens[0]);
                }
            }
            node = evaluateFilter(node, tokens[1], mode);
            if (node == null) {
                return null;
            }
            if (node.isArray()) {
                return forEachElement((ArrayNode) node, null, mode, keys);
            }
            return getNodeByKeys(node, keys);
        }
        if (node.isArray()) {
            return forEachElement((ArrayNode) node, tokens[0], mode, keys);
        }
        return getNodeByKeys(node.get(tokens[0]), keys);
    }

    /**
     * Find an element or filter an array node
     *
     * @param node      the Jackson JsonNode to be processed
     * @param statement multiple conditions combined with relational operator
     * @param mode      {@code FIRST_MATCHED} | {@code ALL_MATCHED} | {@code NESTED_ARRAY}
     * @return The 1st matched element for {@code FIRST_MATCHED} or
     * all matched elements in an array node for {@code ALL_MATCHED} and {@code NESTED_ARRAY}
     */
    private static JsonNode evaluateFilter(JsonNode node, String statement, FilterMode mode) {
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
        if (FilterMode.FIRST_MATCHED != mode) {
            matchedNodes = MAPPER.createArrayNode();
        }
        try {
            if (FilterMode.FIRST_MATCHED == mode) {
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
                if (FilterMode.FIRST_MATCHED == mode) {
                    return node.get(i);
                }
                matchedNodes.add(node.get(i));
            }
        }
        return matchedNodes;
    }

    private static JsonNode forEachElement(ArrayNode node, String elem, FilterMode mode, List<String> keys) {
        ArrayNode matchedNodes = MAPPER.createArrayNode();
        for (int i = 0; i < node.size(); i++) {
            addEachElement(matchedNodes, elem == null ? node.get(i) : node.get(i).get(elem), mode, keys);
        }
        return mode == FilterMode.NESTED_ARRAY ? matchedNodes : getNodeByKeys(matchedNodes, keys);
    }

    private static JsonNode forEachElement(ArrayNode node, List<String> keys) {
        List<String> keysAfterGrouping = new ArrayList<>();
        FilterMode mode = FilterMode.ALL_MATCHED;
        for (int i = 0; i < keys.size(); i++) {
            String[] tokens = matchFilterQuery(keys.get(i));
            mode = FilterMode.fromIndicator(tokens[2]);
            if (mode != FilterMode.FIRST_MATCHED || tokens[0].startsWith("@")) {
                while (i < keys.size()) {
                    keysAfterGrouping.add(keys.remove(i));
                }
                break;
            }
        }
        ArrayNode matchedNodes = MAPPER.createArrayNode();
        for (int i = 0; i < node.size(); i++) {
            addEachElement(matchedNodes, getNodeByKeys(node.get(i), new ArrayList<>(keys)), mode, keysAfterGrouping);
        }
        return mode == FilterMode.NESTED_ARRAY ? matchedNodes : getNodeByKeys(matchedNodes, keysAfterGrouping);
    }

    private static void addEachElement(ArrayNode array, JsonNode node, FilterMode mode, List<String> keys) {
        if (node == null) {
            return;
        }
        if (mode == FilterMode.NESTED_ARRAY) {
            if (node.isArray()) {
                array.add(forEachElement((ArrayNode) node, new ArrayList<>(keys)));
            } else {
                array.add(getNodeByKeys(node, new ArrayList<>(keys)));
            }
        } else if (mode == FilterMode.ALL_MATCHED && node.isArray()) {
            array.addAll((ArrayNode) node);
        } else {
            array.add(node);
        }
    }
}

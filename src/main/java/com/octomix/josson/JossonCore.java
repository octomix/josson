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

import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeParseException;
import java.util.*;

import static com.octomix.josson.ArrayFilter.FilterMode;
import static com.octomix.josson.ArrayFilter.FilterMode.*;
import static com.octomix.josson.FuncDispatcher.isArrayModeFunction;
import static com.octomix.josson.Josson.getNode;
import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.PatternMatcher.*;

class JossonCore {

    static final char QUOTE_SYMBOL = '\'';
    static final char INDEX_SYMBOL = '#';
    static final char CURRENT_NODE_SYMBOL = '?';

    private static final String ZERO_BASED_INDEX = "" + INDEX_SYMBOL;
    private static final String ONE_BASED_INDEX = ZERO_BASED_INDEX + INDEX_SYMBOL;

    static ZoneId zoneId = ZoneId.systemDefault();

    static String unquoteString(String quotedString) {
        int last = quotedString.length() - 1;
        if (last < 1 || quotedString.charAt(0) != QUOTE_SYMBOL || quotedString.charAt(last) != QUOTE_SYMBOL) {
            throw new IllegalArgumentException("Argument is not a valid string literal: " + quotedString);
        }
        return quotedString.substring(1, last).replace("''", "'");
    }

    static String csvQuote(String input) {
        String result;
        boolean needQuote;
        if (input.contains("\"")) {
            needQuote = true;
            result = input.replace("\"", "\"\"");
        } else {
            needQuote = input.contains(",");
            result = input;
        }
        if (needQuote) {
            return "\"" + result + "\"";
        }
        return result;
    }

    static boolean isCurrentNodeSymbol(String path) {
        if (path.length() > 0 && path.charAt(0) == CURRENT_NODE_SYMBOL) {
            if (path.length() > 1) {
                throw new IllegalArgumentException("Invalid syntax: " + path);
            }
            return true;
        }
        return false;
    }

    static boolean anyIsBlank(String[] strings) {
        if (strings == null || strings.length == 0) {
            return true;
        }
        for (String str : strings) {
            if (StringUtils.isBlank(str)) {
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
        if (literal.charAt(0) == QUOTE_SYMBOL) {
            return TextNode.valueOf(unquoteString(literal));
        }
        if (literal.indexOf('.') < 0) {
            return IntNode.valueOf(Integer.parseInt(literal));
        }
        return DoubleNode.valueOf(Double.parseDouble(literal));
    }

    static LocalDateTime toLocalDateTime(JsonNode node) {
        try {
            return LocalDateTime.parse(node.asText());
        } catch (DateTimeParseException e) {
            throw new IllegalArgumentException(e.getMessage());
        }
    }

    static LocalDateTime offsetToLocalDateTime(JsonNode node) {
        return toOffsetDateTime(node).atZoneSameInstant(zoneId).toLocalDateTime();
    }

    static OffsetDateTime toOffsetDateTime(JsonNode node) {
        try {
            return OffsetDateTime.parse(node.asText());
        } catch (DateTimeParseException e) {
            throw new IllegalArgumentException(e.getMessage());
        }
    }

    static OffsetDateTime localToOffsetDateTime(JsonNode node) {
        LocalDateTime dateTime = toLocalDateTime(node);
        return dateTime.atOffset(zoneId.getRules().getOffset(dateTime));
    }

    static String getNodeAsText(JsonNode node, String jossonPath) {
        node = getNode(node, jossonPath);
        return node == null ? "" : node.asText();
    }

    static JsonNode getIndexId(int index, String path) {
        List<String> keys = decomposePaths(path);
        String indexType = keys.remove(0);
        switch (indexType) {
            case ZERO_BASED_INDEX:
                break;
            case ONE_BASED_INDEX:
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
                if (param.charAt(0) == INDEX_SYMBOL) {
                    valueObject = valueAsObject(getIndexId(index, param));
                } else if (isCurrentNodeSymbol(param)) {
                    valueObject = valueAsObject(node);
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
        FuncDispatcher funcDispatcher = matchFunctionAndArgument(key);
        if (funcDispatcher != null) {
            return getNodeByKeys(funcDispatcher.apply(node), keys);
        }
        if (node.isValueNode()) {
            return null;
        }
        ArrayFilter arrayFilter = matchFilterQuery(key);
        if (arrayFilter.getFilter() != null) {
            if (!arrayFilter.getArrayName().isEmpty()) {
                if (node.isArray()) {
                    node = forEachElement((ArrayNode) node, arrayFilter.getArrayName(), FILTER_FIND_ALL, null);
                } else {
                    node = node.get(arrayFilter.getArrayName());
                }
            }
            node = evaluateFilter(node, arrayFilter.getFilter(), arrayFilter.getMode());
            if (node == null) {
                return null;
            }
            if (node.isArray()) {
                return forEachElement((ArrayNode) node, null, arrayFilter.getMode(), keys);
            }
            return getNodeByKeys(node, keys);
        }
        if (node.isArray()) {
            return forEachElement((ArrayNode) node, arrayFilter.getArrayName(), arrayFilter.getMode(), keys);
        }
        return getNodeByKeys(node.get(arrayFilter.getArrayName()), keys);
    }

    /**
     * Find an element or filter an array node
     *
     * @param node      the Jackson JsonNode to be processed
     * @param statement multiple relational operations combined with logical operators
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
        if (mode != FILTER_FIND_FIRST) {
            matchedNodes = MAPPER.createArrayNode();
        }
        try {
            if (mode == FILTER_FIND_FIRST) {
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
                if (mode == FILTER_FIND_FIRST) {
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
        return mode == FILTER_NESTED_ARRAY ? matchedNodes : getNodeByKeys(matchedNodes, keys);
    }

    private static JsonNode forEachElement(ArrayNode node, List<String> keys) {
        List<String> keysAfterGrouping = new ArrayList<>();
        FilterMode mode = FILTER_FIND_ALL;
        for (int i = 0; i < keys.size(); i++) {
            ArrayFilter arrayFilter = matchFilterQuery(keys.get(i));
            String arrayName = arrayFilter.getArrayName();
            mode = arrayFilter.getMode();
            if (mode == FILTER_FIND_ALL || mode == FILTER_NESTED_ARRAY || isArrayModeFunction(arrayName)) {
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
        return mode == FILTER_NESTED_ARRAY ? matchedNodes : getNodeByKeys(matchedNodes, keysAfterGrouping);
    }

    private static void addEachElement(ArrayNode array, JsonNode node, FilterMode mode, List<String> keys) {
        if (node == null) {
            return;
        }
        if (mode == FILTER_NESTED_ARRAY) {
            if (node.isArray()) {
                array.add(forEachElement((ArrayNode) node, new ArrayList<>(keys)));
            } else {
                array.add(getNodeByKeys(node, new ArrayList<>(keys)));
            }
        } else if (mode == FILTER_FIND_ALL && node.isArray()) {
            array.addAll((ArrayNode) node);
        } else {
            array.add(node);
        }
    }
}

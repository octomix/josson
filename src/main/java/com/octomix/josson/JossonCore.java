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

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeParseException;
import java.util.*;

import static com.octomix.josson.ArrayFilter.FilterMode;
import static com.octomix.josson.ArrayFilter.FilterMode.*;
import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.PatternMatcher.*;

class JossonCore {

    static final char QUOTE_SYMBOL = '\'';
    private static final char INDEX_SYMBOL = '#';
    private static final char PARENT_ARRAY_SYMBOL = '@';
    private static final char COLLECT_BRANCHES_SYMBOL = '@';

    static final String CURRENT_NODE_PATH = "?";
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

    static JsonNode getImplicitVariable(String name) {
        if (name.charAt(0) == '$') {
            switch (StringUtils.stripStart(name.substring(1), null).toLowerCase()) {
                case "":
                    return BooleanNode.TRUE;
                case "now":
                    return TextNode.valueOf(LocalDateTime.now().toString());
                case "today":
                    return TextNode.valueOf(LocalDate.now().atStartOfDay().toString());
                case "yesterday":
                    return TextNode.valueOf(LocalDate.now().atStartOfDay().minusDays(1).toString());
                case "tomorrow":
                    return TextNode.valueOf(LocalDate.now().atStartOfDay().plusDays(1).toString());
            }
        }
        return null;
    }

    static boolean isCurrentNodePath(String path) {
        if (path.startsWith(CURRENT_NODE_PATH)) {
            if (path.length() > 1) {
                throw new IllegalArgumentException("Invalid path: " + path);
            }
            return true;
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
        node = getNodeByPath(node, jossonPath);
        return node == null ? "" : node.asText();
    }

    static JsonNode getIndexId(int index, List<String> keys) {
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
                if (index < 0) {
                    valueObject = valueAsObject(getNodeByPath(node, param));
                } else {
                    valueObject = valueAsObject(getNodeByPath(node, index, param));
                }
                if (valueObject == null) {
                    return null;
                }
                objects[i] = valueObject;
            }
        }
        return objects;
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
            JsonNode result = new OperationStack(node).evaluate(statement, 0);
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
        OperationStack opStack = new OperationStack(node);
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

    static JsonNode getNodeByPath(JsonNode node, String jossonPath) {
        return getNodeByKeys(node, decomposePaths(jossonPath));
    }

    static JsonNode getNodeByPath(JsonNode node, int index, String jossonPath) {
        return getNodeByKeys(node, index, decomposePaths(jossonPath));
    }

    static JsonNode getNodeByKeys(JsonNode node, int index, List<String> keys) {
        if (node == null) {
            return null;
        }
        if (keys.isEmpty()) {
            return node.get(index);
        }
        switch (keys.get(0).charAt(0)) {
            case INDEX_SYMBOL:
                return getIndexId(index, keys);
            case PARENT_ARRAY_SYMBOL:
                String key = keys.remove(0);
                if (key.length() > 1) {
                    throw new IllegalArgumentException("Invalid path: " + key);
                }
                break;
            default:
                if (node.isArray()) {
                    node = node.get(index);
                }
        }
        return getNodeByKeys(node, keys);
    }

    static JsonNode getNodeByKeys(JsonNode node, List<String> keys) {
        if (node != null && !keys.isEmpty()) {
            try {
                node = toValueNode(keys.get(0));
                keys.remove(0);
            } catch (NumberFormatException e) {
                // continue
            }
            while (node != null && !keys.isEmpty()) {
                List<String> nextKeys = new ArrayList<>();
                node = getNodeByKeys(node, keys, nextKeys);
                keys = nextKeys;
            }
        }
        return node;
    }

    private static JsonNode getNodeByKeys(JsonNode node, List<String> keys, List<String> nextKeys) {
        if (keys == null || keys.isEmpty()) {
            return node;
        }
        if (node == null || node.isNull()) {
            return null;
        }
        String key = keys.get(0);
        if (key.charAt(0) == COLLECT_BRANCHES_SYMBOL) {
            nextKeys.addAll(keys);
            nextKeys.set(0, key.substring(1).trim());
            keys.clear();
            return node;
        }
        keys.remove(0);
        if (isCurrentNodePath(key)) {
            return getNodeByKeys(node, keys, nextKeys);
        }
        FuncDispatcher funcDispatcher = matchFunctionAndArgument(key);
        if (funcDispatcher != null) {
            return getNodeByKeys(funcDispatcher.apply(node), keys, nextKeys);
        }
        if (node.isValueNode()) {
            return null;
        }
        ArrayFilter filter = matchFilterQuery(key);
        if (filter.getFilter() == null) {
            if (node.isArray()) {
                return forEachElement((ArrayNode) node, filter.getNodeName(), filter.getMode(), keys, nextKeys);
            }
            node = node.get(filter.getNodeName());
        } else {
            if (!filter.getNodeName().isEmpty()) {
                node = getNodeByPath(node, filter.getNodeName());
            }
            node = evaluateFilter(node, filter.getFilter(), filter.getMode());
        }
        if (node == null) {
            return null;
        }
        if (node.isArray()) {
            return forEachElement((ArrayNode) node, null, filter.getMode(), keys, nextKeys);
        }
        return getNodeByKeys(node, keys, nextKeys);
    }

    private static JsonNode forEachElement(ArrayNode node, String elem, FilterMode mode, List<String> keys, List<String> nextKeys) {
        ArrayNode array = MAPPER.createArrayNode();
        if (mode == FILTER_NESTED_ARRAY) {
            for (int i = 0; i < node.size(); i++) {
                List<String> nextNextKeys = new ArrayList<>();
                JsonNode addNode = getNodeByKeys(
                        elem == null ? node.get(i) : node.get(i).get(elem), new ArrayList<>(keys), nextNextKeys);
                if (addNode != null) {
                    array.add(addNode);
                }
                if (nextKeys.isEmpty() && !nextNextKeys.isEmpty()) {
                    nextKeys.addAll(nextNextKeys);
                }
            }
            keys.clear();
            keys.addAll(nextKeys);
            nextKeys.clear();
        } else {
            for (int i = 0; i < node.size(); i++) {
                JsonNode addNode = elem == null ? node.get(i) : node.get(i).get(elem);
                if (addNode != null) {
                    if (mode == FILTER_FIND_ALL && addNode.isArray()) {
                        array.addAll((ArrayNode) addNode);
                    } else {
                        array.add(addNode);
                    }
                }
            }
        }
        return getNodeByKeys(array, keys, nextKeys);
    }
}

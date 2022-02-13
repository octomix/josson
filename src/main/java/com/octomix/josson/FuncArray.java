/*
 * Copyright 2020-2022 Octomix Software Technology Limited
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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static com.octomix.josson.FuncExecutor.*;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Mapper.MAPPER;

/**
 * Array functions.
 */
class FuncArray {

    private FuncArray() {
    }

    static ValueNode funcAggregate(final JsonNode node, final String funcId, final String params) {
        final ArrayNode array = getParamArrayOrItself(params, node);
        if (array == null) {
            return null;
        }
        double sum = 0;
        int count = 0;
        for (int i = array.size() - 1; i >= 0; i--) {
            final JsonNode tryNode = array.get(i);
            if (nodeHasValue(tryNode)) {
                sum += tryNode.asDouble();
                count++;
            }
        }
        if ("count".equals(funcId)) {
            return IntNode.valueOf(count);
        }
        if (count > 0) {
            switch (funcId) {
                case "sum":
                    return DoubleNode.valueOf(sum);
                case "avg":
                    return DoubleNode.valueOf(sum / count);
            }
        }
        return null;
    }

    static JsonNode funcDistinct(final JsonNode node, final String params) {
        final ArrayNode array = getParamArrayOrItself(params, node);
        if (array == null) {
            return null;
        }
        final Set<String> texts = new HashSet<>();
        final Set<Double> doubles = new HashSet<>();
        final Set<Boolean> booleans = new HashSet<>();
        for (int i = 0; i < array.size(); i++) {
            final JsonNode tryNode = array.get(i);
            if (tryNode.isTextual()) {
                texts.add(tryNode.asText());
            } else if (tryNode.isNumber()) {
                doubles.add(tryNode.asDouble());
            } else if (tryNode.isBoolean()) {
                booleans.add(tryNode.asBoolean());
            }
        }
        final ArrayNode result = MAPPER.createArrayNode();
        texts.forEach(value -> result.add(TextNode.valueOf(value)));
        doubles.forEach(value -> result.add(DoubleNode.valueOf(value)));
        booleans.forEach(value -> result.add(BooleanNode.valueOf(value)));
        return result;
    }

    static JsonNode funcFindByMaxMin(JsonNode node, final String params, final boolean isMax, final int nullPriority) {
        final Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        if (!node.isArray()) {
            return node;
        }
        final String path = pathAndParams.getValue().get(0);
        int foundIndex = -1;
        Double maxMinDouble = null;
        String maxMinString = null;
        for (int i = 0; i < node.size(); i++) {
            final JsonNode tryNode = getNodeByPath(node.get(i), path);
            if (tryNode == null || tryNode.isNull()) {
                if (nullPriority > 0) {
                    return node.get(i);
                }
                if (nullPriority < 0 && foundIndex < 0) {
                    foundIndex = i;
                }
                continue;
            }
            if (tryNode.isContainerNode()) {
                continue;
            }
            if (maxMinDouble != null || tryNode.isNumber()) {
                if (tryNode.isNumber()) {
                    final double tryValue = tryNode.asDouble();
                    if (maxMinDouble == null || (isMax ? tryValue > maxMinDouble : tryValue < maxMinDouble)) {
                        maxMinDouble = tryValue;
                        foundIndex = i;
                    }
                }
            } else {
                final String tryValue = tryNode.asText();
                if (maxMinString == null
                        || (isMax ? tryValue.compareTo(maxMinString) > 0 : tryValue.compareTo(maxMinString) < 0)) {
                    maxMinString = tryValue;
                    foundIndex = i;
                }
            }
        }
        return foundIndex >= 0 ? node.get(foundIndex) : null;
    }

    static JsonNode funcFirst(final JsonNode node, final String params) {
        return applyWithoutParam(node, params,
                jsonNode -> !jsonNode.isArray() ? jsonNode : jsonNode.size() == 0 ? null : jsonNode.get(0));
    }

    static IntNode funcIndexOf(JsonNode node, final String params, boolean isForward) {
        final Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        final JsonNode valueNode = getNodeByPath(node, pathAndParams.getValue().get(0));
        if (valueNode == null || !valueNode.isValueNode()) {
            return null;
        }
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        if (!node.isArray()) {
            return null;
        }
        if (isForward) {
            if (valueNode.isNumber()) {
                final double value = valueNode.asDouble();
                for (int i = 0; i < node.size(); i++) {
                    final JsonNode tryNode = node.get(i);
                    if ((tryNode.isNumber() || tryNode.isTextual()) && tryNode.asDouble() == value) {
                        return IntNode.valueOf(i);
                    }
                }
            } else if (valueNode.isNull()) {
                for (int i = 0; i < node.size(); i++) {
                    if (node.get(i).isNull()) {
                        return IntNode.valueOf(i);
                    }
                }
            } else {
                final String value = valueNode.asText();
                for (int i = 0; i < node.size(); i++) {
                    final JsonNode tryNode = node.get(i);
                    if ((tryNode.isNumber() || tryNode.isTextual()) && tryNode.asText().equals(value)) {
                        return IntNode.valueOf(i);
                    }
                }
            }
        } else {
            if (valueNode.isNumber()) {
                final double value = valueNode.asDouble();
                for (int i = node.size() - 1; i >= 0; i--) {
                    final JsonNode tryNode = node.get(i);
                    if ((tryNode.isNumber() || tryNode.isTextual()) && tryNode.asDouble() == value) {
                        return IntNode.valueOf(i);
                    }
                }
            } else if (valueNode.isNull()) {
                for (int i = node.size() - 1; i >= 0; i--) {
                    if (node.get(i).isNull()) {
                        return IntNode.valueOf(i);
                    }
                }
            } else {
                final String value = valueNode.asText();
                for (int i = node.size() - 1; i >= 0; i--) {
                    final JsonNode tryNode = node.get(i);
                    if ((tryNode.isNumber() || tryNode.isTextual()) && tryNode.asText().equals(value)) {
                        return IntNode.valueOf(i);
                    }
                }
            }
        }
        return null;
    }

    static TextNode funcJoin(JsonNode node, final String params) {
        final Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 0, 1);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        final String delimiter = pathAndParams.getValue().size() > 0
                ? getNodeAsText(node, pathAndParams.getValue().get(0)) : "";
        final List<String> texts = new ArrayList<>();
        for (int i = 0; i < node.size(); i++) {
            final JsonNode valueNode = node.get(i);
            if (nodeHasValue(valueNode)) {
                texts.add(valueNode.asText());
            }
        }
        return TextNode.valueOf(String.join(delimiter, texts));
    }

    static JsonNode funcLast(final JsonNode node, final String params) {
        return applyWithoutParam(node, params,
                jsonNode -> !jsonNode.isArray()
                        ? jsonNode : jsonNode.size() == 0 ? null
                        : jsonNode.get(jsonNode.size() - 1)
        );
    }

    static JsonNode funcLastIndex(final JsonNode node, final String params) {
        return applyWithoutParam(node, params,
                jsonNode -> !jsonNode.isArray() ? null : IntNode.valueOf(jsonNode.size() - 1));
    }

    static ValueNode funcMaxMin(final JsonNode node, final String params, final boolean isMax) {
        final ArrayNode array = getParamArrayOrItself(params, node);
        if (array == null) {
            return null;
        }
        double maxMinDouble = 0;
        ValueNode maxMinNumber = null;
        String maxMinString = null;
        for (int i = array.size() - 1; i >= 0; i--) {
            final JsonNode tryNode = array.get(i);
            if (!nodeHasValue(tryNode)) {
                continue;
            }
            if (maxMinNumber != null || tryNode.isNumber()) {
                if (tryNode.isNumber()) {
                    final double tryValue = tryNode.asDouble();
                    if (maxMinNumber == null || (isMax ? tryValue > maxMinDouble : tryValue < maxMinDouble)) {
                        maxMinNumber = (ValueNode) tryNode;
                        maxMinDouble = tryValue;
                    }
                }
            } else {
                final String tryValue = tryNode.asText();
                if (maxMinString == null
                        || (isMax ? tryValue.compareTo(maxMinString) > 0 : tryValue.compareTo(maxMinString) < 0)) {
                    maxMinString = tryValue;
                }
            }
        }
        return maxMinNumber != null ? maxMinNumber : maxMinString != null ? TextNode.valueOf(maxMinString) : null;
    }

    static JsonNode funcReverse(final JsonNode node, final String params) {
        return applyWithoutParam(node, params,
                jsonNode -> {
                    if (jsonNode.isTextual()) {
                        final StringBuilder sb = new StringBuilder(jsonNode.asText());
                        return TextNode.valueOf(sb.reverse().toString());
                    }
                    if (!jsonNode.isArray()) {
                        return null;
                    }
                    final ArrayNode array = MAPPER.createArrayNode();
                    final int len = jsonNode.size();
                    for (int i = len - 1; i >= 0; i--) {
                        array.add(jsonNode.get(i));
                    }
                    return array;
                }
        );
    }

    static JsonNode funcSize(final JsonNode node, final String params) {
        return applyWithoutParam(node, params, jsonNode -> IntNode.valueOf(jsonNode.size()));
    }

    static JsonNode funcSlice(JsonNode node, final String params) {
        final Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 0, 3);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        if (!node.isArray()) {
            return node;
        }
        final int size = node.size();
        int start = pathAndParams.getValue().size() > 0 && !pathAndParams.getValue().get(0).isEmpty()
                ? getNodeAsInt(node, pathAndParams.getValue().get(0)) : 0;
        int end = pathAndParams.getValue().size() > 1 && !pathAndParams.getValue().get(1).isEmpty()
                ? getNodeAsInt(node, pathAndParams.getValue().get(1)) : Integer.MAX_VALUE;
        int step = pathAndParams.getValue().size() > 2 ? getNodeAsInt(node, pathAndParams.getValue().get(2)) : 1;
        start = start >= 0 ? start : size + start;
        start = start < 0 ? 0 : Math.min(start, size);
        end = end >= 0 ? end : size + end;
        end = end < 0 ? 0 : Math.min(end, size);
        step = step == 0 ? 1 : Math.abs(step);
        final ArrayNode array = MAPPER.createArrayNode();
        if (start <= end) {
            for (int i = start; i < end; i += step) {
                array.add(node.get(i));
            }
        } else {
            for (int i = start; i > end; i -= step) {
                array.add(node.get(i));
            }
        }
        return array;
    }

    static JsonNode funcSort(JsonNode node, final String params) {
        final Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 0, 2);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        if (!node.isArray()) {
            return node;
        }
        String param = null;
        int ordering = 1;
        if (pathAndParams.getValue().size() > 0) {
            param = pathAndParams.getValue().get(0);
            try {
                ordering = Integer.parseInt(param);
                param = null;
                if (pathAndParams.getValue().size() > 1) {
                    throw new IllegalArgumentException("Too many function arguments: " + params);
                }
            } catch (NumberFormatException e) {
                if (pathAndParams.getValue().size() > 1) {
                    ordering = Integer.parseInt(pathAndParams.getValue().get(1));
                }
            }
        }
        final String path = param;
        final boolean asc = ordering >= 0;
        final List<JsonNode> nodeList = new ArrayList<>();
        for (int i = 0; i < node.size(); i++) {
            nodeList.add(node.get(i));
        }
        nodeList.sort((o1, o2) -> jsonNodeComparator(o1, o2, path, asc));
        final ArrayNode array = MAPPER.createArrayNode();
        array.addAll(nodeList);
        return array;
    }

    private static int jsonNodeComparator(JsonNode o1, JsonNode o2, final String path, final boolean asc) {
        int compare = 0;
        if (!StringUtils.isEmpty(path)) {
            if (o1.isObject()) {
                o1 = getNodeByPath(o1, path);
                if (o1 == null) {
                    return 1;
                }
            }
            if (o2.isObject()) {
                o2 = getNodeByPath(o2, path);
                if (o2 == null) {
                    return -1;
                }
            }
        }
        if (o1.isNumber() && o2.isNumber()) {
            final double value = o1.asDouble() - o2.asDouble();
            compare = (value > 0) ? 1 : (value < 0) ? -1 : 0;
        } else if (o1.isTextual() && o2.isTextual()) {
            compare = o1.asText().compareTo(o2.asText());
        } else if (o1.isBoolean() && o2.isBoolean()) {
            if (o1.asBoolean() != o2.asBoolean()) {
                compare = o1.asBoolean() ? -1 : 1;
            }
        } else if (!o1.isNull() || !o2.isNull()) {
            if (o1.isNumber()) {
                compare = -1;
            } else if (o1.isTextual()) {
                compare = o2.isNumber() ? 1 : -1;
            } else if (o1.isBoolean()) {
                compare = o2.isNumber() || o2.isTextual() ? 1 : -1;
            } else {
                compare = o2.isValueNode() ? 1 : -1;
            }
        }
        return asc ? compare : -compare;
    }
}

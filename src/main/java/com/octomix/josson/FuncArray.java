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
import static com.octomix.josson.commons.StringUtils.EMPTY;

/**
 * Array functions.
 */
final class FuncArray {

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

    static JsonNode funcFindByMaxMin(final JsonNode node, final String params, final boolean isMax, final int nullPriority) {
        final Pair<JsonNode, List<String>> nodeAndParams = getParamNodeAndStrings(node, params, 1, 1);
        final JsonNode workNode = nodeAndParams.getKey();
        if (workNode == null || !workNode.isArray()) {
            return workNode;
        }
        final String path = nodeAndParams.getValue().get(0);
        int foundIndex = -1;
        Double maxMinDouble = null;
        String maxMinString = null;
        for (int i = 0; i < workNode.size(); i++) {
            final JsonNode tryNode = getNodeByPath(workNode.get(i), path);
            if (tryNode == null || tryNode.isNull()) {
                if (nullPriority > 0) {
                    return workNode.get(i);
                }
                if (nullPriority < 0 && foundIndex < 0) {
                    foundIndex = i;
                }
                continue;
            }
            if (tryNode.isContainerNode()) {
                continue;
            }
            if (tryNode.isNumber()) {
                final double tryValue = tryNode.asDouble();
                if (maxMinDouble == null || (isMax ? tryValue > maxMinDouble : tryValue < maxMinDouble)) {
                    maxMinDouble = tryValue;
                    foundIndex = i;
                }
            } else if (maxMinDouble == null) {
                final String tryValue = tryNode.asText();
                if (maxMinString == null
                        || (isMax ? tryValue.compareTo(maxMinString) > 0 : tryValue.compareTo(maxMinString) < 0)) {
                    maxMinString = tryValue;
                    foundIndex = i;
                }
            }
        }
        return foundIndex >= 0 ? workNode.get(foundIndex) : null;
    }

    static JsonNode funcFirst(final JsonNode node, final String params) {
        return applyWithoutParam(node, params,
                jsonNode -> !jsonNode.isArray() ? jsonNode
                        : jsonNode.size() == 0 ? null
                        : jsonNode.get(0));
    }

    static IntNode funcIndexOf(final JsonNode node, final String params, final int step) {
        final Pair<JsonNode, List<String>> nodeAndParams = getParamNodeAndStrings(node, params, 1, 1);
        final JsonNode workNode = nodeAndParams.getKey();
        if (workNode == null || !workNode.isArray()) {
            return null;
        }
        final JsonNode valueNode = getNodeByPath(node, nodeAndParams.getValue().get(0));
        int i = step > 0 ? 0 : workNode.size() - 1;
        final int end = step > 0 ? workNode.size() : -1;
        for (; i != end; i += step) {
            if (Operator.EQ.relationalCompare(valueNode, workNode.get(i))) {
                return IntNode.valueOf(i);
            }
        }
        return null;
    }

    static TextNode funcJoin(final JsonNode node, final String params) {
        final Pair<JsonNode, List<String>> nodeAndParams = getParamNodeAndStrings(node, params, 0, 1);
        final JsonNode workNode = nodeAndParams.getKey();
        if (workNode == null) {
            return null;
        }
        final String delimiter = nodeAndParams.getValue().size() > 0
                ? getNodeAsText(node, NON_ARRAY_INDEX, nodeAndParams.getValue().get(0)) : EMPTY;
        final List<String> texts = new ArrayList<>();
        for (int i = 0; i < workNode.size(); i++) {
            final JsonNode valueNode = workNode.get(i);
            if (nodeHasValue(valueNode)) {
                texts.add(valueNode.asText());
            }
        }
        return TextNode.valueOf(String.join(delimiter, texts));
    }

    static JsonNode funcLast(final JsonNode node, final String params) {
        return applyWithoutParam(node, params,
                jsonNode -> !jsonNode.isArray() ? jsonNode
                        : jsonNode.size() == 0 ? null
                        : jsonNode.get(jsonNode.size() - 1)
        );
    }

    static JsonNode funcLastIndex(final JsonNode node, final String params) {
        return applyWithoutParam(node, params,
                jsonNode -> !jsonNode.isArray() ? null : IntNode.valueOf(jsonNode.size() - 1));
    }

    static JsonNode funcMaxMin(final JsonNode node, final String params, final boolean isMax) {
        final ArrayNode array = getParamArrayOrItself(params, node);
        if (array == null) {
            return null;
        }
        double maxMinDouble = 0;
        String maxMinString = null;
        JsonNode result = null;
        for (int i = array.size() - 1; i >= 0; i--) {
            final JsonNode tryNode = array.get(i);
            if (!nodeHasValue(tryNode)) {
                continue;
            }
            if (tryNode.isNumber()) {
                final double tryValue = tryNode.asDouble();
                if (result == null || !result.isNumber()
                        || (isMax ? tryValue > maxMinDouble : tryValue < maxMinDouble)) {
                    result = tryNode;
                    maxMinDouble = tryValue;
                }
            } else if (result == null || !result.isNumber()) {
                final String tryValue = tryNode.asText();
                if (maxMinString == null
                        || (isMax ? tryValue.compareTo(maxMinString) > 0 : tryValue.compareTo(maxMinString) < 0)) {
                    result = tryNode;
                    maxMinString = tryValue;
                }
            }
        }
        return result;
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
                });
    }

    static JsonNode funcSize(final JsonNode node, final String params) {
        return applyWithoutParam(node, params, jsonNode -> IntNode.valueOf(jsonNode.size()));
    }

    static JsonNode funcSlice(final JsonNode node, final String params) {
        final Pair<JsonNode, List<String>> nodeAndParams = getParamNodeAndStrings(node, params, 0, 3);
        final JsonNode workNode = nodeAndParams.getKey();
        if (workNode == null || !workNode.isArray()) {
            return workNode;
        }
        final int size = workNode.size();
        int start = nodeAndParams.getValue().size() > 0 && !nodeAndParams.getValue().get(0).isEmpty()
                ? getNodeAsInt(node, nodeAndParams.getValue().get(0)) : 0;
        int end = nodeAndParams.getValue().size() > 1 && !nodeAndParams.getValue().get(1).isEmpty()
                ? getNodeAsInt(node, nodeAndParams.getValue().get(1)) : Integer.MAX_VALUE;
        int step = nodeAndParams.getValue().size() > 2 ? getNodeAsInt(node, nodeAndParams.getValue().get(2)) : 1;
        start = start >= 0 ? start : size + start;
        start = start < 0 ? 0 : Math.min(start, size);
        end = end >= 0 ? end : size + end;
        end = end < 0 ? 0 : Math.min(end, size);
        step = step == 0 ? 1 : Math.abs(step);
        final ArrayNode array = MAPPER.createArrayNode();
        if (start <= end) {
            for (int i = start; i < end; i += step) {
                array.add(workNode.get(i));
            }
        } else {
            for (int i = start; i > end; i -= step) {
                array.add(workNode.get(i));
            }
        }
        return array;
    }

    static JsonNode funcSort(final JsonNode node, final String params) {
        final Pair<JsonNode, List<String>> nodeAndParams = getParamNodeAndStrings(node, params, 0, 2);
        final JsonNode workNode = nodeAndParams.getKey();
        if (workNode == null || !workNode.isArray()) {
            return workNode;
        }
        String param = null;
        int ordering = 1;
        if (nodeAndParams.getValue().size() > 0) {
            param = nodeAndParams.getValue().get(0);
            try {
                ordering = Integer.parseInt(param);
                param = null;
                if (nodeAndParams.getValue().size() > 1) {
                    throw new IllegalArgumentException("Too many function arguments: " + params);
                }
            } catch (NumberFormatException e) {
                if (nodeAndParams.getValue().size() > 1) {
                    ordering = getNodeAsInt(node, nodeAndParams.getValue().get(1));
                }
            }
        }
        final String path = param;
        final boolean asc = ordering >= 0;
        final List<JsonNode> nodeList = new ArrayList<>();
        for (int i = 0; i < workNode.size(); i++) {
            nodeList.add(workNode.get(i));
        }
        nodeList.sort((o1, o2) -> jsonNodeComparator(o1, o2, path, asc));
        final ArrayNode array = MAPPER.createArrayNode();
        array.addAll(nodeList);
        return array;
    }

    private static int jsonNodeComparator(JsonNode o1, JsonNode o2, final String path, final boolean asc) {
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
        int compare = 0;
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

    static ArrayNode funcTopBottomN(final JsonNode node, final String params, final boolean isTop) {
        final Pair<JsonNode, List<String>> nodeAndParams = getParamNodeAndStrings(node, params, 1, 1);
        final JsonNode workNode = nodeAndParams.getKey();
        if (workNode == null || !workNode.isArray()) {
            return null;
        }
        final int n = getNodeAsInt(node, nodeAndParams.getValue().get(0));
        List<Pair<Double, ValueNode>> maxMinDouble = new ArrayList<>();
        List<String> maxMinString = new ArrayList<>();
        for (int i = workNode.size() - 1; i >= 0; i--) {
            final JsonNode tryNode = workNode.get(i);
            if (!nodeHasValue(tryNode)) {
                continue;
            }
            if (tryNode.isNumber()) {
                final double tryValue = tryNode.asDouble();
                int j = maxMinDouble.size() - 1;
                for (; j >= 0; j--) {
                    if (isTop ? tryValue <= maxMinDouble.get(j).getKey() : tryValue >= maxMinDouble.get(j).getKey()) {
                        break;
                    }
                }
                if (++j < n) {
                    maxMinDouble.add(j, Pair.of(tryValue, (ValueNode) tryNode));
                    if (maxMinDouble.size() > n) {
                        maxMinDouble.remove(n);
                    }
                }
            } else if (maxMinDouble.isEmpty()) {
                final String tryValue = tryNode.asText();
                int j = maxMinString.size() - 1;
                for (; j >= 0; j--) {
                    if (isTop ? tryValue.compareTo(maxMinString.get(j)) <= 0 : tryValue.compareTo(maxMinString.get(j)) >= 0) {
                        break;
                    }
                }
                if (++j < n) {
                    maxMinString.add(j, tryValue);
                    if (maxMinString.size() > n) {
                        maxMinString.remove(n);
                    }
                }
            }
        }
        ArrayNode array = MAPPER.createArrayNode();
        if (maxMinString.isEmpty()) {
            maxMinDouble.forEach(pair -> array.add(pair.getValue()));
        } else {
            maxMinString.forEach(array::add);
        }
        return array;
    }
}

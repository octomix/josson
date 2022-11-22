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
import static com.octomix.josson.Utils.nodeHasValue;
import static com.octomix.josson.Utils.nodeIsNull;
import static com.octomix.josson.commons.StringUtils.EMPTY;

/**
 * Array functions.
 */
final class FuncArray {

    private FuncArray() {
    }

    static PathTrace funcNumericAggregate(final PathTrace path, final String funcId, final String params) {
        final PathTrace array = getParamArrayOrItself(path, params);
        if (array == null) {
            return null;
        }
        double sum = 0;
        int count = 0;
        for (JsonNode elem : array.node()) {
            if (nodeHasValue(elem)) {
                sum += elem.asDouble();
                count++;
            }
        }
        if ("count".equals(funcId)) {
            return path.push(IntNode.valueOf(count));
        }
        if (count > 0) {
            switch (funcId) {
                case "sum":
                    return path.push(DoubleNode.valueOf(sum));
                case "avg":
                    return path.push(DoubleNode.valueOf(sum / count));
            }
        }
        return null;
    }

    static PathTrace funcDistinct(final PathTrace path, final String params) {
        final PathTrace array = getParamArrayOrItself(path, params);
        if (array == null) {
            return null;
        }
        final Set<String> texts = new HashSet<>();
        final Set<Double> doubles = new HashSet<>();
        final Set<Boolean> booleans = new HashSet<>();
        array.node().forEach(elem -> {
            if (elem.isTextual()) {
                texts.add(elem.asText());
            } else if (elem.isNumber()) {
                doubles.add(elem.asDouble());
            } else if (elem.isBoolean()) {
                booleans.add(elem.asBoolean());
            }
        });
        final ArrayNode result = MAPPER.createArrayNode();
        texts.forEach(value -> result.add(TextNode.valueOf(value)));
        doubles.forEach(value -> result.add(DoubleNode.valueOf(value)));
        booleans.forEach(value -> result.add(BooleanNode.valueOf(value)));
        return path.push(result);
    }

    static PathTrace funcFindByMaxMin(final PathTrace path, final String params, final boolean isMax, final int nullPriority) {
        final Pair<PathTrace, List<String>> pathAndParams = getParamPathAndStrings(path, params, 1, 1);
        final PathTrace dataPath = pathAndParams.getKey();
        if (dataPath == null || !dataPath.node().isArray()) {
            return dataPath;
        }
        final String expression = pathAndParams.getValue().get(0);
        int foundIndex = -1;
        Double maxMinNum = null;
        String maxMinStr = null;
        for (int i = 0; i < dataPath.node().size(); i++) {
            final PathTrace result = getPathByExpression(dataPath.push(dataPath.node().get(i)), expression);
            if (nodeIsNull(result)) {
                if (nullPriority > 0) {
                    return path.push(dataPath.node().get(i));
                }
                if (nullPriority < 0 && foundIndex < 0) {
                    foundIndex = i;
                }
                continue;
            }
            if (result.node().isContainerNode()) {
                continue;
            }
            if (result.node().isNumber()) {
                final double value = result.node().asDouble();
                if (maxMinNum == null || (isMax ? value > maxMinNum : value < maxMinNum)) {
                    maxMinNum = value;
                    foundIndex = i;
                }
            } else if (maxMinNum == null) {
                final String value = result.node().asText();
                if (maxMinStr == null || (isMax ? value.compareTo(maxMinStr) > 0 : value.compareTo(maxMinStr) < 0)) {
                    maxMinStr = value;
                    foundIndex = i;
                }
            }
        }
        return foundIndex >= 0 ? path.push(dataPath.node().get(foundIndex)) : null;
    }

    static PathTrace funcFirst(final PathTrace path, final String params) {
        return applyWithoutParam(path, params,
            dataPath -> !dataPath.node().isArray() ? dataPath
                : dataPath.node().size() == 0 ? null
                : dataPath.push(dataPath.node().get(0)));
    }

    static PathTrace funcIndexOf(final PathTrace path, final String params, final int step) {
        final Pair<PathTrace, List<String>> pathAndParams = getParamPathAndStrings(path, params, 1, 1);
        final PathTrace dataPath = pathAndParams.getKey();
        if (dataPath == null || !dataPath.node().isArray()) {
            return null;
        }
        final PathTrace result = getPathByExpression(path, pathAndParams.getValue().get(0));
        int i = step > 0 ? 0 : dataPath.node().size() - 1;
        final int end = step > 0 ? dataPath.node().size() : -1;
        for (; i != end; i += step) {
            if (Operator.EQ.relationalCompare(result.node(), dataPath.node().get(i))) {
                return path.push(IntNode.valueOf(i));
            }
        }
        return null;
    }

    static PathTrace funcJoin(final PathTrace path, final String params) {
        final Pair<PathTrace, List<String>> pathAndParams = getParamPathAndStrings(path, params, 0, 1);
        final PathTrace dataPath = pathAndParams.getKey();
        if (dataPath == null) {
            return null;
        }
        final String delimiter = pathAndParams.getValue().size() > 0
                ? getNodeAsText(path, pathAndParams.getValue().get(0)) : EMPTY;
        final List<String> texts = new ArrayList<>();
        dataPath.node().forEach(elem -> {
            if (nodeHasValue(elem)) {
                texts.add(elem.asText());
            }
        });
        return path.push(TextNode.valueOf(String.join(delimiter, texts)));
    }

    static PathTrace funcLast(final PathTrace path, final String params) {
        return applyWithoutParam(path, params,
            dataPath -> !dataPath.node().isArray() ? dataPath
                : dataPath.node().size() == 0 ? null
                : path.push(dataPath.node().get(dataPath.node().size() - 1))
        );
    }

    static PathTrace funcLastIndex(final PathTrace path, final String params) {
        return applyWithoutParam(path, params,
            dataPath -> !dataPath.node().isArray() ? null : path.push(IntNode.valueOf(dataPath.node().size() - 1)));
    }

    static PathTrace funcMaxMin(final PathTrace path, final String params, final boolean isMax) {
        final PathTrace array = getParamArrayOrItself(path, params);
        if (array == null) {
            return null;
        }
        double maxMinNum = 0;
        String maxMinStr = null;
        JsonNode result = null;
        for (int i = array.node().size() - 1; i >= 0; i--) {
            final JsonNode elem = array.node().get(i);
            if (!nodeHasValue(elem)) {
                continue;
            }
            if (elem.isNumber()) {
                final double value = elem.asDouble();
                if (result == null || !result.isNumber() || (isMax ? value > maxMinNum : value < maxMinNum)) {
                    result = elem;
                    maxMinNum = value;
                }
            } else if (result == null || !result.isNumber()) {
                final String value = elem.asText();
                if (maxMinStr == null || (isMax ? value.compareTo(maxMinStr) > 0 : value.compareTo(maxMinStr) < 0)) {
                    result = elem;
                    maxMinStr = value;
                }
            }
        }
        return path.push(result);
    }

    static PathTrace funcReverse(final PathTrace path, final String params) {
        return applyWithoutParam(path, params,
            dataPath -> {
                if (dataPath.node().isTextual()) {
                    final StringBuilder sb = new StringBuilder(dataPath.node().asText());
                    return path.push(TextNode.valueOf(sb.reverse().toString()));
                }
                if (!dataPath.node().isArray()) {
                    return null;
                }
                final ArrayNode array = MAPPER.createArrayNode();
                final int len = dataPath.node().size();
                for (int i = len - 1; i >= 0; i--) {
                    array.add(dataPath.node().get(i));
                }
                return path.push(array);
            });
    }

    static PathTrace funcSize(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, dataPath -> dataPath.push(IntNode.valueOf(dataPath.node().size())));
    }

    static PathTrace funcSlice(final PathTrace path, final String params) {
        final Pair<PathTrace, List<String>> pathAndParams = getParamPathAndStrings(path, params, 0, 3);
        final PathTrace dataPath = pathAndParams.getKey();
        if (dataPath == null || !dataPath.node().isArray()) {
            return dataPath;
        }
        final int size = dataPath.node().size();
        int start = pathAndParams.getValue().size() > 0 && !pathAndParams.getValue().get(0).isEmpty()
                ? getNodeAsInt(path, pathAndParams.getValue().get(0)) : 0;
        int end = pathAndParams.getValue().size() > 1 && !pathAndParams.getValue().get(1).isEmpty()
                ? getNodeAsInt(path, pathAndParams.getValue().get(1)) : Integer.MAX_VALUE;
        int step = pathAndParams.getValue().size() > 2 ? getNodeAsInt(path, pathAndParams.getValue().get(2)) : 1;
        start = start >= 0 ? start : size + start;
        start = start < 0 ? 0 : Math.min(start, size);
        end = end >= 0 ? end : size + end;
        end = end < 0 ? 0 : Math.min(end, size);
        step = step == 0 ? 1 : Math.abs(step);
        final ArrayNode array = MAPPER.createArrayNode();
        if (start <= end) {
            for (int i = start; i < end; i += step) {
                array.add(dataPath.node().get(i));
            }
        } else {
            for (int i = start; i > end; i -= step) {
                array.add(dataPath.node().get(i));
            }
        }
        return path.push(array);
    }

    static PathTrace funcSort(final PathTrace path, final String params) {
        final Pair<PathTrace, List<String>> pathAndParams = getParamPathAndStrings(path, params, 0, 2);
        final PathTrace dataPath = pathAndParams.getKey();
        if (dataPath == null || !dataPath.node().isArray()) {
            return dataPath;
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
                    ordering = getNodeAsInt(path, pathAndParams.getValue().get(1));
                }
            }
        }
        final String paramPath = param;
        final boolean asc = ordering >= 0;
        final List<JsonNode> nodeList = new ArrayList<>();
        dataPath.node().forEach(nodeList::add);
        nodeList.sort((o1, o2) -> jsonNodeComparator(o1, o2, paramPath, asc));
        return path.push(MAPPER.createArrayNode().addAll(nodeList));
    }

    private static int jsonNodeComparator(JsonNode o1, JsonNode o2, final String path, final boolean asc) {
        if (!StringUtils.isEmpty(path)) {
            if (o1.isObject()) {
                o1 = getNodeByExpression(PathTrace.from(o1), path);
                if (o1 == null) {
                    return 1;
                }
            }
            if (o2.isObject()) {
                o2 = getNodeByExpression(PathTrace.from(o2), path);
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

    static PathTrace funcTopBottomN(final PathTrace path, final String params, final boolean isTop) {
        final Pair<PathTrace, List<String>> pathAndParams = getParamPathAndStrings(path, params, 1, 1);
        final PathTrace dataPath = pathAndParams.getKey();
        if (dataPath == null || !dataPath.node().isArray()) {
            return null;
        }
        final int n = getNodeAsInt(path, pathAndParams.getValue().get(0));
        final List<Pair<Double, ValueNode>> maxMinNum = new ArrayList<>();
        final List<String> maxMinStr = new ArrayList<>();
        for (int i = dataPath.node().size() - 1; i >= 0; i--) {
            final JsonNode elem = dataPath.node().get(i);
            if (!nodeHasValue(elem)) {
                continue;
            }
            if (elem.isNumber()) {
                final double value = elem.asDouble();
                int j = maxMinNum.size() - 1;
                for (; j >= 0; j--) {
                    if (isTop ? value <= maxMinNum.get(j).getKey() : value >= maxMinNum.get(j).getKey()) {
                        break;
                    }
                }
                if (++j < n) {
                    maxMinNum.add(j, Pair.of(value, (ValueNode) elem));
                    if (maxMinNum.size() > n) {
                        maxMinNum.remove(n);
                    }
                }
            } else if (maxMinNum.isEmpty()) {
                final String value = elem.asText();
                int j = maxMinStr.size() - 1;
                for (; j >= 0; j--) {
                    if (isTop ? value.compareTo(maxMinStr.get(j)) <= 0 : value.compareTo(maxMinStr.get(j)) >= 0) {
                        break;
                    }
                }
                if (++j < n) {
                    maxMinStr.add(j, value);
                    if (maxMinStr.size() > n) {
                        maxMinStr.remove(n);
                    }
                }
            }
        }
        final ArrayNode array = MAPPER.createArrayNode();
        if (maxMinStr.isEmpty()) {
            maxMinNum.forEach(pair -> array.add(pair.getValue()));
        } else {
            maxMinStr.forEach(array::add);
        }
        return path.push(array);
    }
}

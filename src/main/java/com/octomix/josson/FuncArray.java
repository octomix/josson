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
import java.util.List;

import static com.octomix.josson.GetFuncParam.*;
import static com.octomix.josson.JossonCore.getNodeByPath;
import static com.octomix.josson.Mapper.MAPPER;

class FuncArray {
    static JsonNode funcFirst(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNodeByPath(node, path);
            if (node == null) {
                return null;
            }
        }
        if (!node.isArray()) {
            return node;
        }
        if (node.size() == 0) {
            return null;
        }
        return node.get(0);
    }

    static IntNode funcIndexOf(JsonNode node, String params) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        if (!node.isArray()) {
            return null;
        }
        JsonNode valueNode = getNodeByPath(node, pathAndParams.getValue().get(0));
        if (valueNode != null && valueNode.isValueNode()) {
            if (valueNode.isNumber()) {
                double value = valueNode.asDouble();
                for (int i = 0; i < node.size(); i++) {
                    JsonNode tryNode = node.get(i);
                    if (tryNode.isNumber() || tryNode.isTextual()) {
                        if (tryNode.asDouble() == value) {
                            return IntNode.valueOf(i);
                        }
                    }
                }
            } else {
                String value = valueNode.asText();
                for (int i = 0; i < node.size(); i++) {
                    JsonNode tryNode = node.get(i);
                    if (tryNode.isNumber() || tryNode.isTextual()) {
                        if (tryNode.asText().equals(value)) {
                            return IntNode.valueOf(i);
                        }
                    }
                }
            }
        }
        return null;
    }

    static JsonNode funcLast(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNodeByPath(node, path);
            if (node == null) {
                return null;
            }
        }
        if (!node.isArray()) {
            return node;
        }
        if (node.size() == 0) {
            return null;
        }
        return node.get(node.size() - 1);
    }

    static IntNode funcLastIndex(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNodeByPath(node, path);
            if (node == null) {
                return null;
            }
        }
        if (!node.isArray()) {
            return null;
        }
        return IntNode.valueOf(node.size() - 1);
    }

    static IntNode funcLastIndexOf(JsonNode node, String params) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        if (!node.isArray()) {
            return null;
        }
        JsonNode valueNode = getNodeByPath(node, pathAndParams.getValue().get(0));
        if (valueNode != null && valueNode.isValueNode()) {
            if (valueNode.isNumber()) {
                double value = valueNode.asDouble();
                for (int i = node.size() - 1; i >= 0; i--) {
                    JsonNode tryNode = node.get(i);
                    if (tryNode.isNumber() || tryNode.isTextual()) {
                        if (tryNode.asDouble() == value) {
                            return IntNode.valueOf(i);
                        }
                    }
                }
            } else {
                String value = valueNode.asText();
                for (int i = node.size() - 1; i >= 0; i--) {
                    JsonNode tryNode = node.get(i);
                    if (tryNode.isNumber() || tryNode.isTextual()) {
                        if (tryNode.asText().equals(value)) {
                            return IntNode.valueOf(i);
                        }
                    }
                }
            }
        }
        return null;
    }

    static JsonNode funcFindByMaxMin(JsonNode node, String params, boolean isMax, int nullPriority) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        if (!node.isArray()) {
            return node;
        }
        String path = pathAndParams.getValue().get(0);
        int foundIndex = -1;
        Double maxMinDouble = null;
        String maxMinString = null;
        for (int i = 0; i < node.size(); i++) {
            JsonNode tryNode = getNodeByPath(node.get(i), path);
            if (tryNode == null || tryNode.isNull()) {
                if (nullPriority > 0) {
                    return tryNode;
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
                    double tryValue = tryNode.asDouble();
                    if (maxMinDouble == null
                            || (isMax && tryValue > maxMinDouble)
                            || (!isMax && tryValue < maxMinDouble)) {
                        maxMinDouble = tryValue;
                        foundIndex = i;
                    }
                }
            } else {
                String tryValue = tryNode.asText();
                if (maxMinString == null
                        || (isMax && tryValue.compareTo(maxMinString) > 0)
                        || (!isMax && tryValue.compareTo(maxMinString) < 0)) {
                    maxMinString = tryValue;
                    foundIndex = i;
                }
            }
        }
        return foundIndex >= 0 ? node.get(foundIndex) : null;
    }

    static JsonNode funcReverse(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNodeByPath(node, path);
            if (node == null) {
                return null;
            }
        }
        if (node.isTextual()) {
            StringBuilder sb = new StringBuilder(node.asText());
            return TextNode.valueOf(sb.reverse().toString());
        }
        if (!node.isArray()) {
            return null;
        }
        ArrayNode array = MAPPER.createArrayNode();
        int len = node.size();
        for (int i = len - 1; i >= 0; i--) {
            array.add(node.get(i));
        }
        return array;
    }

    static JsonNode funcSlice(JsonNode node, String params) {
        Pair<String, Integer[]> pathAndParams = getParamPathAndStartEndStep(params);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        if (!node.isArray()) {
            return node;
        }
        int size = node.size();
        int start = pathAndParams.getValue()[0] >= 0 ? pathAndParams.getValue()[0] : size + pathAndParams.getValue()[0];
        start = start < 0 ? 0 : Math.min(start, size);
        int end = pathAndParams.getValue()[1] >= 0 ? pathAndParams.getValue()[1] : size + pathAndParams.getValue()[1];
        end = end < 0 ? 0 : Math.min(end, size);
        int step = pathAndParams.getValue()[2] == 0 ? 1 : Math.abs(pathAndParams.getValue()[2]);
        ArrayNode array = MAPPER.createArrayNode();
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

    static JsonNode funcSort(JsonNode node, String params) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 0, 2);
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
        double ordering = 1;
        if (pathAndParams.getValue().size() > 0) {
            param = pathAndParams.getValue().get(0);
            try {
                ordering = Double.parseDouble(param);
                param = null;
                if (pathAndParams.getValue().size() > 1) {
                    throw new IllegalArgumentException("Too many function arguments: " + params);
                }
            } catch (NumberFormatException e) {
                if (pathAndParams.getValue().size() > 1) {
                    ordering = Double.parseDouble(pathAndParams.getValue().get(1));
                }
            }
        }
        String path = param;
        boolean asc = ordering >= 0;
        List<JsonNode> nodeList = new ArrayList<>();
        for (int i = 0; i < node.size(); i++) {
            nodeList.add(node.get(i));
        }
        nodeList.sort((o1, o2) -> {
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
                double value = o1.asDouble() - o2.asDouble();
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
        });
        ArrayNode array = MAPPER.createArrayNode();
        for (int i  = 0; i < node.size(); i++) {
            array.add(nodeList.get(i));
        }
        return array;
    }
}

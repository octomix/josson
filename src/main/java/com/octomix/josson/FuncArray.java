package com.octomix.josson;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutableTriple;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static com.octomix.josson.GetFuncParam.*;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Josson.getNode;
import static com.octomix.josson.PatternMatcher.decomposeFunctionParameters;

class FuncArray {
    static JsonNode funcDistinct(JsonNode node, String params) {
        getParamNotAccept(params);
        if (!node.isArray()) {
            return node;
        }
        Set<String> texts = new HashSet<>();
        Set<Double> doubles = new HashSet<>();
        Set<Boolean> booleans = new HashSet<>();
        for (int i = 0; i < node.size(); i++) {
            JsonNode tryNode = node.get(i);
            if (tryNode.isTextual()) {
                texts.add(tryNode.asText());
            } else if (tryNode.isNumber()) {
                doubles.add(tryNode.asDouble());
            } else if (tryNode.isBoolean()) {
                booleans.add(tryNode.asBoolean());
            }
        }
        ArrayNode array = MAPPER.createArrayNode();
        texts.forEach(value -> array.add(TextNode.valueOf(value)));
        doubles.forEach(value -> array.add(DoubleNode.valueOf(value)));
        booleans.forEach(value -> array.add(BooleanNode.valueOf(value)));
        return array;
    }

    static JsonNode funcFirst(JsonNode node, String params) {
        getParamNotAccept(params);
        if (!node.isArray()) {
            return node;
        }
        if (node.size() == 0) {
            return null;
        }
        return node.get(0);
    }

    static IntNode funcIndexOf(JsonNode node, String params) {
        List<String> paramList = decomposeFunctionParameters(params, 1, 1);
        if (!node.isArray()) {
            return null;
        }
        JsonNode valueNode = getNode(node, paramList.get(0));
        if (valueNode != null && valueNode.isValueNode()) {
            if (valueNode.isNumber()) {
                double value = valueNode.asDouble();
                for (int i = 0; i < node.size() - 1; i++) {
                    JsonNode tryNode = node.get(i);
                    if (tryNode.isNumber() || tryNode.isTextual()) {
                        if (tryNode.asDouble() == value) {
                            return IntNode.valueOf(i);
                        }
                    }
                }
            } else {
                String value = valueNode.asText();
                for (int i = 0; i < node.size() - 1; i++) {
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
        getParamNotAccept(params);
        if (!node.isArray()) {
            return node;
        }
        if (node.size() == 0) {
            return null;
        }
        return node.get(node.size() - 1);
    }

    static IntNode funcLastIndex(JsonNode node, String params) {
        getParamNotAccept(params);
        if (!node.isArray()) {
            return null;
        }
        return IntNode.valueOf(node.size() - 1);
    }

    static IntNode funcLastIndexOf(JsonNode node, String params) {
        List<String> paramList = decomposeFunctionParameters(params, 1, 1);
        if (!node.isArray()) {
            return null;
        }
        JsonNode valueNode = getNode(node, paramList.get(0));
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

    static JsonNode funcMaxMin(JsonNode node, String params, boolean isMax, int nullPriority) {
        List<String> paramList = decomposeFunctionParameters(params, 0, 1);
        if (!node.isArray()) {
            return node;
        }
        String path = paramList.size() > 0 ? paramList.get(0) : null;
        int foundIndex = -1;
        Double maxDouble = null;
        String maxMinString = null;
        for (int i = 0; i < node.size(); i++) {
            JsonNode value = getNode(node.get(i), path);
            if (value == null || value.isNull()) {
                if (nullPriority > 0) {
                    return value;
                }
                if (nullPriority < 0 && foundIndex < 0) {
                    foundIndex = i;
                }
                continue;
            }
            if (value.isContainerNode()) {
                continue;
            }
            if (maxDouble != null || value.isNumber()) {
                if (value.isNumber()) {
                    double asDouble = value.asDouble();
                    if (maxDouble == null || asDouble > maxDouble) {
                        maxDouble = isMax ? asDouble : -asDouble;
                        foundIndex = i;
                    }
                }
            } else {
                String asText = value.asText();
                if (maxMinString == null
                    || (isMax && asText.compareTo(maxMinString) > 0)
                    || (!isMax && asText.compareTo(maxMinString) < 0)) {
                    maxMinString = asText;
                    foundIndex = i;
                }
            }
        }
        return foundIndex >= 0 ? node.get(foundIndex) : null;
    }

    static JsonNode funcReverse(JsonNode node, String params) {
        getParamNotAccept(params);
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
        ImmutableTriple<Integer, Integer, Integer> startEndStep = getParamStartEndStep(params);
        if (!node.isArray()) {
            return node;
        }
        int size = node.size();
        int start = startEndStep.left >= 0 ? startEndStep.left : size + startEndStep.left;
        start = start < 0 ? 0 : Math.min(start, size);
        int end = startEndStep.middle >= 0 ? startEndStep.middle : size + startEndStep.middle;
        end = end < 0 ? 0 : Math.min(end, size);
        int step = startEndStep.right == 0 ? 1 : Math.abs(startEndStep.right);
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
        List<String> paramList = decomposeFunctionParameters(params, 0, 2);
        if (!node.isArray()) {
            return node;
        }
        String param = null;
        double ordering = 1;
        if (paramList.size() > 0) {
            param = paramList.get(0);
            try {
                ordering = Double.parseDouble(param);
                param = null;
                if (paramList.size() > 1) {
                    throw new IllegalArgumentException("Too many function arguments: " + params);
                }
            } catch (NumberFormatException e) {
                if (paramList.size() > 1) {
                    ordering = Double.parseDouble(paramList.get(1));
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
                    o1 = getNode(o1, path);
                    if (o1 == null) {
                        return 1;
                    }
                }
                if (o2.isObject()) {
                    o2 = getNode(o2, path);
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

package com.octomix.josson;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutableTriple;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;

import static com.octomix.josson.GetFuncParam.*;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Josson.getNode;

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

    static JsonNode funcMaxMin(JsonNode node, String params, boolean isMax, int nullPriority) {
        String path = null;
        Matcher m = DECOMPOSE_PARAMETERS.matcher(params);
        if (m.find()) {
            path = m.group(0);
            getParamNoMore(m);
        }
        if (!node.isArray()) {
            return node;
        }
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
        int start = startEndStep.left >= 0 ? startEndStep.left : node.size() + startEndStep.left;
        int end = startEndStep.middle < node.size() ? startEndStep.middle : node.size();
        int step = startEndStep.right;
        ArrayNode array = MAPPER.createArrayNode();
        if (step > 0) {
            for (int i = start; i < end; i += step) {
                array.add(node.get(i));
            }
        } else {
            for (int i = end - 1; i >= start; i += step) {
                array.add(node.get(i));
            }
        }
        return array;
    }

    static JsonNode funcSort(JsonNode node, String params) {
        Matcher m = DECOMPOSE_PARAMETERS.matcher(params);
        String param = null;
        double ordering = 1;
        if (m.find()) {
            param = m.group(0).trim();
            try {
                ordering = Double.parseDouble(param);
                param = null;
            } catch (NumberFormatException e) {
                if (m.find() && !StringUtils.isBlank(m.group(0))) {
                    ordering = Double.parseDouble(m.group(0).trim());
                }
            }
            getParamNoMore(m);
        }
        if (!node.isArray()) {
            return node;
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

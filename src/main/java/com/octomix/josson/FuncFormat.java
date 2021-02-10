package com.octomix.josson;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.TextNode;
import org.apache.commons.lang3.tuple.ImmutablePair;

import java.text.DecimalFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

import static com.octomix.josson.GetFuncParam.getParamArray;
import static com.octomix.josson.GetFuncParam.getParamStringLiteral;
import static com.octomix.josson.Josson.getNode;
import static com.octomix.josson.JossonCore.MAPPER;
import static com.octomix.josson.PatternMatcher.decomposeFunctionParameters;

class FuncFormat {
    static JsonNode funcCaseValue(JsonNode node, String params) {
        List<String> paramList = decomposeFunctionParameters(params, 1, -1);
        if (node.isObject()) {
            return null;
        }
        int last = paramList.size() - 1;
        List<ImmutablePair<JsonNode, JsonNode>> casePairs = new ArrayList<>();
        JsonNode defaultValue = null;
        for (int i = 0; i <= last; i++) {
            JsonNode caseKey = null;
            if (i < last) {
                if ("?".equals(paramList.get(i))) {
                    caseKey = node;
                } else {
                    caseKey = getNode(node, paramList.get(i));
                }
                i++;
            }
            JsonNode caseValue = "?".equals(paramList.get(i)) ? node : getNode(node, paramList.get(i));
            if (caseKey == null) {
                defaultValue = caseValue;
            } else {
                casePairs.add(ImmutablePair.of(caseKey, caseValue));
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isValueNode()) {
                    if (valueNode.isNumber()) {
                        double key = valueNode.asDouble();
                        ImmutablePair<JsonNode, JsonNode> casePair = casePairs.stream()
                            .filter(pair -> (pair.left.isNumber() || pair.left.isTextual()) && pair.left.asDouble() == key)
                            .findAny()
                            .orElse(null);
                        if (casePair != null) {
                            array.add(casePair.right);
                        } else if (defaultValue != null) {
                            array.add(defaultValue);
                        }
                    } else if (valueNode.isValueNode()) {
                        String key = node.asText();
                        ImmutablePair<JsonNode, JsonNode> casePair = casePairs.stream()
                            .filter(pair -> (pair.left.isNumber() || pair.left.isTextual()) && pair.left.asText().equals(key))
                            .findAny()
                            .orElse(null);
                        if (casePair != null) {
                            array.add(casePair.right);
                        } else if (defaultValue != null) {
                            array.add(defaultValue);
                        }
                    }
                }
            }
            return array;
        }
        if (node.isNumber()) {
            double key = node.asDouble();
            return casePairs.stream()
                    .filter(pair -> (pair.left.isNumber() || pair.left.isTextual()) && pair.left.asDouble() == key)
                    .findAny()
                    .map(pair -> pair.right)
                    .orElse(defaultValue);
        }
        String key = node.asText();
        return casePairs.stream()
                .filter(pair -> (pair.left.isNumber() || pair.left.isTextual()) && pair.left.asText().equals(key))
                .findAny()
                .map(pair -> pair.right)
                .orElse(defaultValue);
    }

    static JsonNode funcCycleValue(JsonNode node, String params) {
        ArrayNode paramArray = getParamArray(params, node);
        int size = paramArray.size();
        if (size == 0 || node.isNull() || node.isObject()) {
            return null;
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isValueNode() && !valueNode.isNull()) {
                    int index = valueNode.asInt() % size;
                    array.add(paramArray.get(index < 0 ? index + size : index));
                }
            }
            return array;
        }
        int index = node.asInt() % size;
        return paramArray.get(index < 0 ? index + size : index);
    }

    static JsonNode funcFormatDate(JsonNode node, String params) {
        String pattern = getParamStringLiteral(params);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(
                            LocalDateTime.parse(textNode.asText(), DateTimeFormatter.ISO_DATE_TIME)
                                    .format(DateTimeFormatter.ofPattern(pattern))));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(LocalDateTime.parse(node.asText(), DateTimeFormatter.ISO_DATE_TIME)
                .format(DateTimeFormatter.ofPattern(pattern)));
    }

    static JsonNode funcFormatNumber(JsonNode node, String params) {
        String pattern = getParamStringLiteral(params);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isValueNode()) {
                    array.add(TextNode.valueOf(
                            new DecimalFormat(pattern).format(textNode.asDouble())));
                }
            }
            return array;
        }
        if (!node.isValueNode()) {
            return null;
        }
        return TextNode.valueOf(new DecimalFormat(pattern).format(node.asDouble()));
    }

    static JsonNode funcFormatText(JsonNode node, String params) {
        String pattern = getParamStringLiteral(params);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isValueNode()) {
                    array.add(TextNode.valueOf(
                            String.format(pattern, textNode.asText())));
                }
            }
            return array;
        }
        if (!node.isValueNode()) {
            return null;
        }
        return TextNode.valueOf(String.format(pattern, node.asText()));
    }

    static JsonNode funcIndexedValue(JsonNode node, String params) {
        ArrayNode paramArray = getParamArray(params, node);
        int size = paramArray.size();
        if (size == 0 || node.isNull() || node.isObject()) {
            return null;
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isValueNode() && !valueNode.isNull()) {
                    int index = valueNode.asInt();
                    if (index >= 0 && index < size) {
                        array.add(paramArray.get(index));
                    }
                }
            }
            return array;
        }
        int index = node.asInt();
        return index >= 0 && index < size ? paramArray.get(index) : null;
    }
}

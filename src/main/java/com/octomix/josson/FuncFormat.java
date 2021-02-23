package com.octomix.josson;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.DoubleNode;
import com.fasterxml.jackson.databind.node.TextNode;
import org.apache.commons.lang3.tuple.ImmutablePair;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.text.DecimalFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

import static com.octomix.josson.GetFuncParam.*;
import static com.octomix.josson.Josson.getNode;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.PatternMatcher.decomposeFunctionParameters;

class FuncFormat {
    static JsonNode funcB64Decode(JsonNode node, String params, Base64.Decoder decoder) {
        String value = getParamStringLiteral(params, false);
        if (value != null) {
            return TextNode.valueOf(new String(decoder.decode(value)));
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(
                            new String(decoder.decode(textNode.asText()))));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(new String(decoder.decode(node.asText())));
    }

    static JsonNode funcB64Encode(JsonNode node, String params, Base64.Encoder encoder) {
        String value = getParamStringLiteral(params, false);
        if (value != null) {
            return TextNode.valueOf(encoder.encodeToString(value.getBytes()));
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(encoder.encodeToString(textNode.asText().getBytes())));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(encoder.encodeToString(node.asText().getBytes()));
    }

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
                if (nodeHasValue(valueNode)) {
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
            for (int i = 0; i < node.size(); i++) {
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
            for (int i = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (nodeHasValue(textNode)) {
                    array.add(TextNode.valueOf(new DecimalFormat(pattern).format(textNode.asDouble())));
                }
            }
            return array;
        }
        if (!nodeHasValue(node)) {
            return null;
        }
        return TextNode.valueOf(new DecimalFormat(pattern).format(node.asDouble()));
    }

    static JsonNode funcFormatText(JsonNode node, String params) {
        List<String> paramList = decomposeFunctionParameters(params, 1, -1);
        String pattern = unquoteString(paramList.remove(0));
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                Object[] valueObjects = valuesAsObjects(node.get(i), paramList, i);
                if (valueObjects != null) {
                    array.add(TextNode.valueOf(String.format(pattern, valueObjects)));
                }
            }
            return array;
        }
        Object[] valueObjects = valuesAsObjects(node, paramList, 0);
        if (valueObjects == null) {
            return null;
        }
        return TextNode.valueOf(String.format(pattern, valueObjects));
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
                if (nodeHasValue(valueNode)) {
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

    static JsonNode funcToNumber(JsonNode node, String params) {
        getParamNotAccept(params);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                JsonNode tryNode = node.get(i);
                if (nodeHasValue(tryNode)) {
                    array.add(tryNode.isNumber() ? tryNode : DoubleNode.valueOf(tryNode.asDouble()));
                }
            }
            return array;
        }
        if (!nodeHasValue(node)) {
            return null;
        }
        return node.isNumber() ? node : DoubleNode.valueOf(node.asDouble());
    }

    static JsonNode funcToText(JsonNode node, String params) {
        getParamNotAccept(params);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                JsonNode tryNode = node.get(i);
                if (nodeHasValue(tryNode)) {
                    array.add(tryNode.isTextual() ? tryNode : TextNode.valueOf(tryNode.asText()));
                }
            }
            return array;
        }
        if (!nodeHasValue(node)) {
            return null;
        }
        return node.isTextual() ? node : TextNode.valueOf(node.asText());
    }

    static JsonNode funcUrlDecode(JsonNode node, String params) {
        String value = getParamStringLiteral(params, false);
        try {
            if (value != null) {
                return TextNode.valueOf(URLDecoder.decode(value, StandardCharsets.UTF_8.toString()));
            }
            if (node.isArray()) {
                ArrayNode array = MAPPER.createArrayNode();
                for (int i  = 0; i < node.size(); i++) {
                    JsonNode textNode = node.get(i);
                    if (textNode.isTextual()) {
                        array.add(TextNode.valueOf(URLDecoder.decode(textNode.asText(), StandardCharsets.UTF_8.toString())));
                    }
                }
                return array;
            }
            if (!node.isTextual()) {
                return null;
            }
            return TextNode.valueOf(URLDecoder.decode(node.asText(), StandardCharsets.UTF_8.toString()));
        } catch (UnsupportedEncodingException e) {
            throw new IllegalArgumentException(e.getMessage());
        }
    }

    static JsonNode funcUrlEncode(JsonNode node, String params) {
        String value = getParamStringLiteral(params, false);
        try {
            if (value != null) {
                return TextNode.valueOf(URLEncoder.encode(value, StandardCharsets.UTF_8.toString()));
            }
            if (node.isArray()) {
                ArrayNode array = MAPPER.createArrayNode();
                for (int i  = 0; i < node.size(); i++) {
                    JsonNode textNode = node.get(i);
                    if (textNode.isTextual()) {
                        array.add(TextNode.valueOf(URLEncoder.encode(textNode.asText(), StandardCharsets.UTF_8.toString())));
                    }
                }
                return array;
            }
            if (!node.isTextual()) {
                return null;
            }
            return TextNode.valueOf(URLEncoder.encode(node.asText(), StandardCharsets.UTF_8.toString()));
        } catch (UnsupportedEncodingException e) {
            throw new IllegalArgumentException(e.getMessage());
        }
    }
}

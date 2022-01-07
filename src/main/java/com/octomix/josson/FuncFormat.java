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
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.DoubleNode;
import com.fasterxml.jackson.databind.node.TextNode;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.text.DecimalFormat;
import java.time.format.DateTimeFormatter;
import java.util.*;

import static com.octomix.josson.GetFuncParam.*;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.PatternMatcher.decomposeFunctionParameters;

class FuncFormat {
    static JsonNode funcB64Decode(JsonNode node, String params, Base64.Decoder decoder) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNodeByPath(node, path);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(new String(decoder.decode(textNode.asText()))));
                } else {
                    array.addNull();
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
        String path = getParamPath(params);
        if (path != null) {
            node = getNodeByPath(node, path);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(encoder.encodeToString(textNode.asText().getBytes())));
                } else {
                    array.addNull();
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
        List<Pair<JsonNode, JsonNode>> casePairs = new ArrayList<>();
        JsonNode defaultValue = null;
        for (int i = 0; i <= last; i++) {
            JsonNode caseKey = null;
            if (i < last) {
                caseKey = getNodeByPath(node, paramList.get(i));
                i++;
            }
            JsonNode caseValue = getNodeByPath(node, paramList.get(i));
            if (caseKey == null) {
                defaultValue = caseValue;
            } else {
                casePairs.add(Pair.of(caseKey, caseValue));
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isValueNode()) {
                    if (valueNode.isNumber()) {
                        double key = valueNode.asDouble();
                        Pair<JsonNode, JsonNode> casePair = casePairs.stream()
                                .filter(pair ->
                                        (pair.getKey().isNumber() || pair.getKey().isTextual())
                                                && pair.getKey().asDouble() == key)
                                .findFirst()
                                .orElse(null);
                        if (casePair != null) {
                            array.add(casePair.getValue());
                        } else if (defaultValue != null) {
                            array.add(defaultValue);
                        }
                    } else if (valueNode.isValueNode()) {
                        String key = node.asText();
                        Pair<JsonNode, JsonNode> casePair = casePairs.stream()
                                .filter(pair ->
                                        (pair.getKey().isNumber() || pair.getKey().isTextual())
                                                && pair.getKey().asText().equals(key))
                                .findFirst()
                                .orElse(null);
                        if (casePair != null) {
                            array.add(casePair.getValue());
                        } else if (defaultValue != null) {
                            array.add(defaultValue);
                        }
                    }
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (node.isNumber()) {
            double key = node.asDouble();
            return casePairs.stream()
                    .filter(pair ->
                            (pair.getKey().isNumber() || pair.getKey().isTextual())
                                    && pair.getKey().asDouble() == key)
                    .findFirst()
                    .map(Pair::getValue)
                    .orElse(defaultValue);
        }
        String key = node.asText();
        return casePairs.stream()
                .filter(pair ->
                        (pair.getKey().isNumber() || pair.getKey().isTextual())
                                && pair.getKey().asText().equals(key))
                .findFirst()
                .map(Pair::getValue)
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
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        int index = node.asInt() % size;
        return paramArray.get(index < 0 ? index + size : index);
    }

    static JsonNode funcFormatDate(JsonNode node, String params) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        String pattern = getNodeAsText(node, pathAndParams.getValue().get(0));
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(toLocalDateTime(textNode).format(DateTimeFormatter.ofPattern(pattern))));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(toLocalDateTime(node).format(DateTimeFormatter.ofPattern(pattern)));
    }

    static JsonNode funcFormatNumber(JsonNode node, String params) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        String pattern = getNodeAsText(node, pathAndParams.getValue().get(0));
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (nodeHasValue(textNode)) {
                    array.add(TextNode.valueOf(new DecimalFormat(pattern).format(textNode.asDouble())));
                } else {
                    array.addNull();
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
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        String pattern = getNodeAsText(node, pathAndParams.getValue().get(0));
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                Object valueObject = valueAsObject(node.get(i));
                if (valueObject != null) {
                    array.add(TextNode.valueOf(String.format(pattern, valueObject)));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!nodeHasValue(node)) {
            return null;
        }
        return TextNode.valueOf(String.format(pattern, valueAsObject(node)));
    }

    static JsonNode funcFormatTexts(JsonNode node, String params) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 2, -2);
        String pattern = getNodeAsText(node, pathAndParams.getKey());
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                Object[] valueObjects = valuesAsObjects(node, i, pathAndParams.getValue());
                if (valueObjects != null) {
                    array.add(TextNode.valueOf(String.format(pattern, valueObjects)));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        Object[] valueObjects = valuesAsObjects(node, -1, pathAndParams.getValue());
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
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        int index = node.asInt();
        return index >= 0 && index < size ? paramArray.get(index) : null;
    }

    static JsonNode funcToNumber(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNodeByPath(node, path);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                JsonNode tryNode = node.get(i);
                if (nodeHasValue(tryNode)) {
                    array.add(tryNode.isNumber() ? tryNode : DoubleNode.valueOf(tryNode.asDouble()));
                } else {
                    array.addNull();
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
        String path = getParamPath(params);
        if (path != null) {
            node = getNodeByPath(node, path);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                JsonNode tryNode = node.get(i);
                if (nodeHasValue(tryNode)) {
                    array.add(tryNode.isTextual() ? tryNode : TextNode.valueOf(tryNode.asText()));
                } else {
                    array.addNull();
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
        String path = getParamPath(params);
        if (path != null) {
            node = getNodeByPath(node, path);
            if (node == null) {
                return null;
            }
        }
        try {
            if (node.isArray()) {
                ArrayNode array = MAPPER.createArrayNode();
                for (int i  = 0; i < node.size(); i++) {
                    JsonNode textNode = node.get(i);
                    if (textNode.isTextual()) {
                        array.add(TextNode.valueOf(URLDecoder.decode(textNode.asText(), StandardCharsets.UTF_8.toString())));
                    } else {
                        array.addNull();
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
        String path = getParamPath(params);
        if (path != null) {
            node = getNodeByPath(node, path);
            if (node == null) {
                return null;
            }
        }
        try {
            if (node.isArray()) {
                ArrayNode array = MAPPER.createArrayNode();
                for (int i  = 0; i < node.size(); i++) {
                    JsonNode textNode = node.get(i);
                    if (textNode.isTextual()) {
                        array.add(TextNode.valueOf(URLEncoder.encode(textNode.asText(), StandardCharsets.UTF_8.toString())));
                    } else {
                        array.addNull();
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

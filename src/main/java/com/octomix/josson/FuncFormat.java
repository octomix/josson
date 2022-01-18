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

import static com.octomix.josson.FuncExecutor.*;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.PatternMatcher.decomposeFunctionParameters;

class FuncFormat {
    static JsonNode funcB64Decode(JsonNode node, String params, Base64.Decoder decoder) {
        return applyFunc(node, params,
                JsonNode::isTextual,
                jsonNode -> TextNode.valueOf(new String(decoder.decode(jsonNode.asText())))
        );
    }

    static JsonNode funcB64Encode(JsonNode node, String params, Base64.Encoder encoder) {
        return applyFunc(node, params,
                JsonNode::isTextual,
                jsonNode -> TextNode.valueOf(encoder.encodeToString(jsonNode.asText().getBytes()))
        );
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
        return applyFunc(node, params, 1, 1,
                paramList -> getNodeAsText(node, paramList.get(0)),
                JsonNode::isTextual,
                (jsonNode, objVar) -> TextNode.valueOf(toLocalDateTime(jsonNode).format(DateTimeFormatter.ofPattern((String) objVar)))
        );
    }

    static JsonNode funcFormatNumber(JsonNode node, String params) {
        return applyFunc(node, params, 1, 1,
                paramList -> getNodeAsText(node, paramList.get(0)),
                jsonNode -> jsonNode.isNumber() || jsonNode.isTextual(),
                (jsonNode, objVar) -> TextNode.valueOf(new DecimalFormat((String) objVar).format(jsonNode.asDouble()))
        );
    }

    static JsonNode funcFormatText(JsonNode node, String params) {
        return applyFunc(node, params, 1, 1,
                paramList -> getNodeAsText(node, paramList.get(0)),
                JossonCore::nodeHasValue,
                (jsonNode, objVar) -> TextNode.valueOf(String.format((String) objVar, valueAsObject(jsonNode)))
        );
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
        return applyFunc(node, params,
                JossonCore::nodeHasValue,
                jsonNode -> jsonNode.isNumber() ? jsonNode : DoubleNode.valueOf(jsonNode.asDouble())
        );
    }

    static JsonNode funcToText(JsonNode node, String params) {
        return applyFunc(node, params,
                JossonCore::nodeHasValue,
                jsonNode -> jsonNode.isTextual() ? jsonNode : TextNode.valueOf(jsonNode.asText())
        );
    }

    static JsonNode funcUrlDecode(JsonNode node, String params) {
        return applyFunc(node, params,
                JsonNode::isTextual,
                jsonNode -> {
                    try {
                        return TextNode.valueOf(URLDecoder.decode(jsonNode.asText(), StandardCharsets.UTF_8.toString()));
                    } catch (UnsupportedEncodingException e) {
                        throw new IllegalArgumentException(e.getMessage());
                    }
                }
        );
    }

    static JsonNode funcUrlEncode(JsonNode node, String params) {
        return applyFunc(node, params,
                JsonNode::isTextual,
                jsonNode -> {
                    try {
                        return TextNode.valueOf(URLEncoder.encode(jsonNode.asText(), StandardCharsets.UTF_8.toString()));
                    } catch (UnsupportedEncodingException e) {
                        throw new IllegalArgumentException(e.getMessage());
                    }
                }
        );
    }
}

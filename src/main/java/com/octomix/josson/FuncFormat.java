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

/**
 * Format functions.
 */
class FuncFormat {

    private FuncFormat() {
    }

    static JsonNode funcB64Decode(final JsonNode node, final String params, final Base64.Decoder decoder) {
        return applyTextNode(node, params, jsonNode -> new String(decoder.decode(jsonNode.asText())));
    }

    static JsonNode funcB64Encode(final JsonNode node, final String params, final Base64.Encoder encoder) {
        return applyTextNode(node, params, jsonNode -> encoder.encodeToString(jsonNode.asText().getBytes()));
    }

    static JsonNode funcCaseValue(final JsonNode node, final String params) {
        final List<String> paramList = decomposeFunctionParameters(params, 1, -1);
        if (node.isObject()) {
            return null;
        }
        final int last = paramList.size() - 1;
        final List<Pair<JsonNode, JsonNode>> casePairs = new ArrayList<>();
        JsonNode defaultValue = null;
        for (int i = 0; i <= last; i++) {
            final JsonNode caseKey = getNodeByPath(node, paramList.get(i++));
            if (caseKey != null) {
                if (i > last) {
                    defaultValue = caseKey;
                } else {
                    casePairs.add(Pair.of(caseKey, getNodeByPath(node, paramList.get(i))));
                }
            }
        }
        if (node.isArray()) {
            final ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                array.add(funcCaseValue(node.get(i), casePairs, defaultValue));
            }
            return array;
        }
        return funcCaseValue(node, casePairs, defaultValue);
    }

    private static JsonNode funcCaseValue(final JsonNode node, final List<Pair<JsonNode, JsonNode>> casePairs,
                                          final JsonNode defaultValue) {
        if (node.isNumber()) {
            final double key = node.asDouble();
            return casePairs.stream()
                    .filter(pair -> {
                        final JsonNode caseKey = pair.getKey();
                        return (caseKey.isNumber() || caseKey.isTextual()) && caseKey.asDouble() == key;
                    })
                    .findFirst()
                    .map(Pair::getValue)
                    .orElse(defaultValue);
        }
        if (node.isNull()) {
            return casePairs.stream()
                    .filter(pair -> pair.getKey().isNull())
                    .findFirst()
                    .map(Pair::getValue)
                    .orElse(defaultValue);
        }
        final String key = node.asText();
        return casePairs.stream()
                .filter(pair -> {
                    final JsonNode caseKey = pair.getKey();
                    return (caseKey.isNumber() || caseKey.isTextual()) && caseKey.asText().equals(key);
                })
                .findFirst()
                .map(Pair::getValue)
                .orElse(defaultValue);
    }

    static JsonNode funcCycleValue(final JsonNode node, final String params) {
        return applyWithArrayNode(node, params, JossonCore::nodeHasValue,
                (jsonNode, objVar) -> {
                    final ArrayNode paramArray = (ArrayNode) objVar;
                    final int size = paramArray.size();
                    final int index = jsonNode.asInt() % size;
                    return paramArray.get(index < 0 ? index + size : index);
                }
        );
    }

    static JsonNode funcFormatDate(final JsonNode node, final String params) {
        return applyWithParamAsText(node, params, JsonNode::isTextual,
                (jsonNode, objVar) -> TextNode.valueOf(toLocalDateTime(jsonNode)
                    .format(DateTimeFormatter.ofPattern((String) objVar).withLocale(getLocale()).withZone(getZoneId())))
        );
    }

    static JsonNode funcFormatNumber(final JsonNode node, final String params) {
        return applyWithParamAsText(node, params, jsonNode -> jsonNode.isNumber() || jsonNode.isTextual(),
                (jsonNode, objVar) -> TextNode.valueOf(new DecimalFormat((String) objVar).format(jsonNode.asDouble())));
    }

    static JsonNode funcFormatText(final JsonNode node, final String params) {
        return applyWithParamAsText(node, params, JossonCore::nodeHasValue,
                (jsonNode, objVar) -> TextNode.valueOf(String.format((String) objVar, valueAsObject(jsonNode))));
    }

    static JsonNode funcFormatTexts(final JsonNode node, final String params) {
        final Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 2, -2);
        final String pattern = getNodeAsText(node, pathAndParams.getKey());
        if (node.isArray()) {
            final ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                final Object[] valueObjects = valuesAsObjects(node, i, pathAndParams.getValue());
                if (valueObjects != null) {
                    array.add(TextNode.valueOf(String.format(pattern, valueObjects)));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        final Object[] valueObjects = valuesAsObjects(node, -1, pathAndParams.getValue());
        if (valueObjects == null) {
            return null;
        }
        return TextNode.valueOf(String.format(pattern, valueObjects));
    }

    static JsonNode funcIndexedValue(final JsonNode node, final String params) {
        return applyWithArrayNode(node, params, JossonCore::nodeHasValue,
                (jsonNode, objVar) -> {
                    final ArrayNode paramArray = (ArrayNode) objVar;
                    final int index = jsonNode.asInt();
                    return index >= 0 && index < paramArray.size() ? paramArray.get(index) : null;
                }
        );
    }

    static JsonNode funcToNumber(final JsonNode node, final String params) {
        return applyWithoutParam(node, params, JossonCore::nodeHasValue,
                jsonNode -> jsonNode.isNumber() ? jsonNode : DoubleNode.valueOf(jsonNode.asDouble()));
    }

    static JsonNode funcToString(final JsonNode node, final String params) {
        return applyWithoutParam(node, params,
                jsonNode -> jsonNode.isTextual() ? jsonNode
                        : TextNode.valueOf(jsonNode.isValueNode() ? jsonNode.asText() : jsonNode.toString()));
    }

    static JsonNode funcToText(final JsonNode node, final String params) {
        return applyWithoutParam(node, params, JsonNode::isValueNode,
                jsonNode -> jsonNode.isTextual() ? jsonNode : TextNode.valueOf(jsonNode.asText()));
    }

    static JsonNode funcUrlDecode(final JsonNode node, final String params) {
        return applyTextNode(node, params,
                jsonNode -> {
                    try {
                        return URLDecoder.decode(jsonNode.asText(), StandardCharsets.UTF_8.toString());
                    } catch (UnsupportedEncodingException e) {
                        throw new IllegalArgumentException(e.getMessage());
                    }
                }
        );
    }

    static JsonNode funcUrlEncode(final JsonNode node, final String params) {
        return applyTextNode(node, params,
                jsonNode -> {
                    try {
                        return URLEncoder.encode(jsonNode.asText(), StandardCharsets.UTF_8.toString());
                    } catch (UnsupportedEncodingException e) {
                        throw new IllegalArgumentException(e.getMessage());
                    }
                }
        );
    }
}

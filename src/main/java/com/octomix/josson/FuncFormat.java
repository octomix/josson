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
import com.fasterxml.jackson.databind.node.DoubleNode;
import com.fasterxml.jackson.databind.node.TextNode;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.text.DecimalFormat;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;

import static com.octomix.josson.FuncExecutor.*;
import static com.octomix.josson.JossonCore.*;

/**
 * Format functions.
 */
final class FuncFormat {

    private FuncFormat() {
    }

    static JsonNode funcB64Decode(final JsonNode node, final String params, final Base64.Decoder decoder) {
        return applyTextNode(node, params, jsonNode -> new String(decoder.decode(jsonNode.asText())));
    }

    static JsonNode funcB64Encode(final JsonNode node, final String params, final Base64.Encoder encoder) {
        return applyTextNode(node, params, jsonNode -> encoder.encodeToString(jsonNode.asText().getBytes()));
    }

    static JsonNode funcCaseValue(final JsonNode node, final String params) {
        return applyWithParams(node, params, 2, -3, JsonNode::isValueNode,
                (data, paramList) -> {
                    final JsonNode dataNode = data.getKey();
                    final int last = paramList.size() - 1;
                    int i = 0;
                    for (; i < last; i += 2) {
                        final JsonNode caseKey = getNodeByPath(node, data.getValue(), paramList.get(i));
                        if (dataNode.isNumber() && (caseKey.isNumber() || caseKey.isTextual()) && caseKey.asDouble() == dataNode.asDouble()
                                || dataNode.isNull() && caseKey.isNull()
                                || caseKey.asText().equals(dataNode.asText())) {
                            return getNodeByPath(node, data.getValue(), paramList.get(i + 1));
                        }
                    }
                    return i == last ? getNodeByPath(node, data.getValue(), paramList.get(i)) : null;
                });
    }

    static TextNode funcCsv(final JsonNode node, final String params, final boolean showNull) {
        final JsonNode container = getParamArrayOrItselfIsContainer(params, node);
        if (container == null) {
            return null;
        }
        final List<JsonNode> values = new ArrayList<>();
        funcCsvCollectValues(values, container, showNull);
        return TextNode.valueOf(values.stream()
                .map(value -> csvQuote(value.asText()))
                .collect(Collectors.joining(",")));
    }

    private static void funcCsvCollectValues(final List<JsonNode> values, final JsonNode node, final boolean showNull) {
        if (node.isObject()) {
            node.forEach(elem -> {
                if (elem.isContainerNode()) {
                    funcCsvCollectValues(values, elem, showNull);
                } else {
                    values.add(showNull || !elem.isNull() ? elem : TextNode.valueOf(""));
                }
            });
            return;
        }
        for (int i = 0; i < node.size(); i++) {
            final JsonNode tryNode = node.get(i);
            if (tryNode.isContainerNode()) {
                funcCsvCollectValues(values, tryNode, showNull);
            } else {
                values.add(showNull || !tryNode.isNull() ? tryNode : TextNode.valueOf(""));
            }
        }
    }

    static JsonNode funcCycleValue(final JsonNode node, final String params) {
        return applyWithArrayNode(node, params, JossonCore::nodeHasValue,
                (jsonNode, paramArray) -> {
                    final int size = paramArray.size();
                    final int index = jsonNode.asInt() % size;
                    return paramArray.get(index < 0 ? index + size : index);
                });
    }

    static JsonNode funcFormatDate(final JsonNode node, final String params) {
        return applyWithParams(node, params, 1, 1, JsonNode::isTextual,
                (data, paramList) -> {
                    final JsonNode dataNode = data.getKey();
                    final JsonNode paramNode = data.getValue() < 0 ? dataNode : node;
                    final String pattern = getNodeAsText(paramNode, data.getValue(), paramList.get(0));
                    return TextNode.valueOf(toLocalDateTime(dataNode)
                            .format(DateTimeFormatter.ofPattern(pattern).withLocale(getLocale()).withZone(getZoneId())));
                });
    }

    static JsonNode funcFormatNumber(final JsonNode node, final String params) {
        return applyWithParams(node, params, 1, 1, jsonNode -> jsonNode.isNumber() || jsonNode.isTextual(),
                (data, paramList) -> {
                    final JsonNode dataNode = data.getKey();
                    final JsonNode paramNode = data.getValue() < 0 ? dataNode : node;
                    final String pattern = getNodeAsText(paramNode, data.getValue(), paramList.get(0));
                    return TextNode.valueOf(new DecimalFormat(pattern).format(dataNode.asDouble()));
                });
    }

    static JsonNode funcFormatText(final JsonNode node, final String params) {
        return applyWithParams(node, params, 1, 1, JossonCore::nodeHasValue,
                (data, paramList) -> {
                    final JsonNode dataNode = data.getKey();
                    final JsonNode paramNode = data.getValue() < 0 ? dataNode : node;
                    final String format = getNodeAsText(paramNode, data.getValue(), paramList.get(0));
                    return TextNode.valueOf(String.format(format, valueAsObject(dataNode)));
                });
    }

    static JsonNode funcFormatTexts(final JsonNode node, final String params) {
        return applyWithParams(node, params, 2, -3, null,
                (data, paramList) -> {
                    final String format = getNodeAsText(node, data.getValue(), paramList.get(0));
                    if (format == null) {
                        return null;
                    }
                    final Object[] valueObjects = valuesAsObjects(node, data.getValue(), paramList.subList(1, paramList.size()));
                    if (valueObjects == null) {
                        return null;
                    }
                    return TextNode.valueOf(String.format(format, valueObjects));
                });
    }

    static JsonNode funcIf(final JsonNode node, final String params) {
        return applyWithParams(node, params, 2, 3, null,
                (data, paramList) -> {
                    final JsonNode paramNode = data.getValue() < 0 ? data.getKey() : node;
                    final String query =
                            asBoolean(getNodeByPath(paramNode, data.getValue(), paramList.get(0))) ? paramList.get(1)
                            : paramList.size() > 2 ? paramList.get(2)
                            : null;
                    return query == null ? null : getNodeByPath(paramNode, data.getValue(), query);
                });
    }

    static JsonNode funcIndexedValue(final JsonNode node, final String params) {
        return applyWithArrayNode(node, params, JossonCore::nodeHasValue,
                (jsonNode, paramArray) -> {
                    final int index = jsonNode.asInt();
                    return index >= 0 && index < paramArray.size() ? paramArray.get(index) : null;
                });
    }

    static JsonNode funcToNumber(final JsonNode node, final String params) {
        return applyWithoutParam(node, params, JossonCore::nodeHasValue,
                (data, paramList) -> {
                    final JsonNode dataNode = data.getKey();
                    return dataNode.isNumber() ? dataNode : DoubleNode.valueOf(dataNode.asDouble());
                });
    }

    static JsonNode funcToString(final JsonNode node, final String params) {
        return applyWithoutParam(node, params,
                jsonNode -> jsonNode.isTextual() ? jsonNode
                        : TextNode.valueOf(jsonNode.isValueNode() ? jsonNode.asText() : jsonNode.toString()));
    }

    static JsonNode funcToText(final JsonNode node, final String params) {
        return applyWithoutParam(node, params, JsonNode::isValueNode,
                (data, paramList) -> {
                    final JsonNode dataNode = data.getKey();
                    return dataNode.isTextual() ? dataNode : TextNode.valueOf(dataNode.asText());
                });
    }

    static JsonNode funcUrlDecode(final JsonNode node, final String params) {
        return applyTextNode(node, params,
                jsonNode -> {
                    try {
                        return URLDecoder.decode(jsonNode.asText(), StandardCharsets.UTF_8.toString());
                    } catch (UnsupportedEncodingException e) {
                        throw new IllegalArgumentException(e.getMessage());
                    }
                });
    }

    static JsonNode funcUrlEncode(final JsonNode node, final String params) {
        return applyTextNode(node, params,
                jsonNode -> {
                    try {
                        return URLEncoder.encode(jsonNode.asText(), StandardCharsets.UTF_8.toString());
                    } catch (UnsupportedEncodingException e) {
                        throw new IllegalArgumentException(e.getMessage());
                    }
                });
    }
}

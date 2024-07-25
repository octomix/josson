/*
 * Copyright 2020-2024 Choi Wai Man Raymond
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

import static com.octomix.josson.FunctionExecutor.*;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Utils.*;

/**
 * Format functions.
 */
final class FuncFormat {

    private FuncFormat() {
    }

    static PathTrace funcB64Decode(final PathTrace path, final String params, final Base64.Decoder decoder) {
        return applyTextNode(path, params, dataPath -> new String(decoder.decode(dataPath.asText())));
    }

    static PathTrace funcB64Encode(final PathTrace path, final String params, final Base64.Encoder encoder) {
        return applyTextNode(path, params, dataPath -> encoder.encodeToString(dataPath.asText().getBytes()));
    }

    static PathTrace funcCsv(final PathTrace path, final String params, final boolean showNull, final boolean forParams) {
        final PathTrace container = getParamArrayOrItselfIsContainer(path, params);
        if (container == null) {
            return null;
        }
        final List<JsonNode> values = new ArrayList<>();
        funcCsvCollectValues(values, container.node(), showNull);
        return path.push(TextNode.valueOf(values.stream()
                .map(value -> forParams ? valueNodeToLiteral(value) : funcCsvQuote(value.asText()))
                .collect(Collectors.joining(","))));
    }

    private static void funcCsvCollectValues(final List<JsonNode> values, final JsonNode node, final boolean showNull) {
        if (node.isObject()) {
            node.forEach(elem -> {
                if (elem.isContainerNode()) {
                    funcCsvCollectValues(values, elem, showNull);
                } else {
                    values.add(showNull || !elem.isNull() ? elem : EMPTY_STRING_NODE);
                }
            });
            return;
        }
        node.forEach(elem -> {
            if (elem.isContainerNode()) {
                funcCsvCollectValues(values, elem, showNull);
            } else {
                values.add(showNull || !elem.isNull() ? elem : EMPTY_STRING_NODE);
            }
        });
    }

    private static String funcCsvQuote(final String input) {
        final String quote = "\"";
        final String result;
        final boolean needQuote;
        if (input.contains(quote)) {
            needQuote = true;
            result = input.replace(quote, "\"\"");
        } else {
            needQuote = input.contains(",");
            result = input;
        }
        if (needQuote) {
            return quote + result + quote;
        }
        return result;
    }

    static PathTrace funcFormatDate(final PathTrace path, final String params) {
        return applyWithParams(path, params, 1, 1, JsonNode::isTextual,
            (data, paramList) -> {
                final PathTrace dataPath = data.getKey();
                final PathTrace paramPath = data.getValue() < 0 ? dataPath : path;
                final String pattern = getNodeAsText(paramPath, data.getValue(), paramList.get(0));
                return path.push(TextNode.valueOf(toLocalDateTime(dataPath.node())
                        .format(DateTimeFormatter.ofPattern(pattern).withLocale(getLocale()).withZone(getZoneId()))));
            });
    }

    static PathTrace funcFormatNumber(final PathTrace path, final String params) {
        return applyWithParams(path, params, 1, 1, node -> node.isNumber() || node.isTextual(),
            (data, paramList) -> {
                final PathTrace dataPath = data.getKey();
                final PathTrace paramPath = data.getValue() < 0 ? dataPath : path;
                final String pattern = getNodeAsText(paramPath, data.getValue(), paramList.get(0));
                return path.push(TextNode.valueOf(new DecimalFormat(pattern).format(dataPath.asDouble())));
            });
    }

    static PathTrace funcFormatText(final PathTrace path, final String params) {
        return applyWithParams(path, params, 1, 1, Utils::nodeHasValue,
            (data, paramList) -> {
                final PathTrace dataPath = data.getKey();
                final PathTrace paramPath = data.getValue() < 0 ? dataPath : path;
                final String format = getNodeAsText(paramPath, data.getValue(), paramList.get(0));
                return path.push(TextNode.valueOf(String.format(format, valueAsObject(dataPath.node()))));
            });
    }

    static PathTrace funcFormatTexts(final PathTrace path, final String params) {
        return applyWithParams(path, params, 2, UNLIMITED_AND_NO_PATH, null,
            (data, paramList) -> {
                final String format = getNodeAsTextExceptNull(path, data.getValue(), paramList.get(0));
                if (format == null) {
                    return null;
                }
                final Object[] valueObjects = valuesAsObjects(path, data.getValue(), paramList.subList(1, paramList.size()));
                if (valueObjects == null) {
                    return null;
                }
                return path.push(TextNode.valueOf(String.format(format, valueObjects)));
            });
    }

    static PathTrace funcMarkupEscape(final PathTrace path, final String params, final MarkupLanguage language) {
        return applyTextNode(path, params, dataPath -> language.escape(dataPath.asText()));
    }

    static PathTrace funcMarkupUnescape(final PathTrace path, final String params, final MarkupLanguage language) {
        return applyTextNode(path, params, dataPath -> language.unescape(dataPath.asText()));
    }

    static PathTrace funcToNumber(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, Utils::nodeHasValue,
            (data, paramList) -> data.getKey().isNumber()
                ? data.getKey()
                : path.push(DoubleNode.valueOf(data.getKey().asDouble())));
    }

    static PathTrace funcToString(final PathTrace path, final String params) {
        return applyWithoutParam(path, params,
            dataPath -> dataPath.isTextual() ? dataPath
                : path.push(TextNode.valueOf(dataPath.isValueNode() ? dataPath.asText()
                : dataPath.node().toString())));
    }

    static PathTrace funcToText(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, JsonNode::isValueNode,
            (data, paramList) -> data.getKey().isTextual()
                ? data.getKey()
                : path.push(TextNode.valueOf(data.getKey().asText())));
    }

    static PathTrace funcUrlDecode(final PathTrace path, final String params) {
        return applyTextNode(path, params,
            dataPath -> {
                try {
                    return URLDecoder.decode(dataPath.asText(), StandardCharsets.UTF_8.toString());
                } catch (UnsupportedEncodingException e) {
                    throw new IllegalArgumentException(e.getMessage());
                }
            });
    }

    static PathTrace funcUrlEncode(final PathTrace path, final String params) {
        return applyTextNode(path, params,
            dataPath -> {
                try {
                    return URLEncoder.encode(dataPath.asText(), StandardCharsets.UTF_8.toString());
                } catch (UnsupportedEncodingException e) {
                    throw new IllegalArgumentException(e.getMessage());
                }
            });
    }
}

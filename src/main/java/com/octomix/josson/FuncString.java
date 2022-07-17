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
import com.fasterxml.jackson.databind.node.IntNode;
import com.fasterxml.jackson.databind.node.TextNode;
import com.octomix.josson.commons.StringUtils;

import java.util.function.Function;

import static com.octomix.josson.FuncExecutor.*;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.commons.StringUtils.EMPTY;

/**
 * String functions.
 */
final class FuncString {

    private FuncString() {
    }

    static JsonNode funcAbbreviate(final JsonNode node, final String params) {
        return applyWithParams(node, params, 1, 2, JsonNode::isTextual,
                (data, paramList) -> {
                    final JsonNode dataNode = data.getKey();
                    final JsonNode paramNode = data.getValue() < 0 ? dataNode : node;
                    int offset = paramList.get(0).isEmpty() ? 0 : getNodeAsInt(paramNode, data.getValue(), paramList.get(0));
                    final int maxWidth;
                    if (paramList.size() > 1) {
                        maxWidth = getNodeAsInt(paramNode, data.getValue(), paramList.get(1));
                    } else {
                        maxWidth = offset;
                        offset = 0;
                    }
                    return TextNode.valueOf(StringUtils.abbreviate(dataNode.asText(), offset, maxWidth));
                });
    }

    static JsonNode funcAppend(final JsonNode node, final String params) {
        return applyTextNodeWithParamAsText(node, params, (str, param) -> str + param);
    }

    static JsonNode funcAppendIfMissing(final JsonNode node, final String params, final boolean ignoreCase) {
        return applyTextNodeWithParamAsText(node, params,
                (str, param) -> ignoreCase
                        ? StringUtils.appendIfMissingIgnoreCase(str, param)
                        : StringUtils.appendIfMissing(str, param)
        );
    }

    static JsonNode funcCapitalize(final JsonNode node, final String params) {
        return applyTextNode(node, params, jsonNode -> StringUtils.capitalize(jsonNode.asText()));
    }

    static JsonNode funcConcat(final JsonNode node, final String params, final boolean notNull) {
        return applyWithParams(node, params, 1, UNLIMITED_AND_NO_PATH, null,
                (data, paramList) -> {
                    final StringBuilder sb = new StringBuilder();
                    for (String path : paramList) {
                        final JsonNode tryNode = getNodeByPath(node, data.getValue(), path);
                        if (nodeHasValue(tryNode)) {
                            sb.append(tryNode.asText());
                        } else if (notNull) {
                            return null;
                        }
                    }
                    return TextNode.valueOf(sb.toString());
                });
    }

    static JsonNode funcDoubleQuote(final JsonNode node, final String params) {
        return applyWithoutParam(node, params, JossonCore::nodeHasValue,
                (data, paramList) -> TextNode.valueOf(String.format("\"%s\"",
                        StringUtils.replace(data.getKey().asText(), "\"", "\\\"", -1, false)))
        );
    }

    static JsonNode funcKeep(final JsonNode node, final String params,
                             final boolean ignoreCase, final boolean after, final boolean last) {
        return applyTextNodeWithParamAsText(node, params,
                (str, param) -> {
                    final int pos = last
                        ? (ignoreCase ? StringUtils.lastIndexOfIgnoreCase(str, param) : StringUtils.lastIndexOf(str, param))
                        : (ignoreCase ? StringUtils.indexOfIgnoreCase(str, param) : StringUtils.indexOf(str, param));
                    return pos < 0 ? EMPTY
                        : after ? str.substring(pos + param.length())
                        : str.substring(0, pos);
                });
    }

    static JsonNode funcLength(final JsonNode node, final String params) {
        return applyWithoutParam(node, params, JossonCore::nodeHasValue,
                (data, paramList) -> IntNode.valueOf(data.getKey().asText().length()));
    }

    static JsonNode funcLowerCase(final JsonNode node, final String params) {
        return applyTextNode(node, params, jsonNode -> StringUtils.lowerCase(jsonNode.asText()));
    }

    static JsonNode funcNotBlankOrEmpty(final JsonNode node, final String params,
                                        final Function<CharSequence, Boolean> transform) {
        return applyWithParams(node, params, 1, UNLIMITED_AND_NO_PATH, null,
                (data, paramList) -> {
                    final JsonNode dataNode = data.getKey();
                    if (dataNode.isTextual() && transform.apply(dataNode.asText())) {
                        return dataNode;
                    }
                    for (String path : paramList) {
                        final JsonNode tryNode = getNodeByPath(node, data.getValue(), path);
                        if (tryNode != null && tryNode.isTextual() && transform.apply(tryNode.asText())) {
                            return tryNode;
                        }
                    }
                    return null;
                });
    }

    static JsonNode funcPadding(final JsonNode node, final String params, final int alignment) {
        return applyWithParams(node, params, 1, 2, JossonCore::nodeHasValue,
                (data, paramList) -> {
                    final JsonNode dataNode = data.getKey();
                    final JsonNode paramNode = data.getValue() < 0 ? dataNode : node;
                    final int size = getNodeAsInt(paramNode, data.getValue(), paramList.get(0));
                    final String padStr = paramList.size() > 1 ? getNodeAsText(paramNode, data.getValue(), paramList.get(1)) : null;
                    return TextNode.valueOf(
                            alignment < 0 ? StringUtils.leftPad(dataNode.asText(), size, padStr)
                            : alignment > 0 ? StringUtils.rightPad(dataNode.asText(), size, padStr)
                            : StringUtils.center(dataNode.asText(), size, padStr)
                    );
                });
    }

    static JsonNode funcPrepend(final JsonNode node, final String params) {
        return applyTextNodeWithParamAsText(node, params, (str, param) -> param + str);
    }

    static JsonNode funcPrependIfMissing(final JsonNode node, final String params, final boolean ignoreCase) {
        return applyTextNodeWithParamAsText(node, params,
                (str, param) -> ignoreCase
                        ? StringUtils.prependIfMissingIgnoreCase(str, param)
                        : StringUtils.prependIfMissing(str, param)
        );
    }

    static JsonNode funcRemoveEnd(final JsonNode node, final String params, final boolean ignoreCase) {
        return applyTextNodeWithParamAsText(node, params,
                (str, param) -> ignoreCase
                        ? StringUtils.removeEndIgnoreCase(str, param)
                        : StringUtils.removeEnd(str, param)
        );
    }

    static JsonNode funcRemoveStart(final JsonNode node, final String params, final boolean ignoreCase) {
        return applyTextNodeWithParamAsText(node, params,
                (str, param) -> ignoreCase
                        ? StringUtils.removeStartIgnoreCase(str, param)
                        : StringUtils.removeStart(str, param)
        );
    }

    static JsonNode funcRepeat(final JsonNode node, final String params) {
        return applyWithParams(node, params, 1, 1, JossonCore::nodeHasValue,
                (data, paramList) -> {
                    final JsonNode dataNode = data.getKey();
                    final JsonNode paramNode = data.getValue() < 0 ? dataNode : node;
                    final int param = getNodeAsInt(paramNode, data.getValue(), paramList.get(0));
                    return TextNode.valueOf(StringUtils.repeat(dataNode.asText(), param));
                });
    }

    static JsonNode funcReplace(final JsonNode node, final String params, final boolean ignoreCase) {
        return applyWithParams(node, params, 2, 3, JsonNode::isTextual,
                (data, paramList) -> {
                    final JsonNode dataNode = data.getKey();
                    final JsonNode paramNode = data.getValue() < 0 ? dataNode : node;
                    final String searchString = getNodeAsText(paramNode, data.getValue(), paramList.get(0));
                    final String replacement = getNodeAsText(paramNode, data.getValue(), paramList.get(1));
                    final int max = paramList.size() > 2 ? getNodeAsInt(paramNode, data.getValue(), paramList.get(2)) : -1;
                    return TextNode.valueOf(StringUtils.replace(dataNode.asText(), searchString, replacement, max, ignoreCase));
                });
    }

    static JsonNode funcSingleQuote(final JsonNode node, final String params) {
        return applyWithoutParam(node, params, JossonCore::nodeHasValue,
                (data, paramList) -> TextNode.valueOf(String.format("'%s'",
                        StringUtils.replace(data.getKey().asText(), "'", "''", -1, false)))
        );
    }

    static JsonNode funcSplit(final JsonNode node, final String params) {
        return applyWithParams(node, params, 0, 1, JsonNode::isTextual,
                (data, paramList) -> {
                    final JsonNode dataNode = data.getKey();
                    final JsonNode paramNode = data.getValue() < 0 ? dataNode : node;
                    final String separator = paramList.size() > 0 ? getNodeAsText(paramNode, data.getValue(), paramList.get(0)) : null;
                    final ArrayNode array = MAPPER.createArrayNode();
                    for (String text : StringUtils.split(dataNode.asText(), separator)) {
                        array.add(TextNode.valueOf(text));
                    }
                    return array;
                });
    }

    static JsonNode funcStrip(final JsonNode node, final String params) {
        return applyTextNodeWithParamAsText(node, params, StringUtils::strip);
    }

    static JsonNode funcStripEnd(final JsonNode node, final String params) {
        return applyTextNodeWithParamAsText(node, params, StringUtils::stripEnd);
    }

    static JsonNode funcStripStart(final JsonNode node, final String params) {
        return applyTextNodeWithParamAsText(node, params, StringUtils::stripStart);
    }

    static JsonNode funcSubstr(final JsonNode node, final String params) {
        return applyWithParams(node, params, 1, 2, JsonNode::isTextual,
                (data, paramList) -> {
                    final JsonNode dataNode = data.getKey();
                    final JsonNode paramNode = data.getValue() < 0 ? dataNode : node;
                    final int start = (paramList.get(0)).isEmpty() ? 0 : getNodeAsInt(paramNode, data.getValue(), paramList.get(0));
                    final int end = paramList.size() > 1 ? getNodeAsInt(paramNode, data.getValue(), paramList.get(1)) : Integer.MAX_VALUE;
                    return TextNode.valueOf(StringUtils.substring(dataNode.asText(), start, end));
                });
    }

    static JsonNode funcTrim(final JsonNode node, final String params) {
        return applyTextNode(node, params, jsonNode -> StringUtils.trim(jsonNode.asText()));
    }

    static JsonNode funcUncapitalize(final JsonNode node, final String params) {
        return applyTextNode(node, params, jsonNode -> StringUtils.uncapitalize(jsonNode.asText()));
    }

    static JsonNode funcUpperCase(final JsonNode node, final String params) {
        return applyTextNode(node, params, jsonNode -> StringUtils.upperCase(jsonNode.asText()));
    }
}

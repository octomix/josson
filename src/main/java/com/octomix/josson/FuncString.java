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

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static com.octomix.josson.FuncExecutor.*;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.PatternMatcher.decomposeFunctionParameters;

/**
 * String functions.
 */
class FuncString {

    private FuncString() {
    }

    static JsonNode funcAbbreviate(final JsonNode node, final String params) {
        return apply(node, params, 1, 2,
                paramList -> {
                    final int offset = paramList.get(0).isEmpty() ? 0 : getNodeAsInt(node, paramList.get(0));
                    if (paramList.size() <= 1) {
                        return new Integer[]{0, offset};
                    }
                    return new Integer[]{offset, getNodeAsInt(node, paramList.get(1))};
                },
                JsonNode::isTextual,
                (jsonNode, objVar) -> TextNode.valueOf(
                        StringUtils.abbreviate(jsonNode.asText(), ((Integer[]) objVar)[0], ((Integer[]) objVar)[1]))
        );
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

    static JsonNode funcCenter(final JsonNode node, final String params) {
        return applyTextAlignment(node, params,
                (jsonNode, objVar) -> {
                    final int size = (int) ((Pair<?, ?>) objVar).getKey();
                    final String padStr = (String) ((Pair<?, ?>) objVar).getValue();
                    return TextNode.valueOf(StringUtils.center(jsonNode.asText(), size, padStr));
                }
        );
    }

    static JsonNode funcConcat(final JsonNode node, final String params) {
        final List<String> paramList = decomposeFunctionParameters(params, 1, -1);
        final List<Pair<Character, String>> args = new ArrayList<>();
        for (String param : paramList) {
            if (param.isEmpty()) {
                continue;
            }
            if (param.charAt(0) == QUOTE_SYMBOL) {
                args.add(Pair.of(QUOTE_SYMBOL, unquoteString(param)));
            } else {
                args.add(Pair.of('.', param));
            }
        }
        if (!node.isArray()) {
            return TextNode.valueOf(funcConcat(node, args, -1));
        }
        final ArrayNode array = MAPPER.createArrayNode();
        for (int i = 0; i < node.size(); i++) {
            array.add(funcConcat(node, args, i));
        }
        return array;
    }

    private static String funcConcat(final JsonNode node, final List<Pair<Character, String>> args, final int index) {
        final StringBuilder sb = new StringBuilder();
        for (Pair<Character, String> arg : args) {
            if (arg.getKey() == QUOTE_SYMBOL) {
                sb.append(arg.getValue());
                continue;
            }
            final JsonNode tryNode = getNodeByPath(node, index, arg.getValue());
            if (!nodeHasValue(tryNode)) {
                return null;
            }
            sb.append(tryNode.asText());
        }
        return sb.toString();
    }

    static JsonNode funcKeep(final JsonNode node, final String params,
                             final boolean ignoreCase, final boolean after, final boolean last) {
        return applyTextNodeWithParamAsText(node, params,
                (str, param) -> {
                    final int pos = last
                            ? (ignoreCase
                                ? StringUtils.lastIndexOfIgnoreCase(str, param)
                                : StringUtils.lastIndexOf(str, param))
                            : (ignoreCase
                                ? StringUtils.indexOfIgnoreCase(str, param)
                                : StringUtils.indexOf(str, param));
                    return pos < 0 ? "" : after ? str.substring(pos + param.length()) : str.substring(0, pos);
                }
        );
    }

    static JsonNode funcLeftPad(final JsonNode node, final String params) {
        return applyTextAlignment(node, params,
                (jsonNode, objVar) -> {
                    final int size = (int) ((Pair<?, ?>) objVar).getKey();
                    final String padStr = (String) ((Pair<?, ?>) objVar).getValue();
                    return TextNode.valueOf(StringUtils.leftPad(jsonNode.asText(), size, padStr));
                }
        );
    }

    static JsonNode funcLength(final JsonNode node, final String params) {
        return applyWithoutParam(node, params, JossonCore::nodeHasValue,
                jsonNode -> IntNode.valueOf(jsonNode.asText().length()));
    }

    static JsonNode funcLowerCase(final JsonNode node, final String params) {
        return applyTextNode(node, params, jsonNode -> StringUtils.lowerCase(jsonNode.asText()));
    }

    static JsonNode funcNotBlankOrEmpty(final JsonNode node, final String params,
                                        final Function<CharSequence, Boolean> transform) {
        final List<String> paramList = decomposeFunctionParameters(params, 1, -1);
        if (node.isArray()) {
            final ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                array.add(funcNotBlankOrEmpty(node.get(i), paramList, transform));
            }
            return array;
        }
        return funcNotBlankOrEmpty(node, paramList, transform);
    }

    private static JsonNode funcNotBlankOrEmpty(final JsonNode node, final List<String> paramList,
                                                final Function<CharSequence, Boolean> transform) {
        if (node.isValueNode()) {
            if (node.isTextual() && transform.apply(node.asText())) {
                return node;
            }
            for (String path : paramList) {
                if (path.charAt(0) == QUOTE_SYMBOL) {
                    final String text = unquoteString(path);
                    if (transform.apply(text)) {
                        return TextNode.valueOf(text);
                    }
                }
            }
        } else if (node.isObject()) {
            for (String path : paramList) {
                final JsonNode tryNode = getNodeByPath(node, path);
                if (tryNode != null && tryNode.isTextual() && transform.apply(tryNode.asText())) {
                    return tryNode;
                }
            }
        }
        return null;
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
        return applyWithParamAsInt(node, params, JossonCore::nodeHasValue,
                (jsonNode, objVar) -> TextNode.valueOf(StringUtils.repeat(jsonNode.asText(), (int) objVar)));
    }

    static JsonNode funcReplace(final JsonNode node, final String params, final boolean ignoreCase) {
        return apply(node, params, 2, 3,
                paramList -> Pair.of(
                        new String[]{getNodeAsText(node, paramList.get(0)), getNodeAsText(node, paramList.get(1))},
                        paramList.size() > 2 ? getNodeAsInt(node, paramList.get(2)) : -1),
                JsonNode::isTextual,
                (jsonNode, objVar) -> {
                    final String[] texts = (String[]) ((Pair<?, ?>) objVar).getKey();
                    final int max = (int) ((Pair<?, ?>) objVar).getValue();
                    return TextNode.valueOf(StringUtils.replace(jsonNode.asText(), texts[0], texts[1], max, ignoreCase));
                }
        );
    }

    static JsonNode funcRightPad(final JsonNode node, final String params) {
        return applyTextAlignment(node, params,
                (jsonNode, objVar) -> {
                    final int size = (int) ((Pair<?, ?>) objVar).getKey();
                    final String padStr = (String) ((Pair<?, ?>) objVar).getValue();
                    return TextNode.valueOf(StringUtils.rightPad(jsonNode.asText(), size, padStr));
                }
        );
    }

    static JsonNode funcSplit(final JsonNode node, final String params) {
        return apply(node, params, 0, 1,
                paramList -> paramList.size() > 0 ? getNodeAsText(node, paramList.get(0)) : null,
                JsonNode::isTextual,
                (jsonNode, objVar) -> {
                    final ArrayNode array = MAPPER.createArrayNode();
                    for (String text : StringUtils.split(jsonNode.asText(), (String) objVar)) {
                        array.add(TextNode.valueOf(text));
                    }
                    return array;
                }
        );
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
        return apply(node, params, 1, 2,
                paramList -> {
                    final int int1 = paramList.get(0).isEmpty() ? 0 : getNodeAsInt(node, paramList.get(0));
                    final int int2 = paramList.size() > 1 ? getNodeAsInt(node, paramList.get(1)) : Integer.MAX_VALUE;
                    return new Integer[]{int1, int2};
                },
                JsonNode::isTextual,
                (jsonNode, objVar) -> TextNode.valueOf(
                        StringUtils.substring(jsonNode.asText(), ((Integer[]) objVar)[0], ((Integer[]) objVar)[1]))
        );
    }

    static JsonNode funcTrim(final JsonNode node, final String params) {
        return applyTextNode(node, params, jsonNode -> StringUtils.trim(jsonNode.asText()));
    }

    static JsonNode funcUncapitalize(final JsonNode node, final String params) {
        return applyTextNode(node, params, jsonNode -> StringUtils.uncapitalize(jsonNode.asText()));
    }

    static JsonNode funcUpperCase(final JsonNode node, final String params) {
        return applyTextNode(node, params,jsonNode -> StringUtils.upperCase(jsonNode.asText()));
    }
}

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

import static com.octomix.josson.FuncExecutor.applyFunc;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.PatternMatcher.decomposeFunctionParameters;

class FuncString {
    static JsonNode funcAbbreviate(JsonNode node, String params) {
        return applyFunc(node, params, 1, 2,
                paramList -> {
                    int offset = paramList.get(0).isEmpty() ? 0 : getNodeAsInt(node, paramList.get(0));
                    int maxWidth;
                    if (paramList.size() > 1) {
                        maxWidth = getNodeAsInt(node, paramList.get(1));
                    } else {
                        maxWidth = offset;
                        offset = 0;
                    }
                    return new Integer[]{offset, maxWidth};
                },
                JsonNode::isTextual,
                (jsonNode, objVar) -> TextNode.valueOf(
                        StringUtils.abbreviate(jsonNode.asText(), ((Integer[]) objVar)[0], ((Integer[]) objVar)[1]))
        );
    }

    static JsonNode funcAppendIfMissing(JsonNode node, String params, boolean ignoreCase) {
        return applyFunc(node, params, 1, 1,
                paramList -> getNodeAsText(node, paramList.get(0)),
                JsonNode::isTextual,
                (jsonNode, objVar) -> TextNode.valueOf(ignoreCase ?
                        StringUtils.appendIfMissingIgnoreCase(jsonNode.asText(), (String) objVar) :
                        StringUtils.appendIfMissing(jsonNode.asText(), (String) objVar))
        );
    }

    static JsonNode funcCapitalize(JsonNode node, String params) {
        return applyFunc(node, params,
                JsonNode::isTextual,
                jsonNode -> TextNode.valueOf(StringUtils.capitalize(jsonNode.asText()))
        );
    }

    static JsonNode funcCenter(JsonNode node, String params) {
        return applyFunc(node, params, 1, 2,
                paramList -> {
                    int size = getNodeAsInt(node, paramList.get(0));
                    String padStr = paramList.size() > 1 ? getNodeAsText(node, paramList.get(1)) : null;
                    return Pair.of(size, padStr);
                },
                JossonCore::nodeHasValue,
                (jsonNode, objVar) -> {
                    int size = (int) ((Pair<?,?>) objVar).getKey();
                    String padStr = (String) ((Pair<?,?>) objVar).getValue();
                    return TextNode.valueOf(StringUtils.center(jsonNode.asText(), size, padStr));
                }
        );
    }

    static JsonNode funcConcat(JsonNode node, String params) {
        List<String> paramList = decomposeFunctionParameters(params, 1, -1);
        List<Pair<Character, String>> args = new ArrayList<>();
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
            return TextNode.valueOf(funcConcatElement(node, args, -1));
        }
        ArrayNode array = MAPPER.createArrayNode();
        for (int i  = 0; i < node.size(); i++) {
            array.add(funcConcatElement(node, args, i));
        }
        return array;
    }

    private static String funcConcatElement(JsonNode node, List<Pair<Character, String>> args, int index) {
        StringBuilder sb = new StringBuilder();
        for (Pair<Character, String> arg : args) {
            if (arg.getKey() == QUOTE_SYMBOL) {
                sb.append(arg.getValue());
                continue;
            }
            JsonNode tryNode = getNodeByPath(node, index, arg.getValue());
            if (!nodeHasValue(tryNode)) {
                return null;
            }
            sb.append(tryNode.asText());
        }
        return sb.toString();
    }

    static JsonNode funcKeepAfter(JsonNode node, String params, boolean ignoreCase, boolean last) {
        return applyFunc(node, params, 1, 1,
                paramList -> getNodeAsText(node, paramList.get(0)),
                JsonNode::isTextual,
                (jsonNode, objVar) -> {
                    String find = (String) objVar;
                    if (find.isEmpty()) {
                        return jsonNode;
                    }
                    String text = jsonNode.asText();
                    int pos = last ?
                            (ignoreCase ?
                                    StringUtils.lastIndexOfIgnoreCase(text, find) :
                                    StringUtils.lastIndexOf(text, find)) :
                            (ignoreCase ?
                                    StringUtils.indexOfIgnoreCase(text, find) :
                                    StringUtils.indexOf(text, find));
                    return TextNode.valueOf(pos < 0 ? "" : text.substring(pos + find.length()));
                }
        );
    }

    static JsonNode funcKeepBefore(JsonNode node, String params, boolean ignoreCase, boolean last) {
        return applyFunc(node, params, 1, 1,
                paramList -> getNodeAsText(node, paramList.get(0)),
                JsonNode::isTextual,
                (jsonNode, objVar) -> {
                    String find = (String) objVar;
                    if (find.isEmpty()) {
                        return jsonNode;
                    }
                    String text = jsonNode.asText();
                    int pos = last ?
                            (ignoreCase ?
                                    StringUtils.lastIndexOfIgnoreCase(text, find) :
                                    StringUtils.lastIndexOf(text, find)) :
                            (ignoreCase ?
                                    StringUtils.indexOfIgnoreCase(text, find) :
                                    StringUtils.indexOf(text, find));
                    return TextNode.valueOf(pos < 0 ? "" : text.substring(0, pos));
                }
        );
    }

    static JsonNode funcLeftPad(JsonNode node, String params) {
        return applyFunc(node, params, 1, 2,
                paramList -> {
                    int size = getNodeAsInt(node, paramList.get(0));
                    String padStr = paramList.size() > 1 ? getNodeAsText(node, paramList.get(1)) : null;
                    return Pair.of(size, padStr);
                },
                JossonCore::nodeHasValue,
                (jsonNode, objVar) -> {
                    int size = (int) ((Pair<?,?>) objVar).getKey();
                    String padStr = (String) ((Pair<?,?>) objVar).getValue();
                    return TextNode.valueOf(StringUtils.leftPad(jsonNode.asText(), size, padStr));
                }
        );
    }

    static JsonNode funcLength(JsonNode node, String params) {
        return applyFunc(node, params,
                JossonCore::nodeHasValue,
                jsonNode -> IntNode.valueOf(jsonNode.asText().length())
        );
    }

    static JsonNode funcLowerCase(JsonNode node, String params) {
        return applyFunc(node, params,
                JsonNode::isTextual,
                jsonNode -> TextNode.valueOf(StringUtils.lowerCase(jsonNode.asText()))
        );
    }

    static JsonNode funcNotBlank(JsonNode node, String params) {
        List<String> paramList = decomposeFunctionParameters(params, 1, -1);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                array.add(funcNotBlank(node.get(i), paramList));
            }
            return array;
        }
        return funcNotBlank(node, paramList);
    }

    static JsonNode funcNotBlank(JsonNode node, List<String> paramList) {
        if (node.isValueNode()) {
            if (node.isTextual() && StringUtils.isNotBlank(node.asText())) {
                return node;
            }
            for (String path : paramList) {
                if (path.charAt(0) == QUOTE_SYMBOL) {
                    String text = unquoteString(path);
                    if (StringUtils.isNotBlank(text)) {
                        return TextNode.valueOf(text);
                    }
                }
            }
        } else if (node.isObject()) {
            for (String path : paramList) {
                JsonNode tryNode = getNodeByPath(node, path);
                if (tryNode != null && tryNode.isTextual() && StringUtils.isNotBlank(tryNode.asText())) {
                    return tryNode;
                }
            }
        }
        return null;
    }

    static JsonNode funcNotEmpty(JsonNode node, String params) {
        List<String> paramList = decomposeFunctionParameters(params, 1, -1);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                array.add(funcNotEmpty(node.get(i), paramList));
            }
            return array;
        }
        return funcNotEmpty(node, paramList);
    }

    static JsonNode funcNotEmpty(JsonNode node, List<String> paramList) {
        if (node.isValueNode()) {
            if (node.isTextual() && StringUtils.isNotEmpty(node.asText())) {
                return node;
            }
            for (String path : paramList) {
                if (path.charAt(0) == QUOTE_SYMBOL) {
                    String text = unquoteString(path);
                    if (StringUtils.isNotEmpty(text)) {
                        return TextNode.valueOf(text);
                    }
                }
            }
        } else if (node.isObject()) {
            for (String path : paramList) {
                JsonNode tryNode = getNodeByPath(node, path);
                if (tryNode != null && tryNode.isTextual() && StringUtils.isNotEmpty(tryNode.asText())) {
                    return tryNode;
                }
            }
        }
        return null;
    }

    static JsonNode funcPrependIfMissing(JsonNode node, String params, boolean ignoreCase) {
        return applyFunc(node, params, 1, 1,
                paramList -> getNodeAsText(node, paramList.get(0)),
                JsonNode::isTextual,
                (jsonNode, objVar) -> TextNode.valueOf(ignoreCase ?
                        StringUtils.prependIfMissingIgnoreCase(jsonNode.asText(), (String) objVar) :
                        StringUtils.prependIfMissing(jsonNode.asText(), (String) objVar))
        );
    }

    static JsonNode funcRemoveEnd(JsonNode node, String params, boolean ignoreCase) {
        return applyFunc(node, params, 1, 1,
                paramList -> getNodeAsText(node, paramList.get(0)),
                JsonNode::isTextual,
                (jsonNode, objVar) -> TextNode.valueOf(ignoreCase ?
                        StringUtils.removeEndIgnoreCase(jsonNode.asText(), (String) objVar) :
                        StringUtils.removeEnd(jsonNode.asText(), (String) objVar))
        );
    }

    static JsonNode funcRemoveStart(JsonNode node, String params, boolean ignoreCase) {
        return applyFunc(node, params, 1, 1,
                paramList -> getNodeAsText(node, paramList.get(0)),
                JsonNode::isTextual,
                (jsonNode, objVar) -> TextNode.valueOf(ignoreCase ?
                        StringUtils.removeStartIgnoreCase(jsonNode.asText(), (String) objVar) :
                        StringUtils.removeStart(jsonNode.asText(), (String) objVar))
        );
    }

    static JsonNode funcRepeat(JsonNode node, String params) {
        return applyFunc(node, params, 1, 1,
                paramList -> getNodeAsInt(node, paramList.get(0)),
                JossonCore::nodeHasValue,
                (jsonNode, objVar) -> TextNode.valueOf(StringUtils.repeat(jsonNode.asText(), (int) objVar))
        );
    }

    static JsonNode funcReplace(JsonNode node, String params, boolean ignoreCase) {
        return applyFunc(node, params, 2, 3,
                paramList -> Pair.of(
                        new String[]{getNodeAsText(node, paramList.get(0)), getNodeAsText(node, paramList.get(1))},
                        paramList.size() > 2 ? getNodeAsInt(node, paramList.get(2)) : -1),
                JsonNode::isTextual,
                (jsonNode, objVar) -> {
                    String[] texts = (String[]) ((Pair<?,?>) objVar).getKey();
                    int max = (int) ((Pair<?,?>) objVar).getValue();
                    return TextNode.valueOf(StringUtils.replace(jsonNode.asText(), texts[0], texts[1], max, ignoreCase));
                }
        );
    }

    static JsonNode funcRightPad(JsonNode node, String params) {
        return applyFunc(node, params, 1, 2,
                paramList -> {
                    int size = getNodeAsInt(node, paramList.get(0));
                    String padStr = paramList.size() > 1 ? getNodeAsText(node, paramList.get(1)) : null;
                    return Pair.of(size, padStr);
                },
                JossonCore::nodeHasValue,
                (jsonNode, objVar) -> {
                    int size = (int) ((Pair<?,?>) objVar).getKey();
                    String padStr = (String) ((Pair<?,?>) objVar).getValue();
                    return TextNode.valueOf(StringUtils.rightPad(jsonNode.asText(), size, padStr));
                }
        );
    }

    static JsonNode funcSplit(JsonNode node, String params) {
        return applyFunc(node, params, 0, 1,
                paramList -> paramList.size() > 0 ? getNodeAsText(node, paramList.get(0)) : null,
                JsonNode::isTextual,
                (jsonNode, objVar) -> {
                    ArrayNode array = MAPPER.createArrayNode();
                    for (String text : StringUtils.split(jsonNode.asText(), (String) objVar)) {
                        array.add(TextNode.valueOf(text));
                    }
                    return array;
                }
        );
    }

    static JsonNode funcStrip(JsonNode node, String params) {
        return applyFunc(node, params, 1, 1,
                paramList -> getNodeAsText(node, paramList.get(0)),
                JsonNode::isTextual,
                (jsonNode, objVar) -> TextNode.valueOf(StringUtils.strip(jsonNode.asText(), (String) objVar))
        );
    }

    static JsonNode funcStripEnd(JsonNode node, String params) {
        return applyFunc(node, params, 1, 1,
                paramList -> getNodeAsText(node, paramList.get(0)),
                JsonNode::isTextual,
                (jsonNode, objVar) -> TextNode.valueOf(StringUtils.stripEnd(jsonNode.asText(), (String) objVar))
        );
    }

    static JsonNode funcStripStart(JsonNode node, String params) {
        return applyFunc(node, params, 1, 1,
                paramList -> getNodeAsText(node, paramList.get(0)),
                JsonNode::isTextual,
                (jsonNode, objVar) -> TextNode.valueOf(StringUtils.stripStart(jsonNode.asText(), (String) objVar))
        );
    }

    static JsonNode funcSubstr(JsonNode node, String params) {
        return applyFunc(node, params, 1, 2,
                paramList -> {
                    int int1 = paramList.get(0).isEmpty() ? 0 : getNodeAsInt(node, paramList.get(0));
                    int int2 = paramList.size() > 1 ? getNodeAsInt(node, paramList.get(1)) : Integer.MAX_VALUE;
                    return new Integer[]{int1, int2};
                },
                JsonNode::isTextual,
                (jsonNode, objVar) -> TextNode.valueOf(
                        StringUtils.substring(jsonNode.asText(), ((Integer[]) objVar)[0], ((Integer[]) objVar)[1]))
        );
    }

    static JsonNode funcTrim(JsonNode node, String params) {
        return applyFunc(node, params,
                JsonNode::isTextual,
                jsonNode -> TextNode.valueOf(StringUtils.trim(jsonNode.asText()))
        );
    }

    static JsonNode funcUncapitalize(JsonNode node, String params) {
        return applyFunc(node, params,
                JsonNode::isTextual,
                jsonNode -> TextNode.valueOf(StringUtils.uncapitalize(jsonNode.asText()))
        );
    }

    static JsonNode funcUpperCase(JsonNode node, String params) {
        return applyFunc(node, params,
                JsonNode::isTextual,
                jsonNode -> TextNode.valueOf(StringUtils.upperCase(jsonNode.asText()))
        );
    }
}

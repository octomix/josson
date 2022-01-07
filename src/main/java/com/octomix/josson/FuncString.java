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
import com.fasterxml.jackson.databind.node.IntNode;
import com.fasterxml.jackson.databind.node.TextNode;
import com.octomix.josson.commons.StringUtils;

import java.util.ArrayList;
import java.util.List;

import static com.octomix.josson.GetFuncParam.*;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.PatternMatcher.decomposeFunctionParameters;

class FuncString {
    static JsonNode funcAbbreviate(JsonNode node, String params) {
        Pair<String, Integer[]> pathAndParams = getParamPathAndStartEnd(params);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        int offset = pathAndParams.getValue()[0];
        int maxWidth;
        if (pathAndParams.getValue()[1] < Integer.MAX_VALUE) {
            maxWidth = pathAndParams.getValue()[1];
        } else {
            maxWidth = offset;
            offset = 0;
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(StringUtils.abbreviate(textNode.asText(), offset, maxWidth)));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.abbreviate(node.asText(), offset, maxWidth));
    }

    static JsonNode funcAppendIfMissing(JsonNode node, String params, boolean ignoreCase) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        String suffix = getNodeAsText(node, pathAndParams.getValue().get(0));
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(ignoreCase ?
                            StringUtils.appendIfMissingIgnoreCase(textNode.asText(), suffix) :
                            StringUtils.appendIfMissing(textNode.asText(), suffix)));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(ignoreCase ?
                StringUtils.appendIfMissingIgnoreCase(node.asText(), suffix) :
                StringUtils.appendIfMissing(node.asText(), suffix));
    }

    static JsonNode funcCapitalize(JsonNode node, String params) {
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
                    array.add(TextNode.valueOf(StringUtils.capitalize(textNode.asText())));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.capitalize(node.asText()));
    }

    static JsonNode funcCenter(JsonNode node, String params) {
        Triple<String, Integer, String> pathAndParams = getParamPathAndAlignment(params);
        if (pathAndParams.getLeft() != null) {
            node = getNodeByPath(node, pathAndParams.getLeft());
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (nodeHasValue(textNode)) {
                    array.add(TextNode.valueOf(StringUtils.center(
                            textNode.asText(), pathAndParams.getMiddle(), pathAndParams.getRight())));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!nodeHasValue(node)) {
            return null;
        }
        return TextNode.valueOf(StringUtils.center(node.asText(), pathAndParams.getMiddle(), pathAndParams.getRight()));
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
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        String find = getNodeAsText(node, pathAndParams.getValue().get(0));
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    if (find.isEmpty()) {
                        array.add(textNode);
                    } else {
                        String text = textNode.asText();
                        int pos = last ?
                                (ignoreCase ?
                                        StringUtils.lastIndexOfIgnoreCase(text, find) :
                                        StringUtils.lastIndexOf(text, find)) :
                                (ignoreCase ?
                                        StringUtils.indexOfIgnoreCase(text, find) :
                                        StringUtils.indexOf(text, find));
                        array.add(pos < 0 ? "" : text.substring(pos + find.length()));
                    }
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        if (find.isEmpty()) {
            return node;
        }
        String text = node.asText();
        int pos = last ?
                (ignoreCase ?
                        StringUtils.lastIndexOfIgnoreCase(text, find) :
                        StringUtils.lastIndexOf(text, find)) :
                (ignoreCase ?
                        StringUtils.indexOfIgnoreCase(text, find) :
                        StringUtils.indexOf(text, find));
        return TextNode.valueOf(pos < 0 ? "" : text.substring(pos + find.length()));
    }

    static JsonNode funcKeepBefore(JsonNode node, String params, boolean ignoreCase, boolean last) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        String find = getNodeAsText(node, pathAndParams.getValue().get(0));
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    String text = textNode.asText();
                    if (find.isEmpty()) {
                        array.add(text);
                    } else {
                        int pos = last ?
                                (ignoreCase ?
                                        StringUtils.lastIndexOfIgnoreCase(text, find) :
                                        StringUtils.lastIndexOf(text, find)) :
                                (ignoreCase ?
                                        StringUtils.indexOfIgnoreCase(text, find) :
                                        StringUtils.indexOf(text, find));
                        array.add(pos < 0 ? "" : text.substring(0, pos));
                    }
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        if (find.isEmpty()) {
            return node;
        }
        String text = node.asText();
        int pos = last ?
                (ignoreCase ?
                        StringUtils.lastIndexOfIgnoreCase(text, find) :
                        StringUtils.lastIndexOf(text, find)) :
                (ignoreCase ?
                        StringUtils.indexOfIgnoreCase(text, find) :
                        StringUtils.indexOf(text, find));
        return TextNode.valueOf(pos < 0 ? "" : text.substring(0, pos));
    }

    static JsonNode funcLeftPad(JsonNode node, String params) {
        Triple<String, Integer, String> pathAndParams = getParamPathAndAlignment(params);
        if (pathAndParams.getLeft() != null) {
            node = getNodeByPath(node, pathAndParams.getLeft());
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (nodeHasValue(textNode)) {
                    array.add(TextNode.valueOf(StringUtils.leftPad(
                            textNode.asText(), pathAndParams.getMiddle(), pathAndParams.getRight())));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!nodeHasValue(node)) {
            return null;
        }
        return TextNode.valueOf(StringUtils.leftPad(node.asText(), pathAndParams.getMiddle(), pathAndParams.getRight()));
    }

    static JsonNode funcLength(JsonNode node, String params) {
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
                if (textNode.isValueNode()) {
                    array.add(IntNode.valueOf(textNode.asText().length()));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!node.isValueNode()) {
            return null;
        }
        return IntNode.valueOf(node.asText().length());
    }

    static JsonNode funcLowerCase(JsonNode node, String params) {
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
                    array.add(TextNode.valueOf(StringUtils.lowerCase(textNode.asText())));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.lowerCase(node.asText()));
    }

    static JsonNode funcPrependIfMissing(JsonNode node, String params, boolean ignoreCase) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        String prefix = getNodeAsText(node, pathAndParams.getValue().get(0));
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(ignoreCase ?
                            StringUtils.prependIfMissingIgnoreCase(textNode.asText(), prefix) :
                            StringUtils.prependIfMissing(textNode.asText(), prefix)));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(ignoreCase ?
                StringUtils.prependIfMissingIgnoreCase(node.asText(), prefix) :
                StringUtils.prependIfMissing(node.asText(), prefix));
    }

    static JsonNode funcRemoveEnd(JsonNode node, String params, boolean ignoreCase) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        String remove = getNodeAsText(node, pathAndParams.getValue().get(0));
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(ignoreCase ?
                            StringUtils.removeEndIgnoreCase(textNode.asText(), remove) :
                            StringUtils.removeEnd(textNode.asText(), remove)));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(ignoreCase ?
                StringUtils.removeEndIgnoreCase(node.asText(), remove) :
                StringUtils.removeEnd(node.asText(), remove));
    }

    static JsonNode funcRemoveStart(JsonNode node, String params, boolean ignoreCase) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        String remove = getNodeAsText(node, pathAndParams.getValue().get(0));
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(ignoreCase ?
                            StringUtils.removeStartIgnoreCase(textNode.asText(), remove) :
                            StringUtils.removeStart(textNode.asText(), remove)));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(ignoreCase ?
                StringUtils.removeStartIgnoreCase(node.asText(), remove) :
                StringUtils.removeStart(node.asText(), remove));
    }

    static JsonNode funcRepeat(JsonNode node, String params) {
        Pair<String, Integer> pathAndParams = getParamPathAndInt(params);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        int repeat = pathAndParams.getValue() == null ? 0 : pathAndParams.getValue();
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (nodeHasValue(textNode)) {
                    array.add(TextNode.valueOf(StringUtils.repeat(textNode.asText(), repeat)));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!nodeHasValue(node)) {
            return null;
        }
        return TextNode.valueOf(StringUtils.repeat(node.asText(), repeat));
    }

    static JsonNode funcReplace(JsonNode node, String params, boolean ignoreCase) {
        Triple<String, String[], Integer> pathAndParams = getParamPathAnd2StringAndInt(params);
        if (pathAndParams.getLeft() != null) {
            node = getNodeByPath(node, pathAndParams.getLeft());
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(StringUtils.replace(
                            textNode.asText(), pathAndParams.getMiddle()[0], pathAndParams.getMiddle()[1],
                            pathAndParams.getRight() == null ? -1 : pathAndParams.getRight(), ignoreCase)));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.replace(
                node.asText(), pathAndParams.getMiddle()[0], pathAndParams.getMiddle()[1],
                pathAndParams.getRight() == null ? -1 : pathAndParams.getRight(), ignoreCase));
    }

    static JsonNode funcRightPad(JsonNode node, String params) {
        Triple<String, Integer, String> pathAndParams = getParamPathAndAlignment(params);
        if (pathAndParams.getLeft() != null) {
            node = getNodeByPath(node, pathAndParams.getLeft());
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (nodeHasValue(textNode)) {
                    array.add(TextNode.valueOf(StringUtils.rightPad(
                            textNode.asText(), pathAndParams.getMiddle(), pathAndParams.getRight())));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!nodeHasValue(node)) {
            return null;
        }
        return TextNode.valueOf(StringUtils.rightPad(node.asText(), pathAndParams.getMiddle(), pathAndParams.getRight()));
    }

    static ArrayNode funcSplit(JsonNode node, String params) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 0, 1);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        String separator = pathAndParams.getValue().size() > 0 ? getNodeAsText(node, pathAndParams.getValue().get(0)) : null;
        ArrayNode array = MAPPER.createArrayNode();
        if (node.isArray()) {
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    for (String text : StringUtils.split(textNode.asText(), separator)) {
                        array.add(TextNode.valueOf(text));
                    }
                }
            }
        } else if (!node.isTextual()) {
            return null;
        } else {
            for (String text : StringUtils.split(node.asText(), separator)) {
                array.add(TextNode.valueOf(text));
            }
        }
        return array;
    }

    static JsonNode funcStrip(JsonNode node, String params) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 0, 1);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        String stripChars = pathAndParams.getValue().size() > 0 ? getNodeAsText(node, pathAndParams.getValue().get(0)) : null;
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(StringUtils.strip(textNode.asText(), stripChars)));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.strip(node.asText(), stripChars));
    }

    static JsonNode funcStripEnd(JsonNode node, String params) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 0, 1);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        String stripChars = pathAndParams.getValue().size() > 0 ? getNodeAsText(node, pathAndParams.getValue().get(0)) : null;
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(StringUtils.stripEnd(textNode.asText(), stripChars)));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.stripEnd(node.asText(), stripChars));
    }

    static JsonNode funcStripStart(JsonNode node, String params) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 0, 1);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        String stripChars = pathAndParams.getValue().size() > 0 ? getNodeAsText(node, pathAndParams.getValue().get(0)) : null;
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(StringUtils.stripStart(textNode.asText(), stripChars)));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.stripStart(node.asText(), stripChars));
    }

    static JsonNode funcSubstr(JsonNode node, String params) {
        Pair<String, Integer[]> pathAndParams = getParamPathAndStartEnd(params);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(StringUtils.substring(
                            textNode.asText(), pathAndParams.getValue()[0], pathAndParams.getValue()[1])));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.substring(node.asText(), pathAndParams.getValue()[0], pathAndParams.getValue()[1]));
    }

    static JsonNode funcTrim(JsonNode node, String params) {
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
                    array.add(TextNode.valueOf(StringUtils.trim(textNode.asText())));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.trim(node.asText()));
    }

    static JsonNode funcUncapitalize(JsonNode node, String params) {
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
                    array.add(TextNode.valueOf(StringUtils.uncapitalize(textNode.asText())));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.uncapitalize(node.asText()));
    }

    static JsonNode funcUpperCase(JsonNode node, String params) {
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
                    array.add(TextNode.valueOf(StringUtils.upperCase(textNode.asText())));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.upperCase(node.asText()));
    }
}

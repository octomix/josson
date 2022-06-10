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
import com.fasterxml.jackson.databind.node.BooleanNode;
import com.fasterxml.jackson.databind.node.IntNode;
import com.fasterxml.jackson.databind.node.TextNode;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UnknownFormatConversionException;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;

import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.PatternMatcher.*;

/**
 * Common logic to execute functions.
 */
final class FuncExecutor {

    private FuncExecutor() {
    }

    static String getParamPath(final String params) {
        final List<String> paramList = decomposeFunctionParameters(params, 0, 1);
        return paramList.isEmpty() ? null : paramList.get(0);
    }

    static Pair<String, List<String>> getParamPathAndStrings(final String params, final int min, final int max) {
        final List<String> paramList = decomposeFunctionParameters(params, min, max + 1);
        final String path = max < -2 ? "" : paramList.size() > max ? paramList.remove(0) : null;
        return Pair.of(path, paramList);
    }

    static Map<String, String> getParamNamePath(final List<String> paramList) {
        final Map<String, String> elements = new LinkedHashMap<>();
        int noNameCount = 0;
        for (String param : paramList) {
            Pair<String, String> namePath;
            try {
                namePath = decomposeNameAndPath(param);
            } catch (UnknownFormatConversionException e) {
                namePath = Pair.of(e.getConversion() + ++noNameCount, param);
            }
            elements.put(namePath.getKey(), namePath.getValue());
        }
        return elements;
    }

    static JsonNode getParamArrayOrItselfIsContainer(final String params, final JsonNode node) {
        final List<String> paramList = decomposeFunctionParameters(params, 0, -1);
        if (paramList.isEmpty()) {
            if (node.isContainerNode()) {
                return node;
            }
            return null;
        }
        return getParamArray(paramList, node);
    }

    static ArrayNode getParamArrayOrItself(final String params, final JsonNode node) {
        final List<String> paramList = decomposeFunctionParameters(params, 0, -1);
        if (paramList.isEmpty()) {
            if (node.isArray()) {
                return (ArrayNode) node;
            }
            return null;
        }
        return getParamArray(paramList, node);
    }

    private static ArrayNode getParamArray(final List<String> paramList, final JsonNode node) {
        final ArrayNode array = MAPPER.createArrayNode();
        for (String param : paramList) {
            if (node.isArray()) {
                for (int i = 0; i < node.size(); i++) {
                    final JsonNode tryNode = getNodeByPath(node, i, param);
                    if (tryNode != null) {
                        array.add(tryNode);
                    }
                }
            } else {
                final JsonNode tryNode = getNodeByPath(node, param);
                if (tryNode != null) {
                    if (tryNode.isArray()) {
                        array.addAll((ArrayNode) tryNode);
                    } else {
                        array.add(tryNode);
                    }
                }
            }
        }
        return array;
    }

    static JsonNode applyWithoutParam(JsonNode node, final String params, final Function<JsonNode, JsonNode> action) {
        final String path = getParamPath(params);
        if (path != null) {
            node = getNodeByPath(node, path);
            if (node == null) {
                return null;
            }
        }
        return action.apply(node);
    }

    static JsonNode applyWithoutParam(final JsonNode node, final String params, final Predicate<JsonNode> isValid,
                                      final BiFunction<Pair<JsonNode, Integer>, List<String>, JsonNode> action) {
        return applyAction(node, getParamPath(params), isValid, action, null);
    }

    static JsonNode applyWithParams(final JsonNode node, final String params, final int min, final int max,
                                    final Predicate<JsonNode> isValid,
                                    final BiFunction<Pair<JsonNode, Integer>, List<String>, JsonNode> action) {
        final Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, min, max);
        return applyAction(node, pathAndParams.getKey(), isValid, action, pathAndParams.getValue());
    }

    static JsonNode applyTextNode(final JsonNode node, final String params,
                                  final Function<JsonNode, String> transform) {
        return applyWithoutParam(node, params, JsonNode::isTextual,
                (data, paramList) -> TextNode.valueOf(transform.apply(data.getKey())));
    }

    static JsonNode applyTextNodeToInt(final JsonNode node, final String params,
                                       final Function<JsonNode, Integer> transform) {
        return applyWithoutParam(node, params, JsonNode::isTextual,
                (data, paramList) -> IntNode.valueOf(transform.apply(data.getKey())));
    }

    static JsonNode applyTextNodeWithParamAsText(final JsonNode node, final String params,
                                                 final BiFunction<String, String, String> transform) {
        return applyWithParams(node, params, 1, 1, JsonNode::isTextual,
                (data, paramList) -> {
                    final String param = getNodeAsText(node, data.getValue(), paramList.get(0));
                    return TextNode.valueOf(transform.apply(data.getKey().asText(), param));
                });
    }

    static JsonNode applyTextNodeWithParamAsText(final JsonNode node, final String params, final boolean not,
                                                 final BiFunction<String, String, Boolean> transform) {
        return applyWithParams(node, params, 1, 1, JsonNode::isTextual,
                (data, paramList) -> {
                    final String param = getNodeAsText(node, data.getValue(), paramList.get(0));
                    return BooleanNode.valueOf(not ^ transform.apply(data.getKey().asText(), param));
                });
    }

    private static JsonNode applyAction(final JsonNode node, final String path, final Predicate<JsonNode> isValid,
                                        final BiFunction<Pair<JsonNode, Integer>, List<String>, JsonNode> action,
                                        final List<String> paramList) {
        final JsonNode target = path == null ? node : getNodeByPath(node, path);
        if (target == null) {
            return null;
        }
        if (target.isArray()) {
            final ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < target.size(); i++) {
                array.add(applyAction(target.get(i), path == null ? -1 : i, isValid, action, paramList));
            }
            return array;
        }
        return applyAction(target, path == null ? -1 : 0, isValid, action, paramList);
    }

    private static JsonNode applyAction(final JsonNode node, final int index, final Predicate<JsonNode> isValid,
                                        final BiFunction<Pair<JsonNode, Integer>, List<String>, JsonNode> action,
                                        final List<String> paramList) {
        return isValid == null || isValid.test(node) ? action.apply(Pair.of(node, index), paramList) : null;
    }

    static JsonNode applyWithArrayNode(final JsonNode node, final String params, final Predicate<JsonNode> isValid,
                                       final BiFunction<JsonNode, ArrayNode, JsonNode> action) {
        final ArrayNode paramArray = getParamArray(decomposeFunctionParameters(params, 1, -1), node);
        if (paramArray.isEmpty()) {
            return null;
        }
        if (node.isArray()) {
            final ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                array.add(applyAction(node.get(i), isValid, action, paramArray));
            }
            return array;
        }
        return applyAction(node, isValid, action, paramArray);
    }

    private static JsonNode applyAction(final JsonNode node, final Predicate<JsonNode> isValid,
                                        final BiFunction<JsonNode, ArrayNode, JsonNode> action,
                                        final ArrayNode paramArray) {
        return isValid == null || isValid.test(node) ? action.apply(node, paramArray) : null;
    }
}

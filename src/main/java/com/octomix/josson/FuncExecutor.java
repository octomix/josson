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
import com.fasterxml.jackson.databind.node.*;

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
import static com.octomix.josson.Utils.addArrayElement;
import static com.octomix.josson.commons.StringUtils.EMPTY;

/**
 * Common logic to execute functions.
 */
final class FuncExecutor {

    /**
     * Unlimited number of parameters with optional data path.
     */
    static final int UNLIMITED_WITH_PATH = -1;

    /**
     * Unlimited number of parameters and not accept data path.
     */
    static final int UNLIMITED_AND_NO_PATH = -2;

    private FuncExecutor() {
    }

    static JsonNode getParamNode(final JsonNode node, final String params) {
        final List<String> paramList = decomposeFunctionParameters(params, 0, 1);
        return paramList.isEmpty() ? node : getNodeByPath(node, paramList.get(0));
    }

    static Pair<JsonNode, List<String>> getParamNodeAndStrings(final JsonNode node, final String params, final int min, final int max) {
        final List<String> paramList = decomposeFunctionParameters(params, min, max + 1);
        final String path = max <= UNLIMITED_AND_NO_PATH ? EMPTY : paramList.size() > max ? paramList.remove(0) : null;
        return Pair.of(path == null ? node : getNodeByPath(node, path), paramList);
    }

    static Map<String, String> getParamNamePath(final List<String> paramList) {
        final Map<String, String> elements = new LinkedHashMap<>();
        int noNameCount = 0;
        for (String param : paramList) {
            String[] namePath;
            try {
                namePath = decomposeNameAndPath(param);
            } catch (UnknownFormatConversionException e) {
                namePath = new String[]{e.getConversion() + ++noNameCount, param};
            }
            elements.put(namePath[0], namePath[1]);
        }
        return elements;
    }

    static JsonNode getParamArrayOrItselfIsContainer(final JsonNode node, final String params) {
        final List<String> paramList = decomposeFunctionParameters(params, 0, UNLIMITED_WITH_PATH);
        if (paramList.isEmpty()) {
            if (node.isContainerNode()) {
                return node;
            }
            return null;
        }
        return getParamArray(node, paramList);
    }

    static ArrayNode getParamArrayOrItself(final JsonNode node, final String params) {
        final List<String> paramList = decomposeFunctionParameters(params, 0, UNLIMITED_WITH_PATH);
        if (paramList.isEmpty()) {
            if (node.isArray()) {
                return (ArrayNode) node;
            }
            return null;
        }
        return getParamArray(node, paramList);
    }

    private static ArrayNode getParamArray(final JsonNode node, final List<String> paramList) {
        final ArrayNode array = MAPPER.createArrayNode();
        for (String param : paramList) {
            if (node.isArray()) {
                for (int i = 0; i < node.size(); i++) {
                    addArrayElement(array, getNodeByPath(node, i, param));
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

    static JsonNode applyWithoutParam(final JsonNode node, final String params, final Function<JsonNode, JsonNode> action) {
        final JsonNode workNode = getParamNode(node, params);
        return workNode == null ? null : action.apply(workNode);
    }

    static JsonNode applyWithoutParam(final JsonNode node, final String params, final Predicate<JsonNode> isValid,
                                      final BiFunction<Pair<JsonNode, Integer>, List<String>, JsonNode> action) {
        final List<String> paramList = decomposeFunctionParameters(params, 0, 1);
        return applyAction(node, paramList.isEmpty() ? null : paramList.get(0), isValid, action, null);
    }

    static JsonNode applyWithParams(final JsonNode node, final String params, final int min, final int max,
                                    final Predicate<JsonNode> isValid,
                                    final BiFunction<Pair<JsonNode, Integer>, List<String>, JsonNode> action) {
        final List<String> paramList = decomposeFunctionParameters(params, min, max + 1);
        final String path = max <= UNLIMITED_AND_NO_PATH ? EMPTY : paramList.size() > max ? paramList.remove(0) : null;
        return applyAction(node, path, isValid, action, paramList);
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

    static JsonNode applyTextNodeToLong(final JsonNode node, final String params,
                                        final Function<JsonNode, Long> transform) {
        return applyWithoutParam(node, params, JsonNode::isTextual,
                (data, paramList) -> LongNode.valueOf(transform.apply(data.getKey())));
    }

    static JsonNode applyNumberNodeToText(final JsonNode node, final String params,
                                          final Function<JsonNode, String> transform) {
        return applyWithoutParam(node, params, jsonNode -> jsonNode.isNumber() || jsonNode.isTextual(),
                (data, paramList) -> TextNode.valueOf(transform.apply(data.getKey())));
    }

    static JsonNode applyNumberNodeToInt(final JsonNode node, final String params,
                                         final Function<JsonNode, Integer> transform) {
        return applyWithoutParam(node, params, jsonNode -> jsonNode.isNumber() || jsonNode.isTextual(),
                (data, paramList) -> IntNode.valueOf(transform.apply(data.getKey())));
    }

    static JsonNode applyTextNodeWithParamAsText(final JsonNode node, final String params,
                                                 final BiFunction<String, String, String> transform) {
        return applyWithParams(node, params, 1, 1, JsonNode::isTextual,
            (data, paramList) -> TextNode.valueOf(
                transform.apply(data.getKey().asText(), getNodeAsText(node, data.getValue(), paramList.get(0)))
            ));
    }

    static JsonNode applyTextNodeWithParamAsText(final JsonNode node, final String params, final boolean not,
                                                 final BiFunction<String, String, Boolean> transform) {
        return applyWithParams(node, params, 1, 1, JsonNode::isTextual,
            (data, paramList) -> BooleanNode.valueOf(
                not ^ transform.apply(data.getKey().asText(), getNodeAsText(node, data.getValue(), paramList.get(0)))
            ));
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
                array.add(applyAction(target.get(i), i, isValid, action, paramList));
            }
            return array;
        }
        return applyAction(target, path == null ? NON_ARRAY_INDEX : 0, isValid, action, paramList);
    }

    private static JsonNode applyAction(final JsonNode node, final int index, final Predicate<JsonNode> isValid,
                                        final BiFunction<Pair<JsonNode, Integer>, List<String>, JsonNode> action,
                                        final List<String> paramList) {
        return isValid == null || isValid.test(node) ? action.apply(Pair.of(node, index), paramList) : null;
    }

    static JsonNode applyWithArrayNode(final JsonNode node, final String params, final Predicate<JsonNode> isValid,
                                       final BiFunction<JsonNode, ArrayNode, JsonNode> action) {
        final ArrayNode paramArray = getParamArray(node, decomposeFunctionParameters(params, 1, UNLIMITED_WITH_PATH));
        if (paramArray.isEmpty()) {
            return null;
        }
        if (node.isArray()) {
            final ArrayNode array = MAPPER.createArrayNode();
            node.forEach(elem -> array.add(applyAction(elem, isValid, action, paramArray)));
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

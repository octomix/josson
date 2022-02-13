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
import com.octomix.josson.commons.StringUtils;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;

import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.PatternMatcher.*;

/**
 * Common logic to execute functions.
 */
class FuncExecutor {

    private FuncExecutor() {
    }

    static String getParamPath(final String params) {
        final List<String> paramList = decomposeFunctionParameters(params, 0, 1);
        return paramList.isEmpty() ? null : paramList.get(0);
    }

    static Pair<String, List<String>> getParamPathAndStrings(final String params, final int minCount,
                                                             final int maxCount) {
        final List<String> paramList = decomposeFunctionParameters(params, minCount, maxCount + 1);
        final String path = paramList.size() > maxCount ? paramList.remove(0) : null;
        return Pair.of(path, paramList);
    }

    static Map<String, String> getParamNamePath(final List<String> paramList) {
        final Map<String, String> elements = new LinkedHashMap<>();
        for (String param : paramList) {
            final String[] values = param.split(":", 2);
            String name = values[0].trim();
            String path = null;
            if (!isCurrentNodePath(name)) {
                if (values.length == 1) {
                    path = name;
                    name = getLastElementName(path);
                } else {
                    checkElementName(name);
                    if (!StringUtils.isBlank(values[1])) {
                        path = values[1].trim();
                    }
                }
            }
            elements.put(name, path);
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

    static ArrayNode getParamArray(final String params, final JsonNode node) {
        return getParamArray(decomposeFunctionParameters(params, 1, -1), node);
    }

    static ArrayNode getParamArray(final List<String> paramList, final JsonNode node) {
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

    static JsonNode applyWithoutParam(JsonNode node, final String params, final Predicate<JsonNode> isValid,
                                      final Function<JsonNode, JsonNode> action) {
        final String path = getParamPath(params);
        if (path != null) {
            node = getNodeByPath(node, path);
            if (node == null) {
                return null;
            }
        }
        return applyAction(node, isValid, action, null, null);
    }

    static JsonNode applyWithParams(JsonNode node, final String params, final int minCount, final int maxCount,
                                    final Function<List<String>, Object> prepare, final Predicate<JsonNode> isValid,
                                    final BiFunction<JsonNode, Object, JsonNode> action) {
        final Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, minCount, maxCount);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        return applyAction(node, isValid, null, action, prepare.apply(pathAndParams.getValue()));
    }

    static JsonNode applyWithArrayNode(final JsonNode node, final String params, final Predicate<JsonNode> isValid,
                                       final BiFunction<JsonNode, Object, JsonNode> action) {
        final ArrayNode paramArray = getParamArray(params, node);
        if (paramArray.isEmpty()) {
            return null;
        }
        return applyAction(node, isValid, null, action, paramArray);
    }

    private static JsonNode applyAction(final JsonNode node, final Predicate<JsonNode> isValid,
                                        final Function<JsonNode, JsonNode> action1,
                                        final BiFunction<JsonNode, Object, JsonNode> action2, final Object variables) {
        if (node.isArray()) {
            final ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                final JsonNode jsonNode = node.get(i);
                if (isValid.test(jsonNode)) {
                    array.add(action1 == null ? action2.apply(jsonNode, variables) : action1.apply(jsonNode));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (isValid.test(node)) {
            return action1 == null ? action2.apply(node, variables) : action1.apply(node);
        }
        return null;
    }

    static JsonNode applyTextNode(final JsonNode node, final String params,
                                  final Function<JsonNode, String> transform) {
        return applyWithoutParam(node, params, JsonNode::isTextual,
                jsonNode -> TextNode.valueOf(transform.apply(jsonNode)));
    }

    static JsonNode applyTextNodeToInt(final JsonNode node, final String params,
                                       final Function<JsonNode, Integer> transform) {
        return applyWithoutParam(node, params, JsonNode::isTextual,
                jsonNode -> IntNode.valueOf(transform.apply(jsonNode)));
    }

    static JsonNode applyWithParamAsInt(final JsonNode node, final String params,
                                        final Predicate<JsonNode> isValid,
                                        final BiFunction<JsonNode, Object, JsonNode> action) {
        return applyWithParams(node, params, 1, 1,
                paramList -> getNodeAsInt(node, paramList.get(0)),
                isValid, action
        );
    }

    static JsonNode applyWithParamAsText(final JsonNode node, final String params,
                                         final Predicate<JsonNode> isValid,
                                         final BiFunction<JsonNode, Object, JsonNode> action) {
        return applyWithParams(node, params, 1, 1,
                paramList -> getNodeAsText(node, paramList.get(0)),
                isValid, action
        );
    }

    static JsonNode applyTextNodeWithParamAsText(final JsonNode node, final String params,
                                                 final BiFunction<String, String, String> transform) {
        return applyWithParamAsText(node, params, JsonNode::isTextual,
                (jsonNode, objVar) -> TextNode.valueOf(transform.apply(jsonNode.asText(), (String) objVar)));
    }

    static JsonNode applyTextNodeWithParamAsText(final JsonNode node, final String params, final boolean not,
                                                 final BiFunction<String, String, Boolean> transform) {
        return applyWithParamAsText(node, params, JsonNode::isTextual,
                (jsonNode, objVar) -> BooleanNode.valueOf(not ^ transform.apply(jsonNode.asText(), (String) objVar)));
    }
}

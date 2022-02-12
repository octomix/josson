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

    static JsonNode applyFunc(JsonNode node, final String params, final Function<JsonNode, JsonNode> action) {
        final String path = getParamPath(params);
        if (path != null) {
            node = getNodeByPath(node, path);
            if (node == null) {
                return null;
            }
        }
        return action.apply(node);
    }

    static JsonNode applyFunc(JsonNode node, final String params, final Predicate<JsonNode> isValid,
                              final Function<JsonNode, JsonNode> action) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNodeByPath(node, path);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            final ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                final JsonNode jsonNode = node.get(i);
                if (isValid.test(jsonNode)) {
                    array.add(action.apply(jsonNode));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (isValid.test(node)) {
            return action.apply(node);
        }
        return null;
    }

    static JsonNode applyFunc(JsonNode node, final String params, final int minCount, final int maxCount,
                              final Function<List<String>, Object> prepare, final Predicate<JsonNode> isValid,
                              final BiFunction<JsonNode, Object, JsonNode> action) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, minCount, maxCount);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        Object variables = prepare.apply(pathAndParams.getValue());
        if (node.isArray()) {
            final ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                JsonNode jsonNode = node.get(i);
                if (isValid.test(jsonNode)) {
                    array.add(action.apply(jsonNode, variables));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (isValid.test(node)) {
            return action.apply(node, variables);
        }
        return null;
    }

    static JsonNode applyFunc(final JsonNode node, final String params, final Predicate<JsonNode> isValid,
                              final BiFunction<JsonNode, ArrayNode, JsonNode> action) {
        ArrayNode paramArray = getParamArray(params, node);
        if (paramArray.isEmpty()) {
            return null;
        }
        if (node.isArray()) {
            final ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                JsonNode jsonNode = node.get(i);
                if (isValid.test(jsonNode)) {
                    array.add(action.apply(jsonNode, paramArray));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (isValid.test(node)) {
            return action.apply(node, paramArray);
        }
        return null;
    }

    static JsonNode applyFuncWithParamAsText(final JsonNode node, final String params,
                                             final Predicate<JsonNode> isValid,
                                             final BiFunction<JsonNode, Object, JsonNode> action) {
        return applyFunc(node, params, 1, 1,
                paramList -> getNodeAsText(node, paramList.get(0)),
                isValid, action
        );
    }

    static JsonNode applyFuncWithParamAsInt(final JsonNode node, final String params,
                                            final Predicate<JsonNode> isValid,
                                            final BiFunction<JsonNode, Object, JsonNode> action) {
        return applyFunc(node, params, 1, 1,
                paramList -> getNodeAsInt(node, paramList.get(0)),
                isValid, action
        );
    }
}

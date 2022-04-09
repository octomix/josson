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

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;

import java.util.List;
import java.util.Map;

import static com.octomix.josson.FuncExecutor.*;
import static com.octomix.josson.Josson.readJsonNode;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.PatternMatcher.decomposeFunctionParameters;

/**
 * Structural functions.
 */
class FuncStructural {

    private FuncStructural() {
    }

    static JsonNode funcCoalesce(final JsonNode node, final String params) {
        final List<String> paramList = decomposeFunctionParameters(params, 1, -1);
        if (node.isArray()) {
            final ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                array.add(funcCoalesce(node.get(i), paramList));
            }
            return array;
        }
        return funcCoalesce(node, paramList);
    }

    private static JsonNode funcCoalesce(JsonNode node, final List<String> paramList) {
        if (node.isValueNode()) {
            if (!node.isNull()) {
                return node;
            }
            for (String path : paramList) {
                try {
                    node = toValueNode(path);
                    if (node != null && !node.isNull()) {
                        return node;
                    }
                } catch (NumberFormatException e) {
                    // continue
                }
            }
        } else if (node.isObject()) {
            for (String path : paramList) {
                final JsonNode tryNode = getNodeByPath(node, path);
                if (tryNode != null && !tryNode.isNull()) {
                    return tryNode;
                }
            }
        }
        return null;
    }

    static JsonNode funcEntries(JsonNode node, final String params) {
        final String path = getParamPath(params);
        if (path != null) {
            node = getNodeByPath(node, path);
            if (node == null) {
                return null;
            }
        }
        if (!node.isObject()) {
            return null;
        }
        final ArrayNode array = MAPPER.createArrayNode();
        node.fields().forEachRemaining((Map.Entry<String, JsonNode> field) ->
                array.add(Josson.createObjectNode().put("key", field.getKey()).set("value", field.getValue())));
        return array;
    }

    static JsonNode funcField(final JsonNode node, final String params) {
        final Map<String, String> args = getParamNamePath(decomposeFunctionParameters(params, 1, -1));
        if (node.isObject()) {
            return funcMap(node.deepCopy(), node, args, -1);
        }
        if (node.isArray()) {
            final ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                final JsonNode elem = node.get(i);
                array.add(elem.isObject() ? funcMap(elem.deepCopy(), node, args, i) : null);
            }
            return array;
        }
        return null;
    }

    static JsonNode funcFlatten(JsonNode node, final String params) {
        final Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 0, 1);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        final int levels = pathAndParams.getValue().size() > 0 ? getNodeAsInt(node, pathAndParams.getValue().get(0)) : 1;
        if (!node.isArray() || levels < 1) {
            return node;
        }
        final ArrayNode array = MAPPER.createArrayNode();
        funcFlatten(array, node, levels);
        return array;
    }

    private static void funcFlatten(final ArrayNode array, final JsonNode node, final int levels) {
        for (int i = 0; i < node.size(); i++) {
            if (node.get(i).isArray()) {
                if (levels == 1) {
                    array.addAll((ArrayNode) node.get(i));
                } else {
                    funcFlatten(array, node.get(i), levels - 1);
                }
            } else {
                array.add(node.get(i));
            }
        }
    }

    static JsonNode funcJson(final JsonNode node, final String params) {
        return applyWithoutParam(node, params, JsonNode::isTextual,
                jsonNode -> {
                    try {
                        return readJsonNode(jsonNode.asText());
                    } catch (JsonProcessingException e) {
                        throw new IllegalArgumentException(e.getMessage());
                    }
                }
        );
    }

    static JsonNode funcKeys(JsonNode node, final String params) {
        final Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 0, 1);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        if (!node.isObject()) {
            return null;
        }
        final ArrayNode array = MAPPER.createArrayNode();
        funcKeys(array, node,
                pathAndParams.getValue().size() > 0 ? getNodeAsInt(node, pathAndParams.getValue().get(0)) : 1);
        return array;
    }

    private static void funcKeys(final ArrayNode array, final JsonNode node, final int levels) {
        node.fields().forEachRemaining((Map.Entry<String, JsonNode> field) -> {
            array.add(field.getKey());
            if (levels != 1 && field.getValue().isObject()) {
                funcKeys(array, field.getValue(), levels - 1);
            }
        });
    }

    static JsonNode funcMap(final JsonNode node, final String params) {
        final Map<String, String> args = getParamNamePath(decomposeFunctionParameters(params, 1, -1));
        if (!node.isArray()) {
            return funcMap(MAPPER.createObjectNode(), node, args, -1);
        }
        final ArrayNode array = MAPPER.createArrayNode();
        for (int i = 0; i < node.size(); i++) {
            array.add(funcMap(MAPPER.createObjectNode(), node, args, i));
        }
        return array;
    }

    private static ObjectNode funcMap(final ObjectNode base, final JsonNode node,
                                      final Map<String, String> args, final int index) {
        for (Map.Entry<String, String> arg : args.entrySet()) {
            final String name = arg.getKey();
            if (isCurrentNodePath(name)) {
                if ((index >= 0 ? node.get(index) : node).isObject()) {
                    base.setAll((ObjectNode) (index >= 0 ? node.get(index) : node));
                }
                continue;
            }
            final String path = arg.getValue();
            if (path == null) {
                base.remove(name);
            } else {
                base.set(name, getNodeByPath(node, index, path));
            }
        }
        return base;
    }

    static JsonNode funcToArray(final JsonNode node, final String params) {
        final JsonNode container = getParamArrayOrItselfIsContainer(params, node);
        if (container == null) {
            return null;
        }
        final ArrayNode array = MAPPER.createArrayNode();
        if (container.isArray()) {
            for (int i = 0; i < container.size(); i++) {
                if (container.get(i).isArray()) {
                    array.addAll((ArrayNode) container.get(i));
                } else if (container.get(i).isObject()) {
                    container.get(i).forEach(array::add);
                } else {
                    array.add(container.get(i));
                }
            }
        } else {
            container.forEach(array::add);
        }
        return array;
    }
}

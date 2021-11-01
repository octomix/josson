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
import com.fasterxml.jackson.databind.node.*;

import java.util.List;
import java.util.Map;

import static com.octomix.josson.GetFuncParam.*;
import static com.octomix.josson.Josson.getNode;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.PatternMatcher.decomposeFunctionParameters;

class FuncStructural {

    static JsonNode funcCoalesce(JsonNode node, String params) {
        List<String> paramList = decomposeFunctionParameters(params, 1, -1);
        if (node.isArray() && paramList.size() == 1) {
            for (int i = 0; i < node.size(); i++) {
                JsonNode tryNode = getNode(node.get(i), paramList.get(0));
                if (tryNode != null && !tryNode.isNull()) {
                    return tryNode;
                }
            }
            return null;
        }
        return funcCoalesce(node, paramList);
    }

    static JsonNode funcCoalesce(JsonNode node, List<String> paramList) {
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                array.add(funcCoalesce(node.get(i), paramList));
            }
            return array;
        }
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
                JsonNode tryNode = getNode(node, path);
                if (tryNode != null && !tryNode.isNull()) {
                    return tryNode;
                }
            }
        }
        return null;
    }

    static JsonNode funcFlatten(JsonNode node, String params) {
        Pair<String, Integer> pathAndParams = getParamPathAndInt(params);
        if (pathAndParams.hasKey()) {
            node = getNode(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        int flattenLevels = pathAndParams.getValue() == null ? 1 : pathAndParams.getValue();
        if (!node.isArray() || flattenLevels < 0) {
            return node;
        }
        ArrayNode array = MAPPER.createArrayNode();
        funcFlattenElement(array, node, flattenLevels);
        return array;
    }

    private static void funcFlattenElement(ArrayNode array, JsonNode node, int level) {
        for (int i = 0; i < node.size(); i++) {
            if (node.get(i).isArray()) {
                if (level == 1) {
                    array.addAll((ArrayNode) node.get(i));
                } else {
                    funcFlattenElement(array, node.get(i), level - 1);
                }
            } else {
                array.add(node.get(i));
            }
        }
    }

    static JsonNode funcMap(JsonNode node, String params) {
        Map<String, String> args = getParamNamePath(decomposeFunctionParameters(params, 1, -1));
        if (!node.isArray()) {
            return funcMapElement(node, args, 0);
        }
        ArrayNode array = MAPPER.createArrayNode();
        for (int i  = 0; i < node.size(); i++) {
            ObjectNode objNode = funcMapElement(node.get(i), args, array.size());
            if (!objNode.isEmpty()) {
                array.add(objNode);
            }
        }
        return array;
    }

    private static ObjectNode funcMapElement(JsonNode node, Map<String, String> args, int index) {
        ObjectNode objNode = MAPPER.createObjectNode();
        for (Map.Entry<String, String> arg : args.entrySet()) {
            String name = arg.getKey();
            if ("?".equals(name)) {
                if (node.isObject()) {
                    objNode.setAll((ObjectNode) node);
                }
                continue;
            }
            String path = arg.getValue();
            if (path == null) {
                objNode.putNull(name);
            } else if ("?".equals(path)) {
                objNode.set(name, node);
            } else if (path.charAt(0) == '#') {
                objNode.set(name, getIndexId(index, path));
            } else if (node.isObject()) {
                objNode.set(name, getNode(node, path));
            }
        }
        return objNode;
    }

    static IntNode funcSize(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        return IntNode.valueOf(node.size());
    }

    static JsonNode funcToArray(JsonNode node, String params) {
        JsonNode container = getParamArrayOrItselfIsContainer(params, node);
        if (container == null) {
            return null;
        }
        if (container.isArray()) {
            return container;
        }
        ArrayNode array = MAPPER.createArrayNode();
        container.forEach(array::add);
        return array;
    }
}

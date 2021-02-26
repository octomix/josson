package com.octomix.josson;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;

import java.util.Map;

import static com.octomix.josson.GetFuncParam.*;
import static com.octomix.josson.Josson.getNode;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.PatternMatcher.decomposeFunctionParameters;

class FuncStructural {

    static JsonNode funcFlatten(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        if (!node.isArray()) {
            return node;
        }
        ArrayNode array = MAPPER.createArrayNode();
        for (int i = 0; i < node.size(); i++) {
            if (node.get(i).isArray()) {
                array.addAll((ArrayNode) node.get(i));
            } else {
                array.add(node.get(i));
            }
        }
        return array;
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
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            return node;
        }
        ArrayNode array = MAPPER.createArrayNode();
        if (node.isObject()) {
            node.forEach(array::add);
        } else {
            array.add(node);
        }
        return array;
    }
}

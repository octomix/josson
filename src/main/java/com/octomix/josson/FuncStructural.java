package com.octomix.josson;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;

import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import static com.octomix.josson.GetFuncParam.*;
import static com.octomix.josson.Josson.getNode;
import static com.octomix.josson.Josson.readJsonNode;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.PatternMatcher.decomposeFunctionParameters;

class FuncStructural {

    static IntNode funcLength(JsonNode node, String params) {
        getParamNotAccept(params);
        if (node.isContainerNode()) {
            return IntNode.valueOf(node.size());
        }
        if (node.isTextual()) {
            return IntNode.valueOf(node.asText().length());
        }
        return IntNode.valueOf(0);
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
                objNode.set(name, getIndexNode(index, path));
            } else if (node.isObject()) {
                objNode.set(name, getNode(node, path));
            }
        }
        return objNode;
    }

    static IntNode funcSize(JsonNode node, String params) {
        getParamNotAccept(params);
        if (node.isContainerNode()) {
            AtomicInteger count = new AtomicInteger(0);
            node.forEach(element -> {
                if (!element.isNull()) {
                    count.getAndIncrement();
                }
            });
            return IntNode.valueOf(count.get());
        }
        if (node.isTextual()) {
            return IntNode.valueOf(node.asText().length());
        }
        return IntNode.valueOf(0);
    }

    static JsonNode funcToArray(JsonNode node, String params) {
        String value = getParamStringLiteral(params, false);
        if (value != null) {
            try {
                JsonNode newNode = readJsonNode(params);
                if (newNode.isArray()) {
                    return newNode;
                }
                ArrayNode array = MAPPER.createArrayNode();
                array.add(newNode);
                return array;
            } catch (JsonProcessingException e) {
                throw new IllegalArgumentException(e.getMessage());
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

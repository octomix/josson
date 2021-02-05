package com.octomix.josson;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;
import org.apache.commons.lang3.tuple.ImmutablePair;

import java.util.List;
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
            return new IntNode(node.size());
        }
        if (node.isTextual()) {
            return new IntNode(node.asText().length());
        }
        return new IntNode(0);
    }

    static JsonNode funcMap(JsonNode node, String params) {
        List<ImmutablePair<String, String>> args =
                getParamNamePath(decomposeFunctionParameters(params, 1, -1));
        if (!node.isArray()) {
            return funcMapElement(node, args, 1);
        }
        ArrayNode array = MAPPER.createArrayNode();
        for (int i  = 0; i < node.size(); i++) {
            ObjectNode objNode = funcMapElement(node.get(i), args, array.size() + 1);
            if (!objNode.isEmpty()) {
                array.add(objNode);
            }
        }
        return array;
    }

    private static ObjectNode funcMapElement(JsonNode node, List<ImmutablePair<String, String>> args, int index) {
        ObjectNode objNode = MAPPER.createObjectNode();
        for (ImmutablePair<String, String> arg : args) {
            String name = arg.left;
            if ("?".equals(name)) {
                if (node.isObject()) {
                    objNode.setAll((ObjectNode) node);
                }
                continue;
            }
            String path = arg.right;
            if (path == null) {
                objNode.putNull(name);
            } else if ("#".equals(path)) {
                objNode.put(name, index);
            } else if ("?".equals(path)) {
                objNode.set(name, node);
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
            return new IntNode(count.get());
        }
        if (node.isTextual()) {
            return new IntNode(node.asText().length());
        }
        return new IntNode(0);
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

package com.octomix.josson.core;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import static com.octomix.josson.core.GetFuncParam.*;
import static com.octomix.josson.core.JossonCore.*;

public class FuncStructural {

    static IntNode funcLength(JsonNode node, String params) {
        if (!StringUtils.isBlank(params)) {
            throw new UnsupportedOperationException("Not accept function argument");
        }
        if (node.isContainerNode()) {
            return new IntNode(node.size());
        }
        if (node.isTextual()) {
            return new IntNode(node.asText().length());
        }
        return new IntNode(0);
    }

    static JsonNode funcMap(JsonNode node, String params) {
        List<ImmutablePair<String, String>> elements = getParamNamePath(params);
        if (!node.isArray()) {
            return funcMapElement(node, elements, 0);
        }
        ArrayNode array = MAPPER.createArrayNode();
        for (int i  = 0; i < node.size(); i++) {
            ObjectNode objNode = funcMapElement(node.get(i), elements, array.size() + 1);
            if (!objNode.isEmpty()) {
                array.add(objNode);
            }
        }
        return array;
    }

    private static ObjectNode funcMapElement(JsonNode node, List<ImmutablePair<String, String>> elements, int index) {
        ObjectNode objNode = MAPPER.createObjectNode();
        for (ImmutablePair<String, String> element : elements) {
            String name = element.left;
            if ("?".equals(name)) {
                if (node.isObject()) {
                    objNode.setAll((ObjectNode) node);
                }
                continue;
            }
            String path = element.right;
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
        if (!StringUtils.isBlank(params)) {
            throw new UnsupportedOperationException("Not accept function argument");
        }
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
            } catch (Exception e) {
                throw new UnsupportedOperationException(e);
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

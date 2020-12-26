package com.octomix.josson.core;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.DoubleNode;
import com.fasterxml.jackson.databind.node.IntNode;
import com.fasterxml.jackson.databind.node.ValueNode;
import org.apache.commons.lang3.StringUtils;

import static com.octomix.josson.core.JossonCore.MAPPER;
import static com.octomix.josson.core.JossonCore.getNode;

public class FuncNumeric {
    static ValueNode funcAggregate(JsonNode node, String funcId, String params) {
        if (!node.isArray()) {
            return null;
        }
        double sum = 0, max = 0, min = 0;
        int count = 0;
        for (int i = 0; i < node.size(); i++) {
            JsonNode tryNode = getNode(node.get(i), params);
            if (tryNode != null && !tryNode.isNull() && tryNode.isValueNode()) {
                double value = tryNode.asDouble();
                sum += value;
                if (count == 0) {
                    max = value;
                    min = value;
                } else if (value > max) {
                    max = value;
                } else if (value < min) {
                    min = value;
                }
                count++;
            }
        }
        if ("count".equals(funcId)) {
            return new IntNode(count);
        }
        if (count > 0) {
            switch (funcId) {
                case "sum":
                    return new DoubleNode(sum);
                case "max":
                    return new DoubleNode(max);
                case "min":
                    return new DoubleNode(min);
                case "avg":
                    return new DoubleNode(sum / count);
            }
        }
        return null;
    }

    static JsonNode funcAbs(JsonNode node, String params) {
        if (!StringUtils.isBlank(params)) {
            throw new UnsupportedOperationException("Not accept function argument");
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isNumber() || valueNode.isTextual()) {
                    array.add(DoubleNode.valueOf(Math.abs(valueNode.asDouble())));
                }
            }
            return array;
        } else if (!node.isNumber() && !node.isTextual()) {
            return null;
        }
        return DoubleNode.valueOf(Math.abs(node.asDouble()));
    }

    static JsonNode funcCeil(JsonNode node, String params) {
        if (!StringUtils.isBlank(params)) {
            throw new UnsupportedOperationException("Not accept function argument");
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isNumber() || valueNode.isTextual()) {
                    array.add(DoubleNode.valueOf(Math.ceil(valueNode.asDouble())));
                }
            }
            return array;
        } else if (!node.isNumber() && !node.isTextual()) {
            return null;
        }
        return DoubleNode.valueOf(Math.ceil(node.asDouble()));
    }

    static JsonNode funcFloor(JsonNode node, String params) {
        if (!StringUtils.isBlank(params)) {
            throw new UnsupportedOperationException("Not accept function argument");
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isNumber() || valueNode.isTextual()) {
                    array.add(DoubleNode.valueOf(Math.floor(valueNode.asDouble())));
                }
            }
            return array;
        } else if (!node.isNumber() && !node.isTextual()) {
            return null;
        }
        return DoubleNode.valueOf(Math.floor(node.asDouble()));
    }
}

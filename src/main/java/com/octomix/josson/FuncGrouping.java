package com.octomix.josson;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static com.octomix.josson.GetFuncParam.*;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Mapper.MAPPER;

class FuncGrouping {
    static ValueNode funcAggregate(JsonNode node, String funcId, String params) {
        ArrayNode array = getParamArrayOrItself(params, node);
        if (array == null) {
            return null;
        }
        double sum = 0;
        int count = 0;
        for (int i = array.size() - 1; i >= 0; i--) {
            JsonNode tryNode = array.get(i);
            if (nodeHasValue(tryNode)) {
                sum += tryNode.asDouble();
                count++;
            }
        }
        if ("count".equals(funcId)) {
            return IntNode.valueOf(count);
        }
        if (count > 0) {
            switch (funcId) {
                case "sum":
                    return DoubleNode.valueOf(sum);
                case "avg":
                    return DoubleNode.valueOf(sum / count);
            }
        }
        return null;
    }

    static JsonNode funcDistinctValue(JsonNode node, String params) {
        ArrayNode array = getParamArrayOrItself(params, node);
        if (array == null) {
            return null;
        }
        Set<String> texts = new HashSet<>();
        Set<Double> doubles = new HashSet<>();
        Set<Boolean> booleans = new HashSet<>();
        for (int i = 0; i < array.size(); i++) {
            JsonNode tryNode = array.get(i);
            if (tryNode.isTextual()) {
                texts.add(tryNode.asText());
            } else if (tryNode.isNumber()) {
                doubles.add(tryNode.asDouble());
            } else if (tryNode.isBoolean()) {
                booleans.add(tryNode.asBoolean());
            }
        }
        ArrayNode result = MAPPER.createArrayNode();
        texts.forEach(value -> result.add(TextNode.valueOf(value)));
        doubles.forEach(value -> result.add(DoubleNode.valueOf(value)));
        booleans.forEach(value -> result.add(BooleanNode.valueOf(value)));
        return result;
    }

    static TextNode funcJoin(JsonNode node, String params) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 0, 1);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        String delimiter = pathAndParams.getValue().size() > 0 ? getNodeAsText(node, pathAndParams.getValue().get(0)) : "";
        List<String> texts = new ArrayList<>();
        for (int i = 0; i < node.size(); i++) {
            JsonNode valueNode = node.get(i);
            if (nodeHasValue(valueNode)) {
                texts.add(valueNode.asText());
            }
        }
        return TextNode.valueOf(String.join(delimiter, texts));
    }

    static ValueNode funcMaxMin(JsonNode node, String params, boolean isMax) {
        ArrayNode array = getParamArrayOrItself(params, node);
        if (array == null) {
            return null;
        }
        double maxMinDouble = 0;
        ValueNode maxMinNumber = null;
        String maxMinString = null;
        for (int i = array.size() - 1; i >= 0; i--) {
            JsonNode tryNode = array.get(i);
            if (!nodeHasValue(tryNode)) {
                continue;
            }
            if (maxMinNumber != null || tryNode.isNumber()) {
                if (tryNode.isNumber()) {
                    double tryValue = tryNode.asDouble();
                    if (maxMinNumber == null
                            || (isMax && tryValue > maxMinDouble)
                            || (!isMax && tryValue < maxMinDouble)) {
                        maxMinNumber = (ValueNode) tryNode;
                        maxMinDouble = tryValue;
                    }
                }
            } else {
                String tryValue = tryNode.asText();
                if (maxMinString == null
                        || (isMax && tryValue.compareTo(maxMinString) > 0)
                        || (!isMax && tryValue.compareTo(maxMinString) < 0)) {
                    maxMinString = tryValue;
                }
            }
        }
        return maxMinNumber != null ? maxMinNumber :
                maxMinString != null ? TextNode.valueOf(maxMinString) : null;
    }

    static IntNode funcSize(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNodeByPath(node, path);
            if (node == null) {
                return null;
            }
        }
        return IntNode.valueOf(node.size());
    }
}

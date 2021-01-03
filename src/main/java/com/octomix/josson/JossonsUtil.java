package com.octomix.josson;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.octomix.josson.core.JossonCore;
import org.apache.commons.lang3.StringUtils;

class JossonsUtil {

    enum JoinOperator {
        INNER_JOIN_ONE,
        LEFT_JOIN_ONE,
        RIGHT_JOIN_ONE,
        LEFT_JOIN_MANY,
        RIGHT_JOIN_MANY
    }

    static String unquoteString(String quotedString) {
        return quotedString.substring(1, quotedString.length() - 1)
                .replaceAll("''", "'");
    }

    static boolean anyIsBlank(String[] strings) {
        if (strings == null || strings.length == 0) {
            return true;
        }
        for (int i = strings.length - 1; i >= 0; i--) {
            if (StringUtils.isBlank(strings[i])) {
                return true;
            }
        }
        return false;
    }

    static JsonNode joinNodes(JsonNode leftNode, String[] leftKeys, String leftArrayName, JoinOperator operator,
                              JsonNode rightNode, String[] rightKeys, String rightArrayName) {
        String arrayName;
        if (operator == JoinOperator.RIGHT_JOIN_ONE || operator == JoinOperator.RIGHT_JOIN_MANY
                || (operator == JoinOperator.INNER_JOIN_ONE && !leftNode.isObject() && rightNode.isObject())) {
            JsonNode swapNode = leftNode;
            leftNode = rightNode;
            rightNode = swapNode;
            String[] swapKeys = leftKeys;
            leftKeys = rightKeys;
            rightKeys = swapKeys;
            if (operator == JoinOperator.RIGHT_JOIN_ONE) {
                operator = JoinOperator.LEFT_JOIN_ONE;
            } else if (operator == JoinOperator.RIGHT_JOIN_MANY) {
                operator = JoinOperator.LEFT_JOIN_MANY;
            }
            arrayName = leftArrayName;
        } else {
            arrayName = rightArrayName;
        }
        ArrayNode rightArray;
        if (rightNode.isArray()) {
            rightArray = (ArrayNode) rightNode;
        } else {
            rightArray = JossonCore.createArrayNode();
            rightArray.add(rightNode);
        }
        if (leftNode.isObject()) {
            return joinToObjectNode((ObjectNode) leftNode, leftKeys, operator, rightArray, rightKeys, arrayName);
        }
        ArrayNode joinedArray = JossonCore.createArrayNode();
        for (int i = 0; i < leftNode.size(); i++) {
            if (leftNode.get(i).isObject()) {
                ObjectNode joinedNode = joinToObjectNode(
                        (ObjectNode) leftNode.get(i), leftKeys, operator, rightArray, rightKeys, arrayName);
                if (joinedNode != null) {
                    joinedArray.add(joinedNode);
                }
            }
        }
        return joinedArray;
    }

    static ObjectNode joinToObjectNode(ObjectNode leftObject, String[] leftKeys, JoinOperator operator,
                                       ArrayNode rightArray, String[] rightKeys, String arrayName) {
        String[] conditions = new String[leftKeys.length];
        for (int j = leftKeys.length - 1; j >= 0; j--) {
            JsonNode leftValue = JossonCore.getNode(leftObject, leftKeys[j]);
            if (leftValue == null || !leftValue.isValueNode()) {
                return null;
            }
            conditions[j] = rightKeys[j]
                    + (leftValue.isTextual() ? "='" : "=") + leftValue.asText().replaceAll("'", "''")
                    + (leftValue.isTextual() ? "'" : "");
        }
        if (operator == JoinOperator.LEFT_JOIN_MANY) {
            JsonNode rightToJoin = JossonCore.getNode(
                    rightArray, "[" + StringUtils.join(conditions, " & ") + "]@");
            if (rightToJoin != null) {
                ObjectNode joinedNode = leftObject.deepCopy();
                joinedNode.set(arrayName, rightToJoin);
                return joinedNode;
            }
        } else {
            JsonNode rightToJoin = JossonCore.getNode(
                    rightArray, "[" + StringUtils.join(conditions, " & ") + "]");
            if (rightToJoin != null && rightToJoin.isObject()) {
                ObjectNode joinedNode = leftObject.deepCopy();
                joinedNode.setAll((ObjectNode) rightToJoin);
                return joinedNode;
            }
            if (operator == JoinOperator.INNER_JOIN_ONE) {
                return null;
            }
        }
        return leftObject;
    }
}

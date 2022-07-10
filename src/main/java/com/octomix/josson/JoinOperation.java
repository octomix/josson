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
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.octomix.josson.commons.StringUtils;

import java.util.function.Function;

import static com.octomix.josson.ArrayFilter.FilterMode.FILTRATE_COLLECT_ALL;
import static com.octomix.josson.JossonCore.QUOTE_SYMBOL;
import static com.octomix.josson.JossonCore.getNodeByPath;
import static com.octomix.josson.Mapper.*;
import static com.octomix.josson.commons.StringUtils.EMPTY;

/**
 * Dataset join operators.
 */
enum JoinOperation {

    /**
     * Inner join one.
     */
    INNER_JOIN_ONE(">=<"),

    /**
     * Left join one.
     */
    LEFT_JOIN_ONE("<=<"),

    /**
     * Right join one.
     */
    RIGHT_JOIN_ONE(">=>"),

    /**
     * Left join many.
     */
    LEFT_JOIN_MANY("<=<<"),

    /**
     * Right join many.
     */
    RIGHT_JOIN_MANY(">>=>"),

    /**
     * Concatenate two objects or two arrays, from right into the left one.
     */
    LEFT_CONCATENATE("<<<"),

    /**
     * Concatenate two objects or two arrays, from left into the right one.
     */
    RIGHT_CONCATENATE(">>>");

    private final String symbol;

    private JoinDataset leftDataset;

    private JoinDataset rightDataset;

    private String arrayName;

    JoinOperation(final String symbol) {
        this.symbol = symbol;
    }

    static JoinOperation fromSymbol(final String symbol) {
        for (JoinOperation operator : values()) {
            if (operator.symbol.equals(symbol)) {
                return operator;
            }
        }
        return null;
    }

    JoinOperation init(final JoinDataset leftDataset, final JoinDataset rightDataset) {
        if (this == LEFT_CONCATENATE || this == RIGHT_CONCATENATE) {
            if (leftDataset.getKeys() != null || rightDataset.getKeys() != null) {
                throw new IllegalArgumentException("Concatenate operation does not need join key");
            }
        } else if (leftDataset.getKeys() == null || rightDataset.getKeys() == null) {
            throw new IllegalArgumentException("Missing join key");
        } else if (leftDataset.getKeys().length != rightDataset.getKeys().length) {
            throw new IllegalArgumentException("Mismatch key count");
        }
        this.leftDataset = leftDataset;
        this.rightDataset = rightDataset;
        switch (this) {
            case LEFT_JOIN_MANY:
                this.arrayName = rightDataset.resolveArrayName();
                break;
            case RIGHT_JOIN_MANY:
                this.arrayName = leftDataset.resolveArrayName();
                break;
            default:
                this.arrayName = null;
                break;
        }
        return this;
    }

    JsonNode apply(final Function<String, JsonNode> evaluateQuery) {
        final JsonNode leftNode = leftDataset.apply(evaluateQuery);
        final JsonNode rightNode = rightDataset.apply(evaluateQuery);
        switch (this) {
            case INNER_JOIN_ONE:
                if (leftNode.isObject() || !rightNode.isObject()) {
                    break;
                }
                return joinNodes(rightNode, leftNode);
            case RIGHT_JOIN_ONE:
                return LEFT_JOIN_ONE.joinNodes(rightNode, leftNode);
            case RIGHT_JOIN_MANY:
                return LEFT_JOIN_MANY.joinNodes(rightNode, leftNode);
            case RIGHT_CONCATENATE:
                return LEFT_CONCATENATE.joinNodes(rightNode, leftNode);
        }
        return joinNodes(leftNode, rightNode);
    }

    private JsonNode joinNodes(final JsonNode leftNode, final JsonNode rightNode) {
        if (this == LEFT_CONCATENATE) {
            if (leftNode.isObject() && rightNode.isObject()) {
                return cloneObjectNode((ObjectNode) leftNode).setAll((ObjectNode) rightNode);
            } else if (leftNode.isArray() && rightNode.isArray()) {
                return cloneArrayNode((ArrayNode) leftNode).addAll((ArrayNode) rightNode);
            }
            throw new IllegalArgumentException("cannot concatenate an object and an array");
        }
        final ArrayNode rightArray;
        if (rightNode.isArray()) {
            rightArray = (ArrayNode) rightNode;
        } else {
            rightArray = MAPPER.createArrayNode();
            rightArray.add(rightNode);
        }
        if (leftNode.isObject()) {
            final ObjectNode joinedObject = joinToObjectNode((ObjectNode) leftNode, rightArray);
            if (joinedObject == null) {
                throw new IllegalArgumentException("invalid data");
            }
            return joinedObject;
        }
        final ArrayNode joinedArray = MAPPER.createArrayNode();
        for (int i = 0; i < leftNode.size(); i++) {
            if (leftNode.get(i).isObject()) {
                final ObjectNode joinedNode = joinToObjectNode((ObjectNode) leftNode.get(i), rightArray);
                if (joinedNode != null) {
                    joinedArray.add(joinedNode);
                }
            }
        }
        return joinedArray;
    }

    private ObjectNode joinToObjectNode(final ObjectNode leftObject, final ArrayNode rightArray) {
        final String[] relationalOps = new String[leftDataset.getKeys().length];
        for (int j = leftDataset.getKeys().length - 1; j >= 0; j--) {
            final JsonNode leftValue = getNodeByPath(leftObject, leftDataset.getKeys()[j]);
            if (leftValue == null || !leftValue.isValueNode()) {
                return null;
            }
            relationalOps[j] = rightDataset.getKeys()[j] + Operator.EQ.getSymbol()
                    + (leftValue.isTextual() ? QUOTE_SYMBOL : EMPTY)
                    + leftValue.asText().replace("'", "''")
                    + (leftValue.isTextual() ? QUOTE_SYMBOL : EMPTY);
        }
        final String path = String.format("[%s]", StringUtils.join(relationalOps, Operator.AND.getSymbol()));
        if (this == LEFT_JOIN_MANY) {
            final JsonNode rightToJoin = getNodeByPath(rightArray, path + FILTRATE_COLLECT_ALL.getSymbol());
            if (rightToJoin != null) {
                return cloneObjectNode(leftObject).set(arrayName, rightToJoin);
            }
        } else {
            final JsonNode rightToJoin = getNodeByPath(rightArray, path);
            if (rightToJoin != null && rightToJoin.isObject()) {
                return cloneObjectNode(leftObject).setAll((ObjectNode) rightToJoin);
            }
            if (this == INNER_JOIN_ONE) {
                return null;
            }
        }
        return leftObject;
    }
}

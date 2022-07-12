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
     * Left excluding join.
     */
    LEFT_EXCLUDING_JOIN("<!<"),

    /**
     * Right excluding join.
     */
    RIGHT_EXCLUDING_JOIN(">!>"),

    /**
     * Outer excluding join.
     */
    OUTER_EXCLUDING_JOIN("<!>"),

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
        leftDataset.apply(evaluateQuery);
        rightDataset.apply(evaluateQuery);
        switch (this) {
            case INNER_JOIN_ONE:
                if (leftDataset.getNode().isObject() || !rightDataset.getNode().isObject()) {
                    break;
                }
                return joinNodes(rightDataset, leftDataset);
            case RIGHT_JOIN_ONE:
                return LEFT_JOIN_ONE.joinNodes(rightDataset, leftDataset);
            case RIGHT_JOIN_MANY:
                return LEFT_JOIN_MANY.joinNodes(rightDataset, leftDataset);
            case RIGHT_EXCLUDING_JOIN:
                return LEFT_EXCLUDING_JOIN.joinNodes(rightDataset, leftDataset);
            case LEFT_CONCATENATE:
                return concatenate(leftDataset.getNode(), rightDataset.getNode());
            case RIGHT_CONCATENATE:
                return concatenate(rightDataset.getNode(), leftDataset.getNode());
        }
        return joinNodes(leftDataset, rightDataset);
    }

    private JsonNode concatenate(final JsonNode leftNode, final JsonNode rightNode) {
        if (leftNode.isObject() && rightNode.isObject()) {
            return cloneObjectNode((ObjectNode) leftNode).setAll((ObjectNode) rightNode);
        } else if (leftNode.isArray() && rightNode.isArray()) {
            return cloneArrayNode((ArrayNode) leftNode).addAll((ArrayNode) rightNode);
        }
        throw new IllegalArgumentException("cannot concatenate an object and an array");
    }

    private JsonNode joinNodes(final JoinDataset left, final JoinDataset right) {
        final ArrayNode rightArray;
        if (right.getNode().isArray()) {
            rightArray = (ArrayNode) right.getNode();
        } else {
            rightArray = MAPPER.createArrayNode();
            rightArray.add(right.getNode());
        }
        if (left.getNode().isObject()) {
            final ObjectNode joinedObject = joinToObjectNode(
                    (ObjectNode) left.getNode(), left.getKeys(), rightArray, right.getKeys());
            if (joinedObject == null) {
                throw new IllegalArgumentException("invalid data");
            }
            return joinedObject;
        }
        final ArrayNode joinedArray = MAPPER.createArrayNode();
        for (int i = 0; i < left.getNode().size(); i++) {
            if (left.getNode().get(i).isObject()) {
                final ObjectNode joinedNode = joinToObjectNode(
                        (ObjectNode) left.getNode().get(i), left.getKeys(), rightArray, right.getKeys());
                if (joinedNode != null) {
                    joinedArray.add(joinedNode);
                }
            }
        }
        if (this == OUTER_EXCLUDING_JOIN) {
            return joinedArray.addAll((ArrayNode) LEFT_EXCLUDING_JOIN.joinNodes(right, left));
        }
        return joinedArray;
    }

    private ObjectNode joinToObjectNode(final ObjectNode leftObject, final String[] leftKeys,
                                        final ArrayNode rightArray, final String[] rightKeys) {
        final String[] relationalOps = new String[leftKeys.length];
        for (int j = leftKeys.length - 1; j >= 0; j--) {
            final JsonNode leftValue = getNodeByPath(leftObject, leftKeys[j]);
            if (leftValue == null || !leftValue.isValueNode()) {
                return null;
            }
            relationalOps[j] = rightKeys[j] + Operator.EQ.getSymbol()
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
            if (this == LEFT_EXCLUDING_JOIN || this == OUTER_EXCLUDING_JOIN) {
                if (rightToJoin != null) {
                    return null;
                }
            } else if (rightToJoin != null && rightToJoin.isObject()) {
                return cloneObjectNode(leftObject).setAll((ObjectNode) rightToJoin);
            } else if (this == INNER_JOIN_ONE) {
                return null;
            }
        }
        return leftObject;
    }
}

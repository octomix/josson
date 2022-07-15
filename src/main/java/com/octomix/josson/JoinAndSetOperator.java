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

import java.util.Map;
import java.util.function.Function;

import static com.octomix.josson.ArrayFilter.FilterMode.FILTRATE_COLLECT_ALL;
import static com.octomix.josson.JossonCore.QUOTE_SYMBOL;
import static com.octomix.josson.JossonCore.getNodeByPath;
import static com.octomix.josson.Mapper.*;
import static com.octomix.josson.commons.StringUtils.EMPTY;

/**
 * Dataset join and set operators.
 */
enum JoinAndSetOperator {

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
     * Concatenate right into left, works on two objects or two arrays.
     */
    LEFT_CONCATENATE("<+<"),

    /**
     * Concatenate left into right, works on two objects or two arrays.
     */
    RIGHT_CONCATENATE(">+>"),

    /**
     * Subtract right from left, works on two objects or two arrays.
     */
    SUBTRACT_RIGHT_FROM_LEFT("<-<"),

    /**
     * Subtract left from right, works on two objects or two arrays.
     */
    SUBTRACT_LEFT_FROM_RIGHT(">->"),

    /**
     * Symmetric difference, works on two objects or two arrays.
     */
    SYMMETRIC_DIFFERENCE("<->"),

    /**
     * Union, works on two arrays.
     */
    UNION("<u>"),

    /**
     * Intersection, works on two arrays.
     */
    INTERSECTION(">n<");

    private final String symbol;

    private JoinAndSetOperand leftDataset;

    private JoinAndSetOperand rightDataset;

    private static final Operator eq = Operator.EQ;

    JoinAndSetOperator(final String symbol) {
        this.symbol = symbol;
    }

    static JoinAndSetOperator fromSymbol(final String symbol) {
        for (JoinAndSetOperator operator : values()) {
            if (operator.symbol.equals(symbol)) {
                return operator;
            }
        }
        return null;
    }

    JoinAndSetOperator init(final JoinAndSetOperand leftDataset, final JoinAndSetOperand rightDataset) {
        switch (this) {
            case LEFT_CONCATENATE:
            case RIGHT_CONCATENATE:
            case SUBTRACT_RIGHT_FROM_LEFT:
            case SUBTRACT_LEFT_FROM_RIGHT:
            case SYMMETRIC_DIFFERENCE:
            case UNION:
            case INTERSECTION:
                if (leftDataset.getKeys() != null || rightDataset.getKeys() != null) {
                    throw new IllegalArgumentException("Set operation does not need join key");
                }
                break;
            default:
                if (leftDataset.getKeys() == null || rightDataset.getKeys() == null) {
                    throw new IllegalArgumentException("Missing join key");
                } else if (leftDataset.getKeys().length != rightDataset.getKeys().length) {
                    throw new IllegalArgumentException("Mismatch key count");
                }
                break;
        }
        this.leftDataset = leftDataset;
        this.rightDataset = rightDataset;
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
            case SUBTRACT_RIGHT_FROM_LEFT:
                return subtract(leftDataset.getNode(), rightDataset.getNode());
            case SUBTRACT_LEFT_FROM_RIGHT:
                return subtract(rightDataset.getNode(), leftDataset.getNode());
            case SYMMETRIC_DIFFERENCE:
                return symmetricDifference(leftDataset.getNode(), rightDataset.getNode());
            case UNION:
                return union(rightDataset.getNode(), leftDataset.getNode());
            case INTERSECTION:
                return intersection(rightDataset.getNode(), leftDataset.getNode());
            default:
                break;
        }
        return joinNodes(leftDataset, rightDataset);
    }

    private JsonNode joinNodes(final JoinAndSetOperand left, final JoinAndSetOperand right) {
        final String arrayName = this == LEFT_JOIN_MANY ? right.resolveArrayName() : null;
        final ArrayNode rightArray;
        if (right.getNode().isArray()) {
            rightArray = (ArrayNode) right.getNode();
        } else {
            rightArray = MAPPER.createArrayNode();
            rightArray.add(right.getNode());
        }
        if (left.getNode().isObject()) {
            final ObjectNode joinedObject = joinToObjectNode(
                    (ObjectNode) left.getNode(), left.getKeys(), rightArray, right.getKeys(), arrayName);
            if (joinedObject == null) {
                throw new IllegalArgumentException("invalid data");
            }
            return joinedObject;
        }
        final ArrayNode joinedArray = MAPPER.createArrayNode();
        for (int i = 0; i < left.getNode().size(); i++) {
            if (left.getNode().get(i).isObject()) {
                final ObjectNode joinedNode = joinToObjectNode(
                        (ObjectNode) left.getNode().get(i), left.getKeys(), rightArray, right.getKeys(), arrayName);
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
                                        final ArrayNode rightArray, final String[] rightKeys,
                                        final String arrayName) {
        final String[] relationalOps = new String[leftKeys.length];
        for (int j = leftKeys.length - 1; j >= 0; j--) {
            final JsonNode leftValue = getNodeByPath(leftObject, leftKeys[j]);
            if (leftValue == null || !leftValue.isValueNode()) {
                return null;
            }
            relationalOps[j] = rightKeys[j] + eq.getSymbol()
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

    private static JsonNode concatenate(final JsonNode leftNode, final JsonNode rightNode) {
        if (leftNode.isObject() && rightNode.isObject()) {
            return cloneObjectNode((ObjectNode) leftNode).setAll((ObjectNode) rightNode);
        } else if (leftNode.isArray() && rightNode.isArray()) {
            return cloneArrayNode((ArrayNode) leftNode).addAll((ArrayNode) rightNode);
        }
        throw new IllegalArgumentException("cannot concatenate an object and an array");
    }

    private static JsonNode subtract(final JsonNode leftNode, final JsonNode rightNode) {
        if (leftNode.isObject() && rightNode.isObject()) {
            final ObjectNode node = MAPPER.createObjectNode();
            leftNode.fields().forEachRemaining(
                    (Map.Entry<String, JsonNode> field) -> {
                        if (!rightNode.has(field.getKey())) {
                            node.set(field.getKey(), field.getValue());
                        }
                    }
            );
            return node;
        } else if (leftNode.isArray() && rightNode.isArray()) {
            final ArrayNode node = MAPPER.createArrayNode();
            for (int i = 0; i < leftNode.size(); i++) {
                int j = rightNode.size() - 1;
                for (; j >= 0 ; j--) {
                    if (eq.relationalCompare(leftNode.get(i), rightNode.get(j))) {
                        break;
                    }
                }
                if (j < 0) {
                    node.add(leftNode.get(i));
                }
            }
            return node;
        }
        throw new IllegalArgumentException("cannot subtract between an object and an array");
    }

    private static ArrayNode symmetricDifference(final JsonNode leftNode, final JsonNode rightNode) {
        if (leftNode.isObject() && rightNode.isObject()) {
            return ((ObjectNode) subtract(leftNode, rightNode)).setAll((ObjectNode) subtract(rightNode, leftNode));
        } else if (leftNode.isArray() && rightNode.isArray()) {
            return ((ArrayNode) subtract(leftNode, rightNode)).addAll((ArrayNode) subtract(rightNode, leftNode));
        }
        throw new IllegalArgumentException("cannot operate difference between an object and an array");
    }

    private static ArrayNode union(final JsonNode leftNode, final JsonNode rightNode) {
        if (leftNode.isArray() && rightNode.isArray()) {
            return cloneArrayNode((ArrayNode) rightNode).addAll((ArrayNode) subtract(leftNode, rightNode));
        }
        throw new IllegalArgumentException("cannot operate union on object");
    }

    private static ArrayNode intersection(final JsonNode leftNode, final JsonNode rightNode) {
        if (leftNode.isArray() && rightNode.isArray()) {
            final ArrayNode node = MAPPER.createArrayNode();
            for (int i = 0; i < leftNode.size(); i++) {
                for (int j = 0; j < rightNode.size(); j++) {
                    if (eq.relationalCompare(leftNode.get(i), rightNode.get(j))) {
                        node.add(leftNode.get(i));
                        break;
                    }
                }
            }
            return node;
        }
        throw new IllegalArgumentException("cannot operate intersection on object");
    }
}

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

import java.util.Iterator;
import java.util.Map;
import java.util.function.Function;

import static com.octomix.josson.ArrayFilter.FilterMode.FILTRATE_COLLECT_ALL;
import static com.octomix.josson.CombineOperator.*;
import static com.octomix.josson.JossonCore.QUOTE_SYMBOL;
import static com.octomix.josson.JossonCore.getNodeByPath;
import static com.octomix.josson.Mapper.*;
import static com.octomix.josson.Utils.mergeObjects;
import static com.octomix.josson.commons.StringUtils.EMPTY;

/**
 * Dataset join and set operation.
 */
class CombineOperation {

    private final CombineOperand leftOperand;

    private final CombineOperator operator;

    private final CombineOperand rightOperand;

    CombineOperation(final CombineOperand leftOperand, final CombineOperator operator, final CombineOperand rightOperand) {
        switch (operator) {
            // Set operations
            case LEFT_CONCATENATE:
            case RIGHT_CONCATENATE:
            case SUBTRACT_RIGHT_FROM_LEFT:
            case SUBTRACT_LEFT_FROM_RIGHT:
            case SYMMETRIC_DIFFERENCE:
            case UNION:
            case INTERSECTION:
                if (leftOperand.getKeys() != null || rightOperand.getKeys() != null) {
                    throw new IllegalArgumentException("Set operation does not need join key");
                }
                break;

            // Join operations
            default:
                if (leftOperand.getKeys() == null || rightOperand.getKeys() == null) {
                    throw new IllegalArgumentException("Missing join key");
                } else if (leftOperand.getKeys().length != rightOperand.getKeys().length) {
                    throw new IllegalArgumentException("Mismatch key count");
                }
                break;
        }
        this.leftOperand = leftOperand;
        this.operator = operator;
        this.rightOperand = rightOperand;
    }

    JsonNode apply(JsonNode previousResult, final Function<String, JsonNode> evaluateQuery) {
        if (previousResult != null) {
            leftOperand.setNode(previousResult);
        } else {
            leftOperand.apply(evaluateQuery);
        }
        rightOperand.apply(evaluateQuery);
        switch (operator) {
            case INNER_JOIN:
                if (leftOperand.getNode().isObject() || !rightOperand.getNode().isObject()) {
                    break;
                }
                return joinNodes(rightOperand, operator, leftOperand);
            case RIGHT_JOIN_ONE:
                return joinNodes(rightOperand, LEFT_JOIN_ONE, leftOperand);
            case RIGHT_JOIN_MANY:
                return joinNodes(rightOperand, LEFT_JOIN_MANY, leftOperand);
            case RIGHT_EXCLUDING_JOIN:
                return joinNodes(rightOperand, LEFT_EXCLUDING_JOIN, leftOperand);
            case LEFT_CONCATENATE:
                return concatenate(leftOperand.getNode(), rightOperand.getNode());
            case RIGHT_CONCATENATE:
                return concatenate(rightOperand.getNode(), leftOperand.getNode());
            case SUBTRACT_RIGHT_FROM_LEFT:
                return subtract(leftOperand.getNode(), rightOperand.getNode());
            case SUBTRACT_LEFT_FROM_RIGHT:
                return subtract(rightOperand.getNode(), leftOperand.getNode());
            case SYMMETRIC_DIFFERENCE:
                return symmetricDifference(leftOperand.getNode(), rightOperand.getNode());
            case UNION:
                return union(rightOperand.getNode(), leftOperand.getNode());
            case INTERSECTION:
                return intersection(rightOperand.getNode(), leftOperand.getNode());
            default:
                break;
        }
        return joinNodes(leftOperand, operator, rightOperand);
    }

    private static JsonNode joinNodes(final CombineOperand left, final CombineOperator operator, final CombineOperand right) {
        final String arrayName = operator == LEFT_JOIN_MANY ? right.resolveArrayName() : null;
        final ArrayNode rightArray;
        if (right.getNode().isArray()) {
            rightArray = (ArrayNode) right.getNode();
        } else {
            rightArray = MAPPER.createArrayNode();
            rightArray.add(right.getNode());
        }
        if (left.getNode().isObject()) {
            final ObjectNode joinedObject = joinToObjectNode(
                    (ObjectNode) left.getNode(), left.getKeys(), operator, rightArray, right.getKeys(), arrayName);
            if (joinedObject == null) {
                throw new IllegalArgumentException("invalid data");
            }
            return joinedObject;
        }
        final ArrayNode joinedArray = MAPPER.createArrayNode();
        for (int i = 0; i < left.getNode().size(); i++) {
            if (left.getNode().get(i).isObject()) {
                final ObjectNode joinedNode = joinToObjectNode(
                        (ObjectNode) left.getNode().get(i), left.getKeys(), operator, rightArray, right.getKeys(), arrayName);
                if (joinedNode != null) {
                    joinedArray.add(joinedNode);
                }
            }
        }
        if (operator == OUTER_EXCLUDING_JOIN) {
            return joinedArray.addAll((ArrayNode) joinNodes(right, LEFT_EXCLUDING_JOIN, left));
        }
        return joinedArray;
    }

    private static ObjectNode joinToObjectNode(final ObjectNode leftObject, final String[] leftKeys,
                                               final CombineOperator operator,
                                               final ArrayNode rightArray, final String[] rightKeys,
                                               final String arrayName) {
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
        if (operator == LEFT_JOIN_MANY) {
            final JsonNode rightToJoin = getNodeByPath(rightArray, path + FILTRATE_COLLECT_ALL.getSymbol());
            if (rightToJoin != null) {
                return cloneObject(leftObject).set(arrayName, rightToJoin);
            }
        } else {
            final JsonNode rightToJoin = getNodeByPath(rightArray, path);
            if (operator == LEFT_EXCLUDING_JOIN || operator == OUTER_EXCLUDING_JOIN) {
                if (rightToJoin != null) {
                    return null;
                }
            } else if (rightToJoin != null && rightToJoin.isObject()) {
                return cloneObject(leftObject).setAll((ObjectNode) rightToJoin);
            } else if (operator == INNER_JOIN) {
                return null;
            }
        }
        return leftObject;
    }

    private static JsonNode concatenate(final JsonNode leftNode, final JsonNode rightNode) {
        if (rightNode.isObject()) {
            if (leftNode.isObject()) {
                ObjectNode concat = leftNode.deepCopy();
                mergeObjects(concat, rightNode);
                return concat;
            }
            return cloneArray((ArrayNode) leftNode).add(leftNode);
        }
        if (leftNode.isObject()) {
            return intoNewArray(leftNode).addAll((ArrayNode) rightNode);
        }
        return cloneArray((ArrayNode) leftNode).addAll((ArrayNode) rightNode);
    }

    private static JsonNode subtract(final JsonNode leftNode, JsonNode rightNode) {
        if (leftNode.isObject() && rightNode.isObject()) {
            final ObjectNode node = MAPPER.createObjectNode();
            final Iterator<Map.Entry<String, JsonNode>> iterator = leftNode.fields();
            while (iterator.hasNext()) {
                final Map.Entry<String, JsonNode> entry = iterator.next();
                if (rightNode.has(entry.getKey())) {
                    if (entry.getValue().isObject() && rightNode.get(entry.getKey()).isObject()
                        || entry.getValue().isArray() && rightNode.get(entry.getKey()).isArray()) {
                        final JsonNode diff = subtract(entry.getValue(), rightNode.get(entry.getKey()));
                        if (!diff.isEmpty()) {
                            node.set(entry.getKey(), diff);
                        }
                    } else if (Operator.NE.relationalCompare(entry.getValue(), rightNode.get(entry.getKey()))) {
                        node.set(entry.getKey(), entry.getValue());
                    }
                } else {
                    node.set(entry.getKey(), entry.getValue());
                }
            }
            return node;
        }
        if (rightNode.isObject()) {
            rightNode = intoNewArray(rightNode);
        }
        final ArrayNode node = MAPPER.createArrayNode();
        for (JsonNode leftElem : leftNode.isObject() ? intoNewArray(leftNode) : leftNode) {
            int i = rightNode.size() - 1;
            for (; i >= 0 ; i--) {
                if (Operator.EQ.relationalCompare(leftElem, rightNode.get(i))) {
                    break;
                }
            }
            if (i < 0) {
                node.add(leftElem);
            }
        }
        return node;
    }

    private static JsonNode symmetricDifference(final JsonNode leftNode, final JsonNode rightNode) {
        if (leftNode.isObject() && rightNode.isObject()) {
            return ((ObjectNode) subtract(leftNode, rightNode)).setAll((ObjectNode) subtract(rightNode, leftNode));
        }
        if (leftNode.isArray() && rightNode.isArray()) {
            return ((ArrayNode) subtract(leftNode, rightNode)).addAll((ArrayNode) subtract(rightNode, leftNode));
        }
        throw new IllegalArgumentException("cannot operate difference between an object and an array");
    }

    private static ArrayNode union(final JsonNode leftNode, final JsonNode rightNode) {
        if (leftNode.isArray() && rightNode.isArray()) {
            return cloneArray((ArrayNode) rightNode).addAll((ArrayNode) subtract(leftNode, rightNode));
        }
        throw new IllegalArgumentException("cannot operate union on object");
    }

    private static ArrayNode intersection(final JsonNode leftNode, final JsonNode rightNode) {
        if (leftNode.isArray() && rightNode.isArray()) {
            final ArrayNode node = MAPPER.createArrayNode();
            for (int i = 0; i < leftNode.size(); i++) {
                for (int j = 0; j < rightNode.size(); j++) {
                    if (Operator.EQ.relationalCompare(leftNode.get(i), rightNode.get(j))) {
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

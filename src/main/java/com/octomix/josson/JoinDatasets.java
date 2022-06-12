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

import java.util.UnknownFormatConversionException;
import java.util.function.Function;

import static com.octomix.josson.ArrayFilter.FilterMode.FILTRATE_COLLECT_ALL;
import static com.octomix.josson.JossonCore.QUOTE_SYMBOL;
import static com.octomix.josson.JossonCore.getNodeByPath;
import static com.octomix.josson.Mapper.*;
import static com.octomix.josson.PatternMatcher.*;

/**
 * Join datasets operations.
 */
class JoinDatasets {

    /**
     * Dataset join operators.
     */
    enum JoinOperator {

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

        JoinOperator(final String symbol) {
            this.symbol = symbol;
        }

        static JoinOperator fromSymbol(final String symbol) {
            for (JoinOperator operator : values()) {
                if (operator.symbol.equals(symbol)) {
                    return operator;
                }
            }
            return null;
        }
    }

    private Dataset leftDataset;

    private JoinOperator operator;

    private Dataset rightDataset;

    private final String arrayName;

    JoinDatasets(final Dataset leftDataset, final JoinOperator operator, final Dataset rightDataset) {
        this.leftDataset = leftDataset;
        this.operator = operator;
        this.rightDataset = rightDataset;
        switch (operator) {
            case LEFT_JOIN_MANY:
                this.arrayName = rightDataset.retrieveArrayName();
                break;
            case RIGHT_JOIN_MANY:
                this.arrayName = leftDataset.retrieveArrayName();
                break;
            default:
                this.arrayName = null;
                break;
        }
    }

    Josson apply(final Function<String, JsonNode> evaluateQuery) {
        leftDataset.apply("left", evaluateQuery);
        rightDataset.apply("right", evaluateQuery);
        switch (operator) {
            case INNER_JOIN_ONE:
                if (leftDataset.node.isObject() || !rightDataset.node.isObject()) {
                    break;
                }
                swapDataset();
                break;
            case RIGHT_JOIN_ONE:
                operator = JoinOperator.LEFT_JOIN_ONE;
                swapDataset();
                break;
            case RIGHT_JOIN_MANY:
                operator = JoinOperator.LEFT_JOIN_MANY;
                swapDataset();
                break;
            case RIGHT_CONCATENATE:
                operator = JoinOperator.LEFT_CONCATENATE;
                swapDataset();
                break;
        }
        return Josson.create(joinNodes());
    }

    private void swapDataset() {
        final Dataset tempDataset = leftDataset;
        leftDataset = rightDataset;
        rightDataset = tempDataset;
    }

    private JsonNode joinNodes() {
        if (operator == JoinOperator.LEFT_CONCATENATE) {
            if (leftDataset.node.isObject() && rightDataset.node.isObject()) {
                return cloneObjectNode((ObjectNode) leftDataset.node).setAll((ObjectNode) rightDataset.node);
            } else if (leftDataset.node.isArray() && rightDataset.node.isArray()) {
                return cloneArrayNode((ArrayNode) leftDataset.node).addAll((ArrayNode) rightDataset.node);
            }
            throw new IllegalArgumentException("cannot concatenate an object and an array");
        }
        final ArrayNode rightArray;
        if (rightDataset.node.isArray()) {
            rightArray = (ArrayNode) rightDataset.node;
        } else {
            rightArray = MAPPER.createArrayNode();
            rightArray.add(rightDataset.node);
        }
        if (leftDataset.node.isObject()) {
            final ObjectNode joinedObject = joinToObjectNode((ObjectNode) leftDataset.node, rightArray);
            if (joinedObject == null) {
                throw new IllegalArgumentException("invalid data");
            }
            return joinedObject;
        }
        final ArrayNode joinedArray = MAPPER.createArrayNode();
        for (int i = 0; i < leftDataset.node.size(); i++) {
            if (leftDataset.node.get(i).isObject()) {
                final ObjectNode joinedNode = joinToObjectNode((ObjectNode) leftDataset.node.get(i), rightArray);
                if (joinedNode != null) {
                    joinedArray.add(joinedNode);
                }
            }
        }
        return joinedArray;
    }

    private ObjectNode joinToObjectNode(final ObjectNode leftObject, final ArrayNode rightArray) {
        final String[] relationalOps = new String[leftDataset.keys.length];
        for (int j = leftDataset.keys.length - 1; j >= 0; j--) {
            final JsonNode leftValue = getNodeByPath(leftObject, leftDataset.keys[j]);
            if (leftValue == null || !leftValue.isValueNode()) {
                return null;
            }
            relationalOps[j] = rightDataset.keys[j] + Operator.EQ.getSymbol()
                    + (leftValue.isTextual() ? QUOTE_SYMBOL : "")
                    + leftValue.asText().replace("'", "''")
                    + (leftValue.isTextual() ? QUOTE_SYMBOL : "");
        }
        final String path = String.format("[%s]", StringUtils.join(relationalOps, Operator.AND.getSymbol()));
        if (operator == JoinOperator.LEFT_JOIN_MANY) {
            final JsonNode rightToJoin = getNodeByPath(rightArray, path + FILTRATE_COLLECT_ALL.getSymbol());
            if (rightToJoin != null) {
                return cloneObjectNode(leftObject).set(arrayName, rightToJoin);
            }
        } else {
            final JsonNode rightToJoin = getNodeByPath(rightArray, path);
            if (rightToJoin != null && rightToJoin.isObject()) {
                return cloneObjectNode(leftObject).setAll((ObjectNode) rightToJoin);
            }
            if (operator == JoinOperator.INNER_JOIN_ONE) {
                return null;
            }
        }
        return leftObject;
    }

    /**
     * Defines left or right dataset for join operation.
     */
    static class Dataset {

        private final String query;

        private final String[] keys;

        private JsonNode node;
        
        Dataset(final String query, final String[] keys) {
            this.query = query;
            this.keys = keys;
        }

        String[] getKeys() {
            return keys;
        }

        private String retrieveArrayName() {
            final int pos = keys[0].indexOf(':');
            if (pos < 0) {
                try {
                    return getLastElementName(query);
                } catch (UnknownFormatConversionException e) {
                    return e.getConversion();
                }
            }
            final String arrayName = keys[0].substring(0, pos).trim();
            checkElementName(arrayName);
            keys[0] = keys[0].substring(pos + 1);
            return arrayName;
        }

        private void apply(final String side, final Function<String, JsonNode> evaluateQuery) {
            node = evaluateQuery.apply(query);
            if (node == null) {
                throw new IllegalArgumentException("unresolvable " + side + " side");
            }
            if (!node.isContainerNode()) {
                throw new IllegalArgumentException(side + " side is not a container node");
            }
        }
    }
}

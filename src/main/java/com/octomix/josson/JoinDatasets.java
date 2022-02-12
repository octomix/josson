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

import static com.octomix.josson.ArrayFilter.FilterMode.FILTRATE_COLLECT_ALL;
import static com.octomix.josson.JossonCore.QUOTE_SYMBOL;
import static com.octomix.josson.JossonCore.getNodeByPath;
import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.PatternMatcher.*;

class JoinDatasets {

    private Dataset leftDataset;
    private JoinOperator operator;
    private Dataset rightDataset;
    private final String arrayName;

    enum JoinOperator {
        INNER_JOIN_ONE(">=<"),
        LEFT_JOIN_ONE("<=<"),
        RIGHT_JOIN_ONE(">=>"),
        LEFT_JOIN_MANY("<=<<"),
        RIGHT_JOIN_MANY(">>=>");

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

    Dataset getLeftDataset() {
        return leftDataset;
    }

    Dataset getRightDataset() {
        return rightDataset;
    }

    JsonNode joinNodes(JsonNode leftNode, JsonNode rightNode) {
        switch (operator) {
            case INNER_JOIN_ONE:
                if (leftNode.isObject() || !rightNode.isObject()) {
                    break;
                }
                // fallthrough
            case RIGHT_JOIN_ONE:
            case RIGHT_JOIN_MANY:
                final JsonNode swapNode = leftNode;
                leftNode = rightNode;
                rightNode = swapNode;
                final Dataset swapDataset = leftDataset;
                leftDataset = rightDataset;
                rightDataset = swapDataset;
                switch (operator) {
                    case RIGHT_JOIN_ONE:
                        operator = JoinOperator.LEFT_JOIN_ONE;
                        break;
                    case RIGHT_JOIN_MANY:
                        operator = JoinOperator.LEFT_JOIN_MANY;
                        break;
                }
        }
        final ArrayNode rightArray;
        if (rightNode.isArray()) {
            rightArray = (ArrayNode) rightNode;
        } else {
            rightArray = MAPPER.createArrayNode();
            rightArray.add(rightNode);
        }
        if (leftNode.isObject()) {
            return joinToObjectNode((ObjectNode) leftNode, rightArray);
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
        final String path = "[" + StringUtils.join(relationalOps, Operator.AND.getSymbol()) + "]";
        if (operator == JoinOperator.LEFT_JOIN_MANY) {
            final JsonNode rightToJoin = getNodeByPath(rightArray, path + FILTRATE_COLLECT_ALL.getSymbol());
            if (rightToJoin != null) {
                final ObjectNode joinedNode = leftObject.deepCopy();
                joinedNode.set(arrayName, rightToJoin);
                return joinedNode;
            }
        } else {
            final JsonNode rightToJoin = getNodeByPath(rightArray, path);
            if (rightToJoin != null && rightToJoin.isObject()) {
                final ObjectNode joinedNode = leftObject.deepCopy();
                joinedNode.setAll((ObjectNode) rightToJoin);
                return joinedNode;
            }
            if (operator == JoinOperator.INNER_JOIN_ONE) {
                return null;
            }
        }
        return leftObject;
    }

    static class Dataset {

        private final String query;
        private final String[] keys;

        Dataset(final String query, final String[] keys) {
            this.query = query;
            this.keys = keys;
        }

        String getQuery() {
            return query;
        }

        String[] getKeys() {
            return keys;
        }

        private String retrieveArrayName() {
            final int pos = keys[0].indexOf(':');
            if (pos < 0) {
                return getLastElementName(query);
            }
            final String arrayName = keys[0].substring(0, pos).trim();
            checkElementName(arrayName);
            keys[0] = keys[0].substring(pos + 1);
            return arrayName;
        }
    }
}

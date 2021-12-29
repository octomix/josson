package com.octomix.josson;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.octomix.josson.commons.StringUtils;

import java.util.List;

import static com.octomix.josson.ArrayFilter.FilterMode.FILTER_FIND_ALL;
import static com.octomix.josson.JossonCore.QUOTE_SYMBOL;
import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.PatternMatcher.*;

class JoinDatasets {

    private enum JoinOperator {
        INNER_JOIN_ONE(">=<"),
        LEFT_JOIN_ONE("<=<"),
        RIGHT_JOIN_ONE(">=>"),
        LEFT_JOIN_MANY("<=<<"),
        RIGHT_JOIN_MANY(">>=>");

        private final String symbol;

        JoinOperator(String symbol) {
            this.symbol = symbol;
        }

        private static JoinOperator fromSymbol(String symbol) {
            for (JoinOperator operator : values()) {
                if (operator.symbol.equals(symbol)) {
                    return operator;
                }
            }
            return null;
        }
    }

    static class Dataset {
        private final String query;
        private final String[] keys;

        Dataset(String query, String[] keys) {
            this.query = query;
            this.keys = keys;
        }

        String getQuery() {
            return query;
        }

        private String retrieveArrayName() {
            int pos = keys[0].indexOf(':');
            if (pos < 0) {
                return getLastElementName(query);
            }
            String arrayName = keys[0].substring(0, pos).trim();
            checkElementName(arrayName);
            keys[0] = keys[0].substring(pos + 1);
            return arrayName;
        }
    }

    private Dataset leftDataset;
    private JoinOperator operator;
    private Dataset rightDataset;
    private final String arrayName;

    private JoinDatasets(Dataset leftDataset, JoinOperator operator, Dataset rightDataset) {
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
        }
    }

    Dataset getLeftDataset() {
        return leftDataset;
    }

    Dataset getRightDataset() {
        return rightDataset;
    }

    static JoinDatasets fromStatement(String statement) {
        List<LogicalOpStep> opSteps = decomposeStatement(statement);
        if (opSteps.size() < 2) {
            return null;
        }
        JoinOperator operator = JoinOperator.fromSymbol(opSteps.get(1).getOperator());
        if (operator == null) {
            return null;
        }
        if (opSteps.size() > 2) {
            throw new IllegalArgumentException("too many arguments");
        }
        JoinDatasets.Dataset leftDataset = matchJoinDatasetQuery(opSteps.get(0).getExpression());
        JoinDatasets.Dataset rightDataset = matchJoinDatasetQuery(opSteps.get(1).getExpression());
        if (leftDataset.keys.length != rightDataset.keys.length) {
            throw new IllegalArgumentException("mismatch key count");
        }
        return new JoinDatasets(leftDataset, operator, rightDataset);
    }

    JsonNode joinNodes(JsonNode leftNode, JsonNode rightNode) {
        if (operator == JoinOperator.RIGHT_JOIN_ONE || operator == JoinOperator.RIGHT_JOIN_MANY
                || (operator == JoinOperator.INNER_JOIN_ONE && !leftNode.isObject() && rightNode.isObject())) {
            JsonNode swapNode = leftNode;
            leftNode = rightNode;
            rightNode = swapNode;
            Dataset swapDataset = leftDataset;
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
        ArrayNode rightArray;
        if (rightNode.isArray()) {
            rightArray = (ArrayNode) rightNode;
        } else {
            rightArray = MAPPER.createArrayNode();
            rightArray.add(rightNode);
        }
        if (leftNode.isObject()) {
            return joinToObjectNode((ObjectNode) leftNode, rightArray);
        }
        ArrayNode joinedArray = MAPPER.createArrayNode();
        for (int i = 0; i < leftNode.size(); i++) {
            if (leftNode.get(i).isObject()) {
                ObjectNode joinedNode = joinToObjectNode((ObjectNode) leftNode.get(i), rightArray);
                if (joinedNode != null) {
                    joinedArray.add(joinedNode);
                }
            }
        }
        return joinedArray;
    }

    private ObjectNode joinToObjectNode(ObjectNode leftObject, ArrayNode rightArray) {
        String[] relationalOps = new String[leftDataset.keys.length];
        for (int j = leftDataset.keys.length - 1; j >= 0; j--) {
            JsonNode leftValue = Josson.getNode(leftObject, leftDataset.keys[j]);
            if (leftValue == null || !leftValue.isValueNode()) {
                return null;
            }
            relationalOps[j] = rightDataset.keys[j] + "="
                    + (leftValue.isTextual() ? QUOTE_SYMBOL : "")
                    + leftValue.asText().replace("'", "''")
                    + (leftValue.isTextual() ? QUOTE_SYMBOL : "");
        }
        String path = "[" + StringUtils.join(relationalOps, " & ") + "]";
        if (operator == JoinOperator.LEFT_JOIN_MANY) {
            JsonNode rightToJoin = Josson.getNode(rightArray, path + FILTER_FIND_ALL.symbol);
            if (rightToJoin != null) {
                ObjectNode joinedNode = leftObject.deepCopy();
                joinedNode.set(arrayName, rightToJoin);
                return joinedNode;
            }
        } else {
            JsonNode rightToJoin = Josson.getNode(rightArray, path);
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

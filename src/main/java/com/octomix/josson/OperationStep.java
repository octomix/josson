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
import com.fasterxml.jackson.databind.node.BooleanNode;
import com.fasterxml.jackson.databind.node.NullNode;
import com.octomix.josson.exception.UnresolvedDatasetException;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.PatternMatcher.matchDatasetQuery;

class OperationStep {

    private Operator operator;
    private String expression;
    private JsonNode resolved;

    OperationStep(Operator operator, String expression) {
        this.operator = operator;
        this.expression = expression;
        this.resolved = null;
    }

    Operator getOperator() {
        return operator;
    }

    void resetOperator() {
        operator = Operator.NOP;
    }

    String getExpression() {
        return expression;
    }

    void setResolved(JsonNode resolved) {
        this.expression = null;
        this.resolved = resolved;
    }

    private static boolean relationalCompare(JsonNode leftNode, Operator operator, JsonNode rightNode) {
        if (leftNode == null) {
            leftNode = NullNode.getInstance();
        }
        if (rightNode == null) {
            rightNode = NullNode.getInstance();
        }
        if (leftNode.isContainerNode() || rightNode.isContainerNode()) {
            if (leftNode.getNodeType() != rightNode.getNodeType() || (operator != Operator.EQ && operator != Operator.NE)) {
                return false;
            }
            int size = leftNode.size();
            if (size != rightNode.size()) {
                return operator == Operator.NE;
            }
            if (leftNode.isArray()) {
                List<JsonNode> rightArray = new ArrayList<>();
                for (int j = 0; j < size; j++) {
                    if (!rightNode.get(j).isValueNode()) {
                        return operator == Operator.NE;
                    }
                    rightArray.add(rightNode.get(j));
                }
                for (int i = size - 1; i >= 0; i--) {
                    JsonNode leftElem = leftNode.get(i);
                    if (!leftElem.isValueNode()) {
                        return false;
                    }
                    int j = i;
                    for (; j >= 0; j--) {
                        if (relationalCompare(leftElem, Operator.EQ, rightArray.get(j))) {
                            break;
                        }
                    }
                    if (j < 0) {
                        return operator == Operator.NE;
                    }
                    rightArray.remove(j);
                }
            } else {
                Iterator<Map.Entry<String, JsonNode>> iterator = leftNode.fields();
                while (iterator.hasNext()) {
                    Map.Entry<String, JsonNode> leftElem = iterator.next();
                    if (!relationalCompare(leftElem.getValue(), Operator.EQ, rightNode.get(leftElem.getKey()))) {
                        return operator == Operator.NE;
                    }
                }
            }
            return operator == Operator.EQ;
        }
        if (rightNode.isTextual()) {
            if (leftNode.isTextual()) {
                int compareResult = leftNode.asText().compareTo(rightNode.asText());
                switch (operator) {
                    case EQ:
                        return compareResult == 0;
                    case NE:
                        return compareResult != 0;
                    case GT:
                        return compareResult > 0;
                    case GTE:
                        return compareResult >= 0;
                    case LT:
                        return compareResult < 0;
                    case LTE:
                        return compareResult <= 0;
                }
            }
            JsonNode swap = leftNode;
            leftNode = rightNode;
            rightNode = swap;
            switch (operator) {
                case GT:
                    operator = Operator.LT;
                    break;
                case GTE:
                    operator = Operator.LTE;
                    break;
                case LT:
                    operator = Operator.GT;
                    break;
                case LTE:
                    operator = Operator.GTE;
                    break;
            }
        }
        if (rightNode.isNumber()) {
            try {
                double value = leftNode.isNumber() ? leftNode.asDouble() : Double.parseDouble(leftNode.asText());
                switch (operator) {
                    case EQ:
                        return value == rightNode.asDouble();
                    case NE:
                        return value != rightNode.asDouble();
                    case GT:
                        return value > rightNode.asDouble();
                    case GTE:
                        return value >= rightNode.asDouble();
                    case LT:
                        return value < rightNode.asDouble();
                    case LTE:
                        return value <= rightNode.asDouble();
                }
            } catch (NumberFormatException e) {
                return false;
            }
        }
        if (rightNode.isBoolean()) {
            switch (operator) {
                case EQ:
                    return !leftNode.asBoolean() ^ rightNode.asBoolean();
                case NE:
                    return leftNode.asBoolean() ^ rightNode.asBoolean();
            }
        } else {
            switch (operator) {
                case EQ:
                    return leftNode.isNull() && rightNode.isNull();
                case NE:
                    return leftNode.isNull() ^ rightNode.isNull();
            }
        }
        return false;
    }

    private JsonNode evaluateExpression(Map<String, Josson> datasets) throws UnresolvedDatasetException {
        try {
            return toValueNode(expression);
        } catch (NumberFormatException e) {
            // continue
        }
        if (datasets.containsKey(expression)) {
            Josson josson = datasets.get(expression);
            if (josson == null) {
                return null;
            }
            return josson.getNode();
        }
        JsonNode implicitVariable = getImplicitVariable(expression);
        if (implicitVariable != null) {
            return implicitVariable;
        }
        String[] tokens = matchDatasetQuery(expression);
        if (tokens == null) {
            throw new UnresolvedDatasetException(expression);
        }
        Josson josson;
        if (datasets.containsKey(tokens[0])) {
            josson = datasets.get(tokens[0]);
            if (josson == null) {
                return null;
            }
        } else {
            implicitVariable = getImplicitVariable(tokens[0]);
            if (implicitVariable == null) {
                throw new UnresolvedDatasetException(tokens[0]);
            }
            josson = Josson.create(implicitVariable);
        }
        JsonNode node = josson.getNode(tokens[1]);
        datasets.put(expression, node == null ? null : Josson.create(node));
        return node;
    }

    /*
        For JossonCore.evaluateFilter()
     */
    JsonNode resolveFrom(Josson arrayNode, int arrayIndex) {
        if (expression != null) {
            setResolved(arrayNode.getNode(arrayIndex, expression));
        }
        return resolved;
    }

    boolean isResolveToTrueFrom(Josson arrayNode, int arrayIndex) {
        JsonNode node = resolveFrom(arrayNode, arrayIndex);
        return node != null && node.asBoolean();
    }

    boolean isResolveToFalseFrom(Josson arrayNode, int arrayIndex) {
        JsonNode node = resolveFrom(arrayNode, arrayIndex);
        return node != null && !node.asBoolean();
    }

    JsonNode relationalCompare(OperationStep step, Josson arrayNode, int arrayIndex) {
        resolved = BooleanNode.valueOf(relationalCompare(
                resolveFrom(arrayNode, arrayIndex), step.getOperator(), arrayNode.getNode(arrayIndex, step.getExpression())));
        return resolved;
    }

    /*
        For Jossons.evaluateStatement()
     */
    JsonNode resolveFrom(Map<String, Josson> datasets) throws UnresolvedDatasetException {
        if (expression != null) {
            setResolved(evaluateExpression(datasets));
        }
        return resolved;
    }

    boolean isResolveToTrueFrom(Map<String, Josson> datasets) throws UnresolvedDatasetException {
        JsonNode node = resolveFrom(datasets);
        return node != null && node.asBoolean();
    }

    boolean isResolveToFalseFrom(Map<String, Josson> datasets) throws UnresolvedDatasetException {
        JsonNode node = resolveFrom(datasets);
        return node != null && !node.asBoolean();
    }

    JsonNode relationalCompare(OperationStep step, Map<String, Josson> datasets) throws UnresolvedDatasetException {
        resolved = BooleanNode.valueOf(relationalCompare(
                resolveFrom(datasets), step.getOperator(), step.evaluateExpression(datasets)));
        return resolved;
    }
}

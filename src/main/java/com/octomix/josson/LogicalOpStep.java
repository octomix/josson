/*
 * Copyright 2020 Octomix Software Technology Limited
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
import com.fasterxml.jackson.databind.node.TextNode;
import com.octomix.josson.commons.StringUtils;
import com.octomix.josson.exception.UnresolvedDatasetException;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.Map;

import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.PatternMatcher.matchJsonQuery;
import static java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME;

class LogicalOpStep {

    private String operator;
    private String unresolved;
    private JsonNode resolved;

    private enum RelationalOperator {
        EQ("="),
        NE("!="),
        GT(">"),
        GTE(">="),
        LT("<"),
        LTE("<=");

        final String symbol;

        RelationalOperator(String symbol) {
            this.symbol = symbol;
        }

        private static RelationalOperator fromSymbol(String symbol) {
            for (RelationalOperator operator : values()) {
                if (operator.symbol.equals(symbol)) {
                    return operator;
                }
            }
            throw new IllegalArgumentException(symbol);
        }
    }

    LogicalOpStep(String operator, String unresolved) {
        this.operator = operator;
        this.unresolved = unresolved;
        this.resolved = null;
    }

    String getOperator() {
        return operator;
    }

    void resetOperator() {
        operator = "";
    }

    String getUnresolved() {
        return unresolved;
    }

    void setResolved(JsonNode resolved) {
        this.unresolved = null;
        this.resolved = resolved;
    }

    private static JsonNode evaluateExpression(String expression, Map<String, Josson> datasets)
            throws UnresolvedDatasetException {
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
        String[] tokens = matchJsonQuery(expression);
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

    private static JsonNode getImplicitVariable(String name) {
        if (name.charAt(0) == '$') {
            switch (StringUtils.stripStart(name.substring(1), null).toLowerCase()) {
                case "":
                    return BooleanNode.TRUE;
                case "now":
                    return TextNode.valueOf(LocalDateTime.now()
                            .format(ISO_LOCAL_DATE_TIME));
                case "today":
                    return TextNode.valueOf(LocalDateTime.now()
                            .truncatedTo(ChronoUnit.DAYS)
                            .format(ISO_LOCAL_DATE_TIME));
                case "yesterday":
                    return TextNode.valueOf(LocalDateTime.now()
                            .truncatedTo(ChronoUnit.DAYS)
                            .minusDays(1)
                            .format(ISO_LOCAL_DATE_TIME));
                case "tomorrow":
                    return TextNode.valueOf(LocalDateTime.now()
                            .truncatedTo(ChronoUnit.DAYS)
                            .plusDays(1)
                            .format(ISO_LOCAL_DATE_TIME));
            }
        }
        return null;
    }

    private static BooleanNode relationalCompare(JsonNode leftNode, String operator, JsonNode rightNode) {
        RelationalOperator oper = RelationalOperator.fromSymbol(operator);
        if (leftNode == null) {
            leftNode = NullNode.getInstance();
        }
        if (rightNode == null) {
            rightNode = NullNode.getInstance();
        }
        if (rightNode.isTextual()) {
            if (leftNode.isTextual()) {
                int compareResult = leftNode.asText().compareTo(rightNode.asText());
                switch (oper) {
                    case EQ:
                        return BooleanNode.valueOf(compareResult == 0);
                    case NE:
                        return BooleanNode.valueOf(compareResult != 0);
                    case GT:
                        return BooleanNode.valueOf(compareResult > 0);
                    case GTE:
                        return BooleanNode.valueOf(compareResult >= 0);
                    case LT:
                        return BooleanNode.valueOf(compareResult < 0);
                    case LTE:
                        return BooleanNode.valueOf(compareResult <= 0);
                }
            }
            JsonNode swap = leftNode;
            leftNode = rightNode;
            rightNode = swap;
            switch (oper) {
                case GT:
                    oper = RelationalOperator.LT;
                    break;
                case GTE:
                    oper = RelationalOperator.LTE;
                    break;
                case LT:
                    oper = RelationalOperator.GT;
                    break;
                case LTE:
                    oper = RelationalOperator.GTE;
                    break;
            }
        }
        if (!leftNode.isContainerNode() && rightNode.isNumber()) {
            try {
                double value = leftNode.isNumber() ? leftNode.asDouble() : Double.parseDouble(leftNode.asText());
                switch (oper) {
                    case EQ:
                        return BooleanNode.valueOf(value == rightNode.asDouble());
                    case NE:
                        return BooleanNode.valueOf(value != rightNode.asDouble());
                    case GT:
                        return BooleanNode.valueOf(value > rightNode.asDouble());
                    case GTE:
                        return BooleanNode.valueOf(value >= rightNode.asDouble());
                    case LT:
                        return BooleanNode.valueOf(value < rightNode.asDouble());
                    case LTE:
                        return BooleanNode.valueOf(value <= rightNode.asDouble());
                }
            } catch (NumberFormatException e) {
                return BooleanNode.FALSE;
            }
        }
        if (!leftNode.isContainerNode() && rightNode.isBoolean()) {
            switch (oper) {
                case EQ:
                    return BooleanNode.valueOf(!leftNode.asBoolean() ^ rightNode.asBoolean());
                case NE:
                    return BooleanNode.valueOf(leftNode.asBoolean() ^ rightNode.asBoolean());
            }
        } else {
            switch (oper) {
                case EQ:
                    return BooleanNode.valueOf(leftNode.isNull() && rightNode.isNull());
                case NE:
                    return BooleanNode.valueOf(leftNode.isNull() ^ rightNode.isNull());
            }
        }
        return BooleanNode.FALSE;
    }

    /*
        For JossonCore.evaluateFilter()
     */
    private static JsonNode getNodeFrom(Josson arrayNode, int index, String expression) {
        if (expression.equals("?")) {
            return arrayNode.getNode(index);
        }
        switch (expression.charAt(0)) {
            case '#':
                return getIndexId(index, expression);
            case '@':
                return arrayNode.getNode(expression);
        }
        return arrayNode.getNode(index, expression);
    }

    JsonNode resolveFrom(Josson arrayNode, int arrayIndex) {
        if (unresolved != null) {
            resolved = getNodeFrom(arrayNode, arrayIndex, unresolved);
            unresolved = null;
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

    JsonNode relationalCompare(String operator, String expression, Josson arrayNode, int arrayIndex) {
        resolved = relationalCompare(
                resolveFrom(arrayNode, arrayIndex), operator, getNodeFrom(arrayNode, arrayIndex, expression));
        return resolved;
    }

    /*
        For Jossons.evaluateStatement()
     */
    JsonNode resolveFrom(Map<String, Josson> datasets) throws UnresolvedDatasetException {
        if (unresolved != null) {
            resolved = evaluateExpression(unresolved, datasets);
            unresolved = null;
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

    JsonNode relationalCompare(String operator, String expression, Map<String, Josson> datasets)
            throws UnresolvedDatasetException {
        resolved = relationalCompare(
                resolveFrom(datasets), operator, evaluateExpression(expression, datasets));
        return resolved;
    }
}

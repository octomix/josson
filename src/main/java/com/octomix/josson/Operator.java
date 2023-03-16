/*
 * Copyright 2020-2023 Octomix Software Technology Limited
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
import com.fasterxml.jackson.databind.node.NullNode;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

/**
 * Defines relational and logical operators.
 */
enum Operator {

    /**
     * Equal.
     */
    EQ("="),

    /**
     * Not equal.
     */
    NE("!="),

    /**
     * Greater than.
     */
    GT(">"),

    /**
     * Greater than or equal.
     */
    GTE(">="),

    /**
     * Less than.
     */
    LT("<"),

    /**
     * Less than or equal.
     */
    LTE("<="),

    /**
     * Matches regular expression.
     */
    MATCH("=~"),

    /**
     * Matches regular expression.
     */
    NOT_MATCH("!=~"),

    /**
     * Not.
     */
    NOT("!"),

    /**
     * And.
     */
    AND("&"),

    /**
     * Or.
     */
    OR("|"),

    /**
     * No operation.
     */
    NOP("");

    private final String symbol;

    Operator(final String symbol) {
        this.symbol = symbol;
    }

    static Operator fromSymbol(final String symbol) {
        for (Operator operator : values()) {
            if (operator.symbol.equals(symbol)) {
                return operator;
            }
        }
        return null;
    }

    String getSymbol() {
        return symbol;
    }

    boolean compare(JsonNode leftNode, JsonNode rightNode) {
        if (leftNode == null) {
            leftNode = NullNode.getInstance();
        }
        if (rightNode == null) {
            rightNode = NullNode.getInstance();
        }
        if (this == MATCH || this == NOT_MATCH) {
            if (!leftNode.isValueNode() || !rightNode.isTextual()) {
                return false;
            }
            return this == NOT_MATCH ^ Pattern.compile(rightNode.asText()).matcher(leftNode.asText()).matches();
        }
        if (leftNode.isContainerNode() || rightNode.isContainerNode()) {
            return compareContainer(leftNode, rightNode);
        }
        if (rightNode.isTextual()) {
            if (leftNode.isTextual()) {
                final int compareResult = leftNode.asText().compareTo(rightNode.asText());
                switch (this) {
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
            switch (this) {
                case GT:
                    return LT.compareValue(rightNode, leftNode);
                case GTE:
                    return LTE.compareValue(rightNode, leftNode);
                case LT:
                    return GT.compareValue(rightNode, leftNode);
                case LTE:
                    return GTE.compareValue(rightNode, leftNode);
            }
            return compareValue(rightNode, leftNode);
        }
        return compareValue(leftNode, rightNode);
    }

    private boolean compareValue(final JsonNode leftNode, final JsonNode rightNode) {
        if (rightNode.isNumber()) {
            try {
                final double value = leftNode.isNumber() ? leftNode.asDouble() : Double.parseDouble(leftNode.asText());
                switch (this) {
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
        switch (this) {
            case EQ:
                if (rightNode.isBoolean()) {
                    return leftNode.asBoolean() == rightNode.asBoolean();
                }
                return leftNode.isNull() && rightNode.isNull();
            case NE:
                if (rightNode.isBoolean()) {
                    return leftNode.asBoolean() != rightNode.asBoolean();
                }
                return !(leftNode.isNull() && rightNode.isNull());
        }
        return false;
    }

    private boolean compareContainer(final JsonNode leftNode, final JsonNode rightNode) {
        if (leftNode.getNodeType() != rightNode.getNodeType() || this != EQ && this != NE) {
            return this == NE;
        }
        final int size = leftNode.size();
        if (size != rightNode.size()) {
            return this == NE;
        }
        if (leftNode.isArray()) {
            final List<JsonNode> rightArray = new ArrayList<>();
            rightNode.forEach(rightArray::add);
            for (int i = size - 1; i >= 0; i--) {
                final JsonNode leftElem = leftNode.get(i);
                int j = i;
                for (; j >= 0; j--) {
                    if (EQ.compare(leftElem, rightArray.get(j))) {
                        break;
                    }
                }
                if (j < 0) {
                    return this == NE;
                }
                rightArray.remove(j);
            }
        } else {
            final Iterator<Map.Entry<String, JsonNode>> iterator = leftNode.fields();
            while (iterator.hasNext()) {
                final Map.Entry<String, JsonNode> leftElem = iterator.next();
                if (!EQ.compare(leftElem.getValue(), rightNode.get(leftElem.getKey()))) {
                    return this == NE;
                }
            }
        }
        return this == EQ;
    }
}

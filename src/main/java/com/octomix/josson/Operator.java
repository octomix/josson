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
}

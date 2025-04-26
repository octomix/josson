/*
 * Copyright 2020-2025 Choi Wai Man Raymond
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
 * Dataset join and set operators.
 */
enum CombineOperator {

    /**
     * Inner join.
     */
    INNER_JOIN(">=<"),

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
    INTERSECTION(">n<"),

    /**
     * Equals, works on two objects or two arrays. Returns true or false.
     */
    EQUALS("<==>"),

    /**
     * Not equals, works on two objects or two arrays. Returns true or false.
     */
    NOT_EQUALS("<!=>");

    private final String symbol;

    CombineOperator(final String symbol) {
        this.symbol = symbol;
    }

    static CombineOperator fromSymbol(final String symbol) {
        for (CombineOperator operator : values()) {
            if (operator.symbol.equals(symbol)) {
                return operator;
            }
        }
        return null;
    }

    static int findEndingPos(final String input, final int beg, final int last) {
        int pos = beg;
        for (; pos <= last; pos++) {
            if ("<=>!+-un".indexOf(input.charAt(pos)) < 0) {
                return pos;
            }
        }
        return pos;
    }
}

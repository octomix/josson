/*
 * Copyright 2020-2024 Octomix Software Technology Limited
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

package com.octomix.josson.exception;

/**
 * Thrown to indicate that there is a syntax error in a query statement.
 */
public class SyntaxErrorException extends IllegalArgumentException {

    /**
     * The error is at the end of the statement.
     */
    public static final int POS_AT_THE_END = -1;

    /**
     * Thrown to indicate that there is a syntax error in a query statement.
     *
     * @param statement the invalid statement
     */
    public SyntaxErrorException(final String statement) {
        super("Invalid statement: " + statement);
    }

    /**
     * Thrown to indicate that there is a syntax error in a query statement.
     *
     * @param statement the invalid statement
     * @param message the error message
     */
    public SyntaxErrorException(final String statement, final String message) {
        super(String.format("%s: %s", message, statement));
    }

    /**
     * Thrown to indicate that there is a syntax error in a query statement with position.
     *
     * @param statement the invalid statement
     * @param message the error message
     * @param pos the position where the error occur
     */
    public SyntaxErrorException(final String statement, final String message, final int pos) {
        super(String.format("%s %s: %s", message, pos == POS_AT_THE_END ? "at the end" : "at position " + pos, statement));
    }

    /**
     * Thrown to indicate that there is a syntax error in a query statement.
     *
     * @param statement the invalid statement
     * @param e {@code IllegalArgumentException} that contains the error information
     */
    public SyntaxErrorException(final String statement, final IllegalArgumentException e) {
        super(e.getMessage() == null ? statement : String.format("\"%s\" in %s", e.getMessage(), statement));
    }
}

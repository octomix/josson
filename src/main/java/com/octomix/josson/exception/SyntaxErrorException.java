package com.octomix.josson.exception;

/**
 * Thrown to indicate that there is a syntax error in a query statement.
 */
public class SyntaxErrorException extends IllegalArgumentException {

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
        super(String.format("%s %s: %s", message, pos == -1 ? "at the end" : "at position " + pos, statement));
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

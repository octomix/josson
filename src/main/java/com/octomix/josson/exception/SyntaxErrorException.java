package com.octomix.josson.exception;

/**
 * Thrown to indicate that there is a syntax error in a query statement.
 */
public class SyntaxErrorException extends IllegalArgumentException {
    public SyntaxErrorException(final String statement) {
        super("Invalid statement: " + statement);
    }

    public SyntaxErrorException(final String statement, final String message) {
        super(message + ": " + statement);
    }

    public SyntaxErrorException(final String statement, final String message, final int pos) {
        super(message + (pos == -1 ? " at the end" : " at position " + pos) + ": " + statement);
    }

    public SyntaxErrorException(final String statement, final IllegalArgumentException e) {
        super(e.getMessage() == null ? statement : "\"" + e.getMessage() + "\" in " + statement);
    }
}

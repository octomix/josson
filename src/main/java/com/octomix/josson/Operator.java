package com.octomix.josson;

enum Operator {
    EQ("="),
    NE("!="),
    GT(">"),
    GTE(">="),
    LT("<"),
    LTE("<="),
    NOT("!"),
    AND("&"),
    OR("|"),
    NOP("");

    final String symbol;

    Operator(String symbol) {
        this.symbol = symbol;
    }

    static Operator fromSymbol(String symbol) {
        for (Operator operator : values()) {
            if (operator.symbol.equals(symbol)) {
                return operator;
            }
        }
        return null;
    }
}

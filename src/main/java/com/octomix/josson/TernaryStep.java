package com.octomix.josson;

class TernaryStep {

    private final String statement;
    private final String ifTrueValue;

    TernaryStep(String statement, String ifTrueValue) {
        this.statement = statement;
        this.ifTrueValue = ifTrueValue;
    }

    String getStatement() {
        return statement;
    }

    String getIfTrueValue() {
        return ifTrueValue;
    }
}

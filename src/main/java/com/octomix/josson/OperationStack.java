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
import com.octomix.josson.exception.SyntaxErrorException;

import java.util.LinkedList;

import static com.octomix.josson.PatternMatcher.decomposeStatement;

/**
 * Abstract class of logical operations on a stack of OperationStep.
 */
abstract class OperationStack {

    private final LinkedList<OperationStep> stack = new LinkedList<>();

    private OperationStep lastStep;

    abstract protected JsonNode evaluateExpression(final OperationStep step, final int arrayIndex);

    void clear() {
        stack.clear();
    }

    private void pushStep(final OperationStep step) {
        stack.addLast(step);
        lastStep = step;
    }

    private OperationStep popStep() {
        final OperationStep step = stack.removeLast();
        lastStep = stack.peekLast();
        return step;
    }

    private JsonNode evaluateSteps(final boolean inParentheses, final int arrayIndex) {
        final LinkedList<OperationStep> iterator;
        if (inParentheses) {
            iterator = new LinkedList<>();
            while (!stack.isEmpty()) {
                if ("(".equals(lastStep.getExpression())) {
                    break;
                }
                iterator.addFirst(popStep());
            }
            if (lastStep == null) {
                throw new IllegalArgumentException(")");
            }
        } else {
            iterator = stack;
        }
        JsonNode node = null;
        boolean result = true;
        for (OperationStep step : iterator) {
            if (step.getOperator() == Operator.OR) {
                if (result) {
                    node = BooleanNode.TRUE;
                    break;
                }
                result = true;
            }
            if (result && !"".equals(step.getExpression())) {
                node = resolveFrom(step, arrayIndex);
                if (step.getOperator() == Operator.NOT) {
                    result = node == null || node.isNull() || node.isValueNode() && !node.asBoolean();
                    node = BooleanNode.valueOf(result);
                } else {
                    result = node != null && node.asBoolean();
                }
            }
        }
        if (inParentheses) {
            if (lastStep.getOperator() == Operator.NOT) {
                lastStep.resetOperator();
                node = BooleanNode.valueOf(!result);
            }
            lastStep.setResolved(node);
        }
        return node;
    }

    private JsonNode resolveFrom(final OperationStep step, final int arrayIndex) {
        if (step.getExpression() != null) {
            step.setResolved(evaluateExpression(step, arrayIndex));
        }
        return step.getResolved();
    }

    private boolean isResolvedTo(final boolean expected, final int arrayIndex) {
        final JsonNode node = resolveFrom(lastStep, arrayIndex);
        return node != null && (node.asBoolean() == expected);
    }

    private JsonNode relationalCompare(final OperationStep prevStep, final OperationStep step, final int arrayIndex) {
        return BooleanNode.valueOf(OperationStep.relationalCompare(
                resolveFrom(prevStep, arrayIndex),
                step.getOperator(),
                evaluateExpression(step, arrayIndex)));
    }

    JsonNode evaluate(final String statement, final int arrayIndex) {
        for (OperationStep step : decomposeStatement(statement)) {
            try {
                evaluate(step, arrayIndex);
            } catch (IllegalArgumentException e) {
                throw new SyntaxErrorException(statement, e);
            }
        }
        return evaluateSteps(false, arrayIndex);
    }

    private void evaluate(final OperationStep step, final int arrayIndex) {
        switch (step.getOperator()) {
            case NOT:
            case NOP:
                if (")".equals(step.getExpression())) {
                    evaluateSteps(true, arrayIndex);
                } else {
                    pushStep(step);
                }
                return;
        }
        if (stack.isEmpty()) {
            throw new IllegalArgumentException();
        }
        if ("(".equals(lastStep.getExpression())) {
            pushStep(step);
            return;
        }
        switch (step.getOperator()) {
            case AND:
            case OR:
                if (lastStep.getOperator() == Operator.AND) {
                    final OperationStep thisStep = popStep();
                    if (isResolvedTo(true, arrayIndex)) {
                        lastStep.setResolved(resolveFrom(thisStep, arrayIndex));
                    }
                } else if (lastStep.getOperator() == Operator.OR && step.getOperator() == Operator.OR) {
                    final OperationStep thisStep = popStep();
                    if (isResolvedTo(false, arrayIndex)) {
                        lastStep.setResolved(resolveFrom(thisStep, arrayIndex));
                    }
                }
                pushStep(step);
                return;
        }
        if (lastStep.getOperator() == Operator.AND) {
            final OperationStep thisStep = popStep();
            if (isResolvedTo(true, arrayIndex)) {
                lastStep.setResolved(relationalCompare(thisStep, step, arrayIndex));
            }
        } else {
            lastStep.setResolved(relationalCompare(lastStep, step, arrayIndex));
        }
    }
}

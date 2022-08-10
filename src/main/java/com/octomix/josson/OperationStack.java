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
import java.util.function.BiFunction;

import static com.octomix.josson.JossonsCore.antiInjectionDecode;
import static com.octomix.josson.PatternMatcher.decomposeStatement;
import static com.octomix.josson.Utils.asBoolean;
import static com.octomix.josson.commons.StringUtils.EMPTY;

/**
 * Abstract class of logical operations on a stack of OperationStep.
 */
abstract class OperationStack {

    private static final String OPEN_PARENTHESIS = "(";

    private static final String CLOSE_PARENTHESIS = ")";

    private final LinkedList<OperationStep> stack = new LinkedList<>();

    protected final boolean isAntiInject;

    private OperationStep lastStep;

    abstract protected JsonNode evaluateExpression(final OperationStep step, final int arrayIndex);

    protected OperationStack(final boolean isAntiInject) {
        this.isAntiInject = isAntiInject;
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
                if (OPEN_PARENTHESIS.equals(lastStep.getExpression())) {
                    break;
                }
                iterator.addFirst(popStep());
            }
            if (lastStep == null) {
                throw new IllegalArgumentException(CLOSE_PARENTHESIS);
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
            if (result && !EMPTY.equals(step.getExpression())) {
                node = resolveFrom(step, arrayIndex);
                if (step.getOperator() == Operator.NOT) {
                    result = !asBoolean(node);
                    node = BooleanNode.valueOf(result);
                } else {
                    result = asBoolean(node);
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

    private void popAndResolveIf(final boolean ifPrevResult, final int arrayIndex,
                                 final BiFunction<OperationStep, Integer, JsonNode> value) {
        final OperationStep thisStep = popStep();
        if (lastStep.getOperator() == Operator.NOT) {
            if (asBoolean(resolveFrom(lastStep, arrayIndex)) != ifPrevResult) {
                lastStep.resetOperator();
                lastStep.setResolved(value.apply(thisStep, arrayIndex));
            }
        } else {
            if (asBoolean(resolveFrom(lastStep, arrayIndex)) == ifPrevResult) {
                lastStep.setResolved(value.apply(thisStep, arrayIndex));
            }
        }
    }

    private JsonNode relationalCompare(final OperationStep prevStep, final OperationStep step, final int arrayIndex) {
        return BooleanNode.valueOf(step.getOperator().relationalCompare(
                resolveFrom(prevStep, arrayIndex), evaluateExpression(step, arrayIndex)));
    }

    JsonNode evaluate(final String statement, final int arrayIndex) {
        stack.clear();
        for (OperationStep step : decomposeStatement(isAntiInject ? antiInjectionDecode(statement) : statement)) {
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
                if (CLOSE_PARENTHESIS.equals(step.getExpression())) {
                    evaluateSteps(true, arrayIndex);
                } else {
                    pushStep(step);
                }
                return;
        }
        if (stack.isEmpty()) {
            throw new IllegalArgumentException();
        }
        if (OPEN_PARENTHESIS.equals(lastStep.getExpression())) {
            pushStep(step);
        } else if (step.getOperator() == Operator.AND || step.getOperator() == Operator.OR) {
            if (lastStep.getOperator() == Operator.AND) {
                popAndResolveIf(true, arrayIndex, this::resolveFrom);
            } else if (lastStep.getOperator() == Operator.OR && step.getOperator() == Operator.OR) {
                popAndResolveIf(false, arrayIndex, this::resolveFrom);
            }
            pushStep(step);
        } else if (lastStep.getOperator() == Operator.AND) {
            popAndResolveIf(true, arrayIndex, (thisStep, index) -> relationalCompare(thisStep, step, index));
        } else {
            lastStep.setResolved(relationalCompare(lastStep, step, arrayIndex));
        }
    }
}

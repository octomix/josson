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
import com.octomix.josson.exception.UnresolvedDatasetException;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.PatternMatcher.decomposeStatement;

/**
 * Logical operations on a stack of OperationStep.
 */
class OperationStack {

    private final Josson arrayNode;

    private final Map<String, Josson> datasets;

    private final LinkedList<OperationStep> stack = new LinkedList<>();

    private OperationStep lastStep;

    OperationStack(final JsonNode node) {
        this.arrayNode = node.isArray()
                ? Josson.create(node)
                : Josson.create(MAPPER.createArrayNode().add(node));
        this.datasets = null;
    }

    OperationStack(final Map<String, Josson> datasets) {
        this.arrayNode = null;
        this.datasets = datasets;
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

    private JsonNode evaluateSteps(final boolean inParentheses, final Function<OperationStep, JsonNode> resolver) {
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
                node = resolver.apply(step);
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

    /*
        For JossonCore.evaluateFilter()
     */
    JsonNode evaluate(final String statement, final int arrayIndex) {
        stack.clear();
        final List<OperationStep> steps = decomposeStatement(statement);
        for (OperationStep step : steps) {
            try {
                evaluate(step, arrayIndex);
            } catch (IllegalArgumentException e) {
                throw new SyntaxErrorException(statement, e);
            }
        }
        return evaluateSteps(false, (OperationStep opStep) -> opStep.resolveFrom(arrayNode, arrayIndex));
    }

    private void evaluate(final OperationStep step, final int arrayIndex) {
        switch (step.getOperator()) {
            case NOT:
            case NOP:
                if (")".equals(step.getExpression())) {
                    evaluateSteps(true, (OperationStep opStep) -> opStep.resolveFrom(arrayNode, arrayIndex));
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
                    if (lastStep.isResolveToTrueFrom(arrayNode, arrayIndex)) {
                        lastStep.setResolved(thisStep.resolveFrom(arrayNode, arrayIndex));
                    }
                } else if (lastStep.getOperator() == Operator.OR && step.getOperator() == Operator.OR) {
                    final OperationStep thisStep = popStep();
                    if (lastStep.isResolveToFalseFrom(arrayNode, arrayIndex)) {
                        lastStep.setResolved(thisStep.resolveFrom(arrayNode, arrayIndex));
                    }
                }
                pushStep(step);
                return;
        }
        if (lastStep.getOperator() == Operator.AND) {
            final OperationStep thisStep = popStep();
            if (lastStep.isResolveToTrueFrom(arrayNode, arrayIndex)) {
                lastStep.setResolved(thisStep.relationalCompare(step, arrayNode, arrayIndex));
            }
        } else {
            lastStep.setResolved(lastStep.relationalCompare(step, arrayNode, arrayIndex));
        }
    }

    /*
        For Jossons.evaluateStatement()
     */
    JsonNode evaluate(final String statement) throws UnresolvedDatasetException {
        final List<OperationStep> steps = decomposeStatement(statement);
        for (OperationStep step : steps) {
            try {
                evaluate(step);
            } catch (IllegalArgumentException e) {
                throw new SyntaxErrorException(statement, e);
            }
        }
        try {
            return evaluateSteps(false, (OperationStep opStep) -> {
                try {
                    return opStep.resolveFrom(datasets);
                } catch (UnresolvedDatasetException e) {
                    throw new UnsupportedOperationException(e);
                }
            });
        } catch (UnsupportedOperationException e) {
            if (e.getCause() instanceof UnresolvedDatasetException) {
                throw (UnresolvedDatasetException) e.getCause();
            }
            throw e;
        }
    }

    private void evaluate(final OperationStep step) throws UnresolvedDatasetException {
        switch (step.getOperator()) {
            case NOT:
            case NOP:
                if (")".equals(step.getExpression())) {
                    try {
                        evaluateSteps(true, (OperationStep opStep) -> {
                            try {
                                return opStep.resolveFrom(datasets);
                            } catch (UnresolvedDatasetException e) {
                                throw new UnsupportedOperationException(e);
                            }
                        });
                    } catch (UnsupportedOperationException e) {
                        if (e.getCause() instanceof UnresolvedDatasetException) {
                            throw (UnresolvedDatasetException) e.getCause();
                        }
                        throw e;
                    }
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
                    if (lastStep.isResolveToTrueFrom(datasets)) {
                        lastStep.setResolved(thisStep.resolveFrom(datasets));
                    }
                } else if (lastStep.getOperator() == Operator.OR && step.getOperator() == Operator.OR) {
                    final OperationStep thisStep = popStep();
                    if (lastStep.isResolveToFalseFrom(datasets)) {
                        lastStep.setResolved(thisStep.resolveFrom(datasets));
                    }
                }
                pushStep(step);
                return;
        }
        if (lastStep.getOperator() == Operator.AND) {
            final OperationStep thisStep = popStep();
            if (lastStep.isResolveToTrueFrom(datasets)) {
                lastStep.setResolved(thisStep.relationalCompare(step, datasets));
            }
        } else {
            lastStep.setResolved(lastStep.relationalCompare(step, datasets));
        }
    }
}

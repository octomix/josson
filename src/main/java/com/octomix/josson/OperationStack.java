/*
 * Copyright 2020 Octomix Software Technology Limited
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
import com.octomix.josson.exception.UnresolvedDatasetException;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.PatternMatcher.decomposeStatement;

class OperationStack {

    private final Josson arrayNode;
    private final Map<String, Josson> datasets;
    private final LinkedList<OperationStep> stack = new LinkedList<>();
    private OperationStep lastStep = null;

    OperationStack(JsonNode node) {
        this.arrayNode = node.isArray() ?
                Josson.create(node) :
                Josson.create(MAPPER.createArrayNode().add(node));
        this.datasets = null;
    }

    OperationStack(Map<String, Josson> datasets) {
        this.arrayNode = null;
        this.datasets = datasets;
    }

    private void pushStep(OperationStep step) {
        stack.addLast(step);
        lastStep = step;
    }

    private OperationStep popStep() {
        OperationStep step = stack.removeLast();
        lastStep = stack.peekLast();
        return step;
    }

    private JsonNode evaluateSteps(boolean inParentheses, Function<OperationStep, JsonNode> resolver) {
        LinkedList<OperationStep> iterator;
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
                    result = node == null || node.isNull() || (node.isValueNode() && !node.asBoolean());
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
    JsonNode evaluate(String statement, int arrayIndex) {
        stack.clear();
        List<OperationStep> steps = decomposeStatement(statement);
        for (OperationStep step : steps) {
            try {
                evaluate(step, arrayIndex);
            } catch (IllegalArgumentException e) {
                if (e.getMessage() == null) {
                    throw new IllegalArgumentException(statement);
                }
                throw new IllegalArgumentException("\"" + e.getMessage() + "\" in " + statement);
            }
        }
        return evaluateSteps(false, (OperationStep opStep) -> opStep.resolveFrom(arrayNode, arrayIndex));
    }

    private void evaluate(OperationStep step, int arrayIndex) {
        if (step.getOperator() == Operator.NOP) {
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
                    OperationStep thisStep = popStep();
                    if (lastStep.isResolveToTrueFrom(arrayNode, arrayIndex)) {
                        lastStep.setResolved(thisStep.resolveFrom(arrayNode, arrayIndex));
                    }
                } else if (lastStep.getOperator() == Operator.OR && step.getOperator() == Operator.OR) {
                    OperationStep thisStep = popStep();
                    if (lastStep.isResolveToFalseFrom(arrayNode, arrayIndex)) {
                        lastStep.setResolved(thisStep.resolveFrom(arrayNode, arrayIndex));
                    }
                }
                pushStep(step);
                return;
        }
        if (lastStep.getOperator() == Operator.AND) {
            OperationStep thisStep = popStep();
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
    JsonNode evaluate(String statement) throws UnresolvedDatasetException {
        List<OperationStep> steps = decomposeStatement(statement);
        for (OperationStep step : steps) {
            try {
                evaluate(step);
            } catch (IllegalArgumentException e) {
                if (e.getMessage() == null) {
                    throw new IllegalArgumentException(statement);
                }
                throw new IllegalArgumentException("\"" + e.getMessage() + "\" in " + statement);
            }
        }
        try {
            return evaluateSteps(false, (OperationStep opStep) -> {
                try {
                    return opStep.resolveFrom(datasets);
                } catch (UnresolvedDatasetException e) {
                    throw new RuntimeException(e);
                }
            });
        } catch (RuntimeException e) {
            if (e.getCause() instanceof UnresolvedDatasetException) {
                throw (UnresolvedDatasetException) e.getCause();
            }
            throw e;
        }
    }

    private void evaluate(OperationStep step) throws UnresolvedDatasetException {
        switch (step.getOperator()) {
            case NOT:
            case NOP:
                if (")".equals(step.getExpression())) {
                    try {
                        evaluateSteps(true, (OperationStep opStep) -> {
                            try {
                                return opStep.resolveFrom(datasets);
                            } catch (UnresolvedDatasetException e) {
                                throw new RuntimeException(e);
                            }
                        });
                    } catch (RuntimeException e) {
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
                    OperationStep thisStep = popStep();
                    if (lastStep.isResolveToTrueFrom(datasets)) {
                        lastStep.setResolved(thisStep.resolveFrom(datasets));
                    }
                } else if (lastStep.getOperator() == Operator.OR && step.getOperator() == Operator.OR) {
                    OperationStep thisStep = popStep();
                    if (lastStep.isResolveToFalseFrom(datasets)) {
                        lastStep.setResolved(thisStep.resolveFrom(datasets));
                    }
                }
                pushStep(step);
                return;
        }
        if (lastStep.getOperator() == Operator.AND) {
            OperationStep thisStep = popStep();
            if (lastStep.isResolveToTrueFrom(datasets)) {
                lastStep.setResolved(thisStep.relationalCompare(step, datasets));
            }
        } else {
            lastStep.setResolved(lastStep.relationalCompare(step, datasets));
        }
    }
}

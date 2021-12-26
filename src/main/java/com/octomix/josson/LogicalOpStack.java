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
import com.fasterxml.jackson.databind.node.TextNode;
import com.octomix.josson.exception.UnresolvedDatasetException;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static com.octomix.josson.PatternMatcher.decomposeConditions;

class LogicalOpStack {

    private final Josson arrayNode;
    private final Map<String, Josson> datasets;
    private final LinkedList<LogicalOpStep> steps = new LinkedList<>();
    private LogicalOpStep lastStep = null;

    LogicalOpStack(JsonNode node) {
        this.arrayNode = node.isArray() ?
                Josson.create(node) :
                Josson.create(Josson.createArrayNode().add(node));
        this.datasets = null;
    }

    LogicalOpStack(Map<String, Josson> datasets) {
        this.arrayNode = null;
        this.datasets = datasets;
    }

    private void push(String operator, String unresolved) {
        lastStep = new LogicalOpStep(operator, unresolved);
        steps.addLast(lastStep);
    }

    private LogicalOpStep pop() {
        LogicalOpStep thisStep = steps.removeLast();
        lastStep = steps.peekLast();
        return thisStep;
    }

    private JsonNode evaluateSteps(boolean inParentheses, Function<LogicalOpStep, JsonNode> resolver) {
        LinkedList<LogicalOpStep> iterator;
        if (inParentheses) {
            iterator = new LinkedList<>();
            while (!steps.isEmpty()) {
                if ("(".equals(lastStep.getUnresolved())) {
                    break;
                }
                iterator.addFirst(pop());
            }
            if (lastStep == null) {
                throw new IllegalArgumentException(")");
            }
        } else {
            iterator = steps;
        }
        JsonNode node = null;
        boolean result = true;
        for (LogicalOpStep step : iterator) {
            if ("|".equals(step.getOperator())) {
                if (result) {
                    node = BooleanNode.TRUE;
                    break;
                }
                result = true;
            }
            if ("".equals(step.getUnresolved())) {
                continue;
            }
            if (result) {
                node = resolver.apply(step);
                if ("!".equals(step.getOperator())) {
                    result = node == null || node.isNull() || (node.isValueNode() && !node.asBoolean());
                    node = BooleanNode.valueOf(result);
                } else {
                    result = node != null && node.asBoolean();
                }
            }
        }
        if (inParentheses) {
            if ("!".equals(lastStep.getOperator())) {
                lastStep.resetOperator();
                node = BooleanNode.valueOf(!result);
            }
            lastStep.setResolved(node);
            return null;
        }
        return result || node != null ? node : TextNode.valueOf("");
    }

    /*
        For JossonCore.evaluateFilter()
     */
    JsonNode evaluate(String statement, int arrayIndex) {
        steps.clear();
        List<String[]> conditions = decomposeConditions(statement);
        for (String[] condition : conditions) {
            try {
                evaluate(condition[0], condition[1], arrayIndex);
            } catch (IllegalArgumentException e) {
                if (e.getMessage() == null) {
                    throw new IllegalArgumentException(statement);
                }
                throw new IllegalArgumentException("\"" + e.getMessage() + "\" in " + statement);
            }
        }
        return evaluateSteps(false, (LogicalOpStep step) -> step.resolveFrom(arrayNode, arrayIndex));
    }

    private void evaluate(String operator, String expression, int arrayIndex) {
        if (operator.isEmpty()) {
            if (")".equals(expression)) {
                evaluateSteps(true, (LogicalOpStep step) -> step.resolveFrom(arrayNode, arrayIndex));
            } else {
                push(operator, expression);
            }
            return;
        }
        if (steps.isEmpty()) {
            throw new IllegalArgumentException();
        }
        if ("(".equals(lastStep.getUnresolved())) {
            push(operator, expression);
            return;
        }
        switch (operator) {
            case "&":
            case "|":
                if ("&".equals(lastStep.getOperator())) {
                    LogicalOpStep thisStep = pop();
                    if (lastStep.isResolveToTrueFrom(arrayNode, arrayIndex)) {
                        lastStep.setResolved(thisStep.resolveFrom(arrayNode, arrayIndex));
                    }
                } else if ("|".equals(lastStep.getOperator()) && "|".equals(operator)) {
                    LogicalOpStep thisStep = pop();
                    if (lastStep.isResolveToFalseFrom(arrayNode, arrayIndex)) {
                        lastStep.setResolved(thisStep.resolveFrom(arrayNode, arrayIndex));
                    }
                }
                push(operator, expression);
                return;
        }
        if ("&".equals(lastStep.getOperator())) {
            LogicalOpStep thisStep = pop();
            if (lastStep.isResolveToTrueFrom(arrayNode, arrayIndex)) {
                lastStep.setResolved(thisStep.relationalCompare(operator, expression, arrayNode, arrayIndex));
            }
        } else {
            lastStep.setResolved(lastStep.relationalCompare(operator, expression, arrayNode, arrayIndex));
        }
    }

    /*
        For Jossons.evaluateStatement()
     */
    JsonNode evaluate(String statement) throws UnresolvedDatasetException {
        List<String[]> conditions = decomposeConditions(statement);
        for (String[] condition : conditions) {
            try {
                evaluate(condition[0], condition[1]);
            } catch (IllegalArgumentException e) {
                if (e.getMessage() == null) {
                    throw new IllegalArgumentException(statement);
                }
                throw new IllegalArgumentException("\"" + e.getMessage() + "\" in " + statement);
            }
        }
        try {
            return evaluateSteps(false, (LogicalOpStep step) -> {
                try {
                    return step.resolveFrom(datasets);
                } catch (UnresolvedDatasetException e) {
                    throw new RuntimeException(e);
                }
            });
        } catch (RuntimeException e) {
            if (e.getCause() instanceof UnresolvedDatasetException) {
                throw (UnresolvedDatasetException) e.getCause();
            } else {
                throw e;
            }
        }
    }

    private void evaluate(String operator, String expression) throws UnresolvedDatasetException {
        switch (operator) {
            case "":
            case "!":
                if (")".equals(expression)) {
                    try {
                        evaluateSteps(true, (LogicalOpStep step) -> {
                            try {
                                return step.resolveFrom(datasets);
                            } catch (UnresolvedDatasetException e) {
                                throw new RuntimeException(e);
                            }
                        });
                    } catch (RuntimeException e) {
                        if (e.getCause() instanceof UnresolvedDatasetException) {
                            throw (UnresolvedDatasetException) e.getCause();
                        } else {
                            throw e;
                        }
                    }
                } else {
                    push(operator, expression);
                }
                return;
        }
        if (steps.isEmpty()) {
            throw new IllegalArgumentException();
        }
        if ("(".equals(lastStep.getUnresolved())) {
            push(operator, expression);
            return;
        }
        switch (operator) {
            case "&":
            case "|":
                if ("&".equals(lastStep.getOperator())) {
                    LogicalOpStep thisStep = pop();
                    if (lastStep.isResolveToTrueFrom(datasets)) {
                        lastStep.setResolved(thisStep.resolveFrom(datasets));
                    }
                } else if ("|".equals(lastStep.getOperator()) && "|".equals(operator)) {
                    LogicalOpStep thisStep = pop();
                    if (lastStep.isResolveToFalseFrom(datasets)) {
                        lastStep.setResolved(thisStep.resolveFrom(datasets));
                    }
                }
                push(operator, expression);
                return;
        }
        if ("&".equals(lastStep.getOperator())) {
            LogicalOpStep thisStep = pop();
            if (lastStep.isResolveToTrueFrom(datasets)) {
                lastStep.setResolved(thisStep.relationalCompare(operator, expression, datasets));
            }
        } else {
            lastStep.setResolved(lastStep.relationalCompare(operator, expression, datasets));
        }
    }
}

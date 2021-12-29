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

import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.PatternMatcher.decomposeStatement;

class LogicalOpStack {

    private final Josson arrayNode;
    private final Map<String, Josson> datasets;
    private final LinkedList<LogicalOpStep> steps = new LinkedList<>();
    private LogicalOpStep lastStep = null;

    LogicalOpStack(JsonNode node) {
        this.arrayNode = node.isArray() ?
                Josson.create(node) :
                Josson.create(MAPPER.createArrayNode().add(node));
        this.datasets = null;
    }

    LogicalOpStack(Map<String, Josson> datasets) {
        this.arrayNode = null;
        this.datasets = datasets;
    }

    private void pushStep(LogicalOpStep opStep) {
        steps.addLast(opStep);
        lastStep = opStep;
    }

    private LogicalOpStep popStep() {
        LogicalOpStep thisStep = steps.removeLast();
        lastStep = steps.peekLast();
        return thisStep;
    }

    private JsonNode evaluateSteps(boolean inParentheses, Function<LogicalOpStep, JsonNode> resolver) {
        LinkedList<LogicalOpStep> iterator;
        if (inParentheses) {
            iterator = new LinkedList<>();
            while (!steps.isEmpty()) {
                if ("(".equals(lastStep.getExpression())) {
                    break;
                }
                iterator.addFirst(popStep());
            }
            if (lastStep == null) {
                throw new IllegalArgumentException(")");
            }
        } else {
            iterator = steps;
        }
        JsonNode node = null;
        boolean result = true;
        for (LogicalOpStep opStep : iterator) {
            if ("|".equals(opStep.getOperator())) {
                if (result) {
                    node = BooleanNode.TRUE;
                    break;
                }
                result = true;
            }
            if ("".equals(opStep.getExpression())) {
                continue;
            }
            if (result) {
                node = resolver.apply(opStep);
                if ("!".equals(opStep.getOperator())) {
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
        List<LogicalOpStep> opSteps = decomposeStatement(statement);
        for (LogicalOpStep opStep : opSteps) {
            try {
                evaluate(opStep, arrayIndex);
            } catch (IllegalArgumentException e) {
                if (e.getMessage() == null) {
                    throw new IllegalArgumentException(statement);
                }
                throw new IllegalArgumentException("\"" + e.getMessage() + "\" in " + statement);
            }
        }
        return evaluateSteps(false, (LogicalOpStep step) -> step.resolveFrom(arrayNode, arrayIndex));
    }

    private void evaluate(LogicalOpStep opStep, int arrayIndex) {
        if (opStep.getOperator().isEmpty()) {
            if (")".equals(opStep.getExpression())) {
                evaluateSteps(true, (LogicalOpStep step) -> step.resolveFrom(arrayNode, arrayIndex));
            } else {
                pushStep(opStep);
            }
            return;
        }
        if (steps.isEmpty()) {
            throw new IllegalArgumentException();
        }
        if ("(".equals(lastStep.getExpression())) {
            pushStep(opStep);
            return;
        }
        switch (opStep.getOperator()) {
            case "&":
            case "|":
                if ("&".equals(lastStep.getOperator())) {
                    LogicalOpStep thisStep = popStep();
                    if (lastStep.isResolveToTrueFrom(arrayNode, arrayIndex)) {
                        lastStep.setResolved(thisStep.resolveFrom(arrayNode, arrayIndex));
                    }
                } else if ("|".equals(lastStep.getOperator()) && "|".equals(opStep.getOperator())) {
                    LogicalOpStep thisStep = popStep();
                    if (lastStep.isResolveToFalseFrom(arrayNode, arrayIndex)) {
                        lastStep.setResolved(thisStep.resolveFrom(arrayNode, arrayIndex));
                    }
                }
                pushStep(opStep);
                return;
        }
        if ("&".equals(lastStep.getOperator())) {
            LogicalOpStep thisStep = popStep();
            if (lastStep.isResolveToTrueFrom(arrayNode, arrayIndex)) {
                lastStep.setResolved(thisStep.relationalCompare(opStep, arrayNode, arrayIndex));
            }
        } else {
            lastStep.setResolved(lastStep.relationalCompare(opStep, arrayNode, arrayIndex));
        }
    }

    /*
        For Jossons.evaluateStatement()
     */
    JsonNode evaluate(String statement) throws UnresolvedDatasetException {
        List<LogicalOpStep> opSteps = decomposeStatement(statement);
        for (LogicalOpStep opStep : opSteps) {
            try {
                evaluate(opStep);
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

    private void evaluate(LogicalOpStep opStep) throws UnresolvedDatasetException {
        switch (opStep.getOperator()) {
            case "":
            case "!":
                if (")".equals(opStep.getExpression())) {
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
                    pushStep(opStep);
                }
                return;
        }
        if (steps.isEmpty()) {
            throw new IllegalArgumentException();
        }
        if ("(".equals(lastStep.getExpression())) {
            pushStep(opStep);
            return;
        }
        switch (opStep.getOperator()) {
            case "&":
            case "|":
                if ("&".equals(lastStep.getOperator())) {
                    LogicalOpStep thisStep = popStep();
                    if (lastStep.isResolveToTrueFrom(datasets)) {
                        lastStep.setResolved(thisStep.resolveFrom(datasets));
                    }
                } else if ("|".equals(lastStep.getOperator()) && "|".equals(opStep.getOperator())) {
                    LogicalOpStep thisStep = popStep();
                    if (lastStep.isResolveToFalseFrom(datasets)) {
                        lastStep.setResolved(thisStep.resolveFrom(datasets));
                    }
                }
                pushStep(opStep);
                return;
        }
        if ("&".equals(lastStep.getOperator())) {
            LogicalOpStep thisStep = popStep();
            if (lastStep.isResolveToTrueFrom(datasets)) {
                lastStep.setResolved(thisStep.relationalCompare(opStep, datasets));
            }
        } else {
            lastStep.setResolved(lastStep.relationalCompare(opStep, datasets));
        }
    }
}

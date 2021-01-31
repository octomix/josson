package com.octomix.josson;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.BooleanNode;
import com.fasterxml.jackson.databind.node.TextNode;
import com.octomix.josson.exception.UnresolvedDatasetException;

import java.util.Map;
import java.util.Stack;
import java.util.function.Supplier;

class LogicalOpStack {

    private final Josson arrayNode;
    private final Map<String, Josson> datasets;
    private final Stack<LogicalOpStep> steps = new Stack<>();
    private LogicalOpStep lastStep = null;

    LogicalOpStack(JsonNode arrayNode) {
        this.arrayNode = Josson.create(arrayNode);
        this.datasets = null;
    }

    LogicalOpStack(Map<String, Josson> datasets) {
        this.arrayNode = null;
        this.datasets = datasets;
    }

    void clear() {
        steps.clear();
    }

    private void push(String operator, String unresolved) {
        lastStep = new LogicalOpStep(operator, unresolved);
        steps.push(lastStep);
    }

    private LogicalOpStep pop() {
        LogicalOpStep thisStep = steps.pop();
        lastStep = steps.isEmpty() ? null : steps.peek();
        return thisStep;
    }

    private void reduceLastGroup(Supplier<JsonNode> resolveLastStep) {
        JsonNode node = null;
        boolean result = true;
        while (!steps.isEmpty()) {
            if ("(".equals(lastStep.getUnresolved())) {
                break;
            }
            if (node == null || !node.asBoolean()) {
                switch (lastStep.getOperator()) {
                    case "":
                    case "&":
                    case "|":
                        if (result) {
                            JsonNode lastStepNode = resolveLastStep.get();
                            if (lastStep.getOperator().isEmpty()) {
                                node = lastStepNode;
                            } else {
                                result = lastStepNode != null && lastStepNode.asBoolean();
                            }
                        }
                        if ("|".equals(lastStep.getOperator())) {
                            if (result) {
                                node = BooleanNode.TRUE;
                            } else {
                                result = true;
                            }
                        }
                        break;
                    default:
                        throw new IllegalArgumentException(lastStep.getOperator());
                }
            }
            pop();
        }
        if (steps.isEmpty()) {
            throw new IllegalArgumentException(")");
        }
        lastStep.setResolved(node);
    }

    private JsonNode finalResult(Supplier<JsonNode> resolveLastStep) {
        boolean result = true;
        while (!steps.isEmpty()) {
            switch (lastStep.getOperator()) {
                case "":
                case "&":
                case "|":
                    if (result) {
                        JsonNode lastStepNode = resolveLastStep.get();
                        if (lastStep.getOperator().isEmpty()) {
                            return lastStepNode;
                        }
                        result = lastStepNode != null && lastStepNode.asBoolean();
                    }
                    if ("|".equals(lastStep.getOperator())) {
                        if (result) {
                            return BooleanNode.TRUE;
                        }
                        result = true;
                    }
                    break;
                default:
                    throw new IllegalArgumentException(lastStep.getOperator());
            }
            pop();
        }
        return TextNode.valueOf("");
    }

    /*
        For JossonCore.filterArrayNode()
     */
    void evaluate(String operator, String expression, int arrayIndex) {
        if (operator.isEmpty()) {
            if (")".equals(expression)) {
                reduceLastGroup(() -> lastStep.resolveFrom(arrayNode, arrayIndex));
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

    JsonNode finalResult(int arrayIndex) {
        return finalResult(() -> lastStep.resolveFrom(arrayNode, arrayIndex));
    }

    /*
        For Jossons.evaluateStatement()
     */
    void evaluate(String operator, String expression) throws UnresolvedDatasetException {
        if (operator.isEmpty()) {
            if (")".equals(expression)) {
                try {
                    reduceLastGroup(() -> {
                        try {
                            return lastStep.resolveFrom(datasets);
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

    JsonNode finalResult() throws UnresolvedDatasetException {
        try {
            return finalResult(() -> {
                try {
                    return lastStep.resolveFrom(datasets);
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
}

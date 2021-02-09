package com.octomix.josson;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.IntNode;
import com.octomix.josson.exception.UnresolvedDatasetException;

import java.util.List;
import java.util.Map;

import static com.octomix.josson.JossonCore.evaluateExpression;
import static com.octomix.josson.JossonCore.getNodeByKeys;
import static com.octomix.josson.PatternMatcher.decomposePaths;

class LogicalOpStep {

    private final String operator;
    private String unresolved;
    private JsonNode resolved;

    LogicalOpStep(String operator, String unresolved) {
        this.operator = operator;
        this.unresolved = unresolved;
        this.resolved = null;
    }

    String getOperator() {
        return operator;
    }

    String getUnresolved() {
        return unresolved;
    }

    void setResolved(JsonNode resolved) {
        this.unresolved = null;
        this.resolved = resolved;
    }

    /*
        For JossonCore.filterArrayNode()
     */
    private static JsonNode getNodeFrom(Josson arrayNode, int arrayIndex, String expression) {
        if (expression.charAt(0) == '#') {
            List<String> keys = decomposePaths(expression);
            switch (keys.remove(0)) {
                case "#":
                    return getNodeByKeys(IntNode.valueOf(arrayIndex), keys);
                case "##":
                    return getNodeByKeys(IntNode.valueOf(arrayIndex + 1), keys);
            }
            return null;
        }
        return expression.equals("?") ?
                arrayNode.getNode(arrayIndex) :
                expression.startsWith("@") ?
                        arrayNode.getNode(expression.substring(1)) :
                        arrayNode.getNode(arrayIndex, expression);
    }

    JsonNode resolveFrom(Josson arrayNode, int arrayIndex) {
        if (unresolved != null) {
            resolved = getNodeFrom(arrayNode, arrayIndex, unresolved);
            unresolved = null;
        }
        return resolved;
    }

    boolean isResolveToTrueFrom(Josson arrayNode, int arrayIndex) {
        JsonNode node = resolveFrom(arrayNode, arrayIndex);
        return node != null && node.asBoolean();
    }

    boolean isResolveToFalseFrom(Josson arrayNode, int arrayIndex) {
        JsonNode node = resolveFrom(arrayNode, arrayIndex);
        return node != null && !node.asBoolean();
    }

    JsonNode relationalCompare(String operator, String expression, Josson arrayNode, int arrayIndex) {
        resolved = JossonCore.relationalCompare(
                resolveFrom(arrayNode, arrayIndex), operator, getNodeFrom(arrayNode, arrayIndex, expression));
        return resolved;
    }

    /*
        For Jossons.evaluateStatement()
     */
    JsonNode resolveFrom(Map<String, Josson> datasets) throws UnresolvedDatasetException {
        if (unresolved != null) {
            resolved = evaluateExpression(unresolved, datasets);
            unresolved = null;
        }
        return resolved;
    }

    boolean isResolveToTrueFrom(Map<String, Josson> datasets) throws UnresolvedDatasetException {
        JsonNode node = resolveFrom(datasets);
        return node != null && node.asBoolean();
    }

    boolean isResolveToFalseFrom(Map<String, Josson> datasets) throws UnresolvedDatasetException {
        JsonNode node = resolveFrom(datasets);
        return node != null && !node.asBoolean();
    }

    JsonNode relationalCompare(String operator, String expression, Map<String, Josson> datasets)
            throws UnresolvedDatasetException {
        resolved = JossonCore.relationalCompare(
                resolveFrom(datasets), operator, evaluateExpression(expression, datasets));
        return resolved;
    }
}

/*
 * Copyright 2020-2024 Choi Wai Man Raymond
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

import java.util.HashMap;
import java.util.Map;

/**
 * Contains all progressive nodes and variables defined along the path.
 */
public class PathTrace {

    private final JsonNode[] steps;
    private final Map<String, CustomFunction> customFunctions;
    private MergeArraysOption mergeArraysOption;
    private Map<String, JsonNode> variables;

    PathTrace(final int size, final PathTrace cloneProperties) {
        this.steps = new JsonNode[size];
        this.customFunctions = cloneProperties.customFunctions;
        this.mergeArraysOption = cloneProperties.mergeArraysOption;
        this.variables = cloneProperties.variables;
    }

    PathTrace(final JsonNode node, final PathTrace cloneProperties) {
        this.steps = new JsonNode[]{node};
        this.customFunctions = cloneProperties.customFunctions;
        this.mergeArraysOption = cloneProperties.mergeArraysOption;
        this.variables = cloneProperties.variables;
    }

    PathTrace(
        final JsonNode node,
        final Map<String, CustomFunction> customFunctions,
        final Map<String, JsonNode> variables
    ) {
        this.steps = new JsonNode[]{node};
        this.customFunctions = customFunctions;
        this.mergeArraysOption = JossonCore.mergeArraysOption;
        this.variables = variables;
    }

    static PathTrace from(final JsonNode node) {
        return new PathTrace(node, null, null);
    }

    static PathTrace from(final JsonNode node, final Map<String, CustomFunction> customFunctions) {
        return new PathTrace(node, customFunctions, null);
    }

    static PathTrace from(
        final JsonNode node,
        final Map<String, CustomFunction> customFunctions,
        final Map<String, JsonNode> variables
    ) {
        if (variables != null) {
            variables.keySet().forEach(JossonCore::checkVariableName);
        }
        return new PathTrace(node, customFunctions, variables);
    }

    PathTrace root() {
        return steps.length == 1 ? this : new PathTrace(steps[0], this);
    }

    PathTrace push(final JsonNode node) {
        if (node == null) {
            return null;
        }
        final PathTrace clone = new PathTrace(steps.length + 1, this);
        System.arraycopy(steps, 0, clone.steps, 0, steps.length);
        clone.steps[steps.length] = node;
        return clone;
    }

    PathTrace pop(final int steps) {
        final PathTrace clone = new PathTrace(this.steps.length - steps, this);
        System.arraycopy(this.steps, 0, clone.steps, 0, this.steps.length - steps);
        return clone;
    }

    JsonNode node() {
        return steps[steps.length - 1];
    }

    int steps() {
        return steps.length - 1;
    }

    void setMergeArraysOption(final MergeArraysOption mergeArraysOption) {
        this.mergeArraysOption = mergeArraysOption;
    }

    MergeArraysOption getMergeArraysOption() {
        return mergeArraysOption;
    }

    void setVariable(final String name, final JsonNode value) {
        JossonCore.checkVariableName(name);
        if (variables == null) {
            variables = new HashMap<>();
        }
        variables.put(name, value);
    }

    JsonNode getVariable(final String name) {
        return variables == null ? null : variables.get(name);
    }

    CustomFunction getCustomFunction(final String functionName) {
        return customFunctions == null ? null : customFunctions.get(functionName);
    }

    boolean isObject() {
        return node().isObject();
    }
    
    boolean isArray() {
        return node().isArray();
    }

    boolean isContainer() {
        return node().isContainerNode();
    }

    boolean isValueNode() {
        return node().isValueNode();
    }

    boolean isTextual() {
        return node().isTextual();
    }

    boolean isNumber() {
        return node().isNumber();
    }

    boolean isBoolean() {
        return node().isBoolean();
    }

    boolean isNull() {
        return node().isNull();
    }

    boolean isEmpty() {
        return node().isEmpty();
    }

    String asText() {
        return node().asText();
    }

    double asDouble() {
        return node().asDouble();
    }

    int asInt() {
        return node().asInt();
    }

    boolean asBoolean() {
        return node().asBoolean();
    }

    int containerSize() {
        return node().size();
    }

    JsonNode get(final int i) {
        return node().get(i);
    }

    JsonNode get(final String fieldName) {
        return node().get(fieldName);
    }

    /**
     * Get all progressive nodes along the path.
     *
     * @return all progressive nodes along the path.
     */
    public JsonNode[] getNodes() {
        return steps;
    }

    /**
     * Get all variables defined by function let() along the path.
     *
     * @return all variables defined by function let().
     */
    public Map<String, JsonNode> getVariables() {
        return variables;
    }
}

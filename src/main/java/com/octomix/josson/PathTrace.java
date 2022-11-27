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
import com.octomix.josson.exception.SyntaxErrorException;

import java.util.HashMap;
import java.util.Map;

import static com.octomix.josson.JossonCore.VARIABLE_PREFIX_SYMBOL;

class PathTrace {

    private final JsonNode[] steps;
    private Map<String, JsonNode> variables;

    private PathTrace(final int size, final Map<String, JsonNode> variables) {
        this.steps = new JsonNode[size];
        this.variables = variables;
    }

    private PathTrace(final JsonNode node, final Map<String, JsonNode> variables) {
        this.steps = new JsonNode[]{node};
        this.variables = variables;
    }

    static PathTrace from(final JsonNode node) {
        return new PathTrace(node, null);
    }

    PathTrace root() {
        return steps.length == 1 ? this : new PathTrace(steps[0], variables);
    }

    PathTrace push(final JsonNode node) {
        final PathTrace clone = new PathTrace(steps.length + 1, variables);
        System.arraycopy(steps, 0, clone.steps, 0, steps.length);
        clone.steps[steps.length] = node;
        return clone;
    }

    PathTrace pop(final int levels) {
        final PathTrace clone = new PathTrace(steps.length - levels, variables);
        System.arraycopy(steps, 0, clone.steps, 0, steps.length - levels);
        return clone;
    }

    JsonNode node() {
        return steps[steps.length - 1];
    }

    int level() {
        return steps.length - 1;
    }

    void setVariable(final String name, final JsonNode value) {
        if (name.length() < 2 || name.charAt(0) != VARIABLE_PREFIX_SYMBOL) {
            throw new SyntaxErrorException("Variable name must start with '$' and has at least 2 characters");
        }
        if (variables == null) {
            variables = new HashMap<>();
        }
        variables.put(name, value);
    }

    JsonNode getVariable(final String name) {
        return variables == null ? null : variables.get(name);
    }
}

/*
 * Copyright 2020-2023 Octomix Software Technology Limited
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
import com.octomix.josson.commons.StringUtils;
import com.octomix.josson.exception.UnresolvedDatasetException;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Map;

import static com.octomix.josson.JossonsCore.isCacheDataset;
import static com.octomix.josson.Utils.literalToValueNode;
import static com.octomix.josson.commons.StringUtils.EMPTY;

/**
 * Relational operation.
 */
class OperationStep {

    private Operator operator;

    private String expression;

    private JsonNode resolved;

    OperationStep(final Operator operator, final String expression) {
        this.operator = operator;
        this.expression = expression;
        this.resolved = null;
    }

    Operator getOperator() {
        return operator;
    }

    void resetOperator() {
        operator = Operator.NOP;
    }

    String getExpression() {
        return expression;
    }

    void setResolved(final JsonNode resolved) {
        this.expression = null;
        this.resolved = resolved;
    }

    JsonNode getResolved() {
        return resolved;
    }

    JsonNode evaluateExpression(final Map<String, Josson> datasets) throws UnresolvedDatasetException {
        try {
            return literalToValueNode(expression);
        } catch (NumberFormatException ignore) {
        }
        if (datasets.containsKey(expression)) {
            final Josson josson = datasets.get(expression);
            if (josson == null) {
                return null;
            }
            return josson.getNode();
        }
        JsonNode implicitVariable = getImplicitVariable(expression);
        if (implicitVariable != null) {
            return implicitVariable;
        }
        final String[] tokens = new SyntaxDecomposer(expression).deDatasetQuery();
        if (tokens == null) {
            if (isCacheDataset(expression)) {
                throw new UnresolvedDatasetException(expression);
            }
            return null;
        }
        final Josson josson;
        if (datasets.containsKey(tokens[0])) {
            josson = datasets.get(tokens[0]);
            if (josson == null) {
                return null;
            }
        } else {
            implicitVariable = getImplicitVariable(tokens[0]);
            if (implicitVariable == null) {
                if (isCacheDataset(tokens[0])) {
                    throw new UnresolvedDatasetException(tokens[0]);
                }
                return null;
            }
            josson = Josson.create(implicitVariable);
        }
        final JsonNode node = josson.getNode(tokens[1]);
        if (isCacheDataset(expression)) {
            datasets.put(expression, node == null ? null : Josson.create(node));
        }
        return node;
    }

    private static JsonNode getImplicitVariable(final String name) {
        if (name.charAt(0) == '$') {
            switch (StringUtils.stripStart(name.substring(1), null).toLowerCase()) {
                case EMPTY:
                    return BooleanNode.TRUE;
                case "now":
                    return TextNode.valueOf(LocalDateTime.now().toString());
                case "today":
                    return TextNode.valueOf(LocalDate.now().atStartOfDay().toString());
                case "yesterday":
                    return TextNode.valueOf(LocalDate.now().atStartOfDay().minusDays(1).toString());
                case "tomorrow":
                    return TextNode.valueOf(LocalDate.now().atStartOfDay().plusDays(1).toString());
            }
        }
        return null;
    }
}

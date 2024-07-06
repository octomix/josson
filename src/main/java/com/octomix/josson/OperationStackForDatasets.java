/*
 * Copyright 2020-2024 Octomix Software Technology Limited
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
import com.octomix.josson.commons.StringUtils;
import com.octomix.josson.exception.UnresolvedDatasetException;

import java.util.List;
import java.util.Map;

import static com.octomix.josson.JossonCore.EMPTY_STRING_NODE;
import static com.octomix.josson.Utils.asBoolean;
import static com.octomix.josson.Utils.nodeIsNull;
import static com.octomix.josson.commons.StringUtils.EMPTY;

/**
 * Logical operations on a stack of OperationStep for datasets.
 */
class OperationStackForDatasets extends OperationStack {

    private final Map<String, Josson> datasets;

    OperationStackForDatasets(final Map<String, Josson> datasets) {
        this(datasets, false);
    }

    OperationStackForDatasets(final Map<String, Josson> datasets, final boolean isAntiInject) {
        super(isAntiInject);
        this.datasets = datasets;
    }

    protected JsonNode evaluateExpression(final OperationStep step, final int arrayIndex) {
        try {
            return step.evaluateExpression(datasets);
        } catch (UnresolvedDatasetException e) {
            throw new UnsupportedOperationException(e);
        }
    }

    JsonNode evaluateStatement(final String statement) throws UnresolvedDatasetException {
        try {
            return evaluate(statement, 0);
        } catch (UnsupportedOperationException e) {
            if (e.getCause() instanceof UnresolvedDatasetException) {
                throw (UnresolvedDatasetException) e.getCause();
            }
            throw e;
        }
    }

    JsonNode evaluateQuery(final String query) throws UnresolvedDatasetException {
        if (StringUtils.isBlank(query)) {
            return EMPTY_STRING_NODE;
        }
        final SyntaxDecomposer decomposer = new SyntaxDecomposer(query);
        final List<CombineOperation> operations = decomposer.deCombineOperations();
        if (operations != null) {
            JsonNode combined = null;
            for (CombineOperation operation : operations) {
                try {
                    combined = operation.apply(combined, eachQuery -> {
                        try {
                            return evaluateStatement(eachQuery);
                        } catch (UnresolvedDatasetException e) {
                            throw new IllegalArgumentException(null, e);
                        }
                    });
                } catch (IllegalArgumentException e) {
                    if (e.getCause() instanceof UnresolvedDatasetException) {
                        throw (UnresolvedDatasetException) e.getCause();
                    }
                    throw e;
                }
            }
            return combined;
        }
        for (TernaryStep step : decomposer.deTernarySteps()) {
            final JsonNode node = evaluateStatement(step.getStatement());
            if (nodeIsNull(node) && EMPTY.equals(step.getIfTrueValue())) {
                continue;
            }
            if (step.getIfTrueValue() == null) {
                return node;
            }
            if (!nodeIsNull(node)) {
                if (step.getIfTrueValue().isEmpty()) {
                    if (!(node.isTextual() && node.textValue().isEmpty())) {
                        return node;
                    }
                } else if (asBoolean(node)) {
                    return evaluateStatement(step.getIfTrueValue());
                }
            }
        }
        return EMPTY_STRING_NODE;
    }
}

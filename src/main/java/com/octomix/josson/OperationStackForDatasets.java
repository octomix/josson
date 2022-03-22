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
import com.octomix.josson.exception.UnresolvedDatasetException;

import java.util.Map;

/**
 * Logical operations on a stack of OperationStep for datasets.
 */
class OperationStackForDatasets extends OperationStack {

    private final Map<String, Josson> datasets;

    OperationStackForDatasets(final Map<String, Josson> datasets) {
        this.datasets = datasets;
    }

    protected JsonNode evaluateExpression(final OperationStep step, final int arrayIndex) {
        try {
            return step.evaluateExpression(datasets);
        } catch (UnresolvedDatasetException e) {
            throw new UnsupportedOperationException(e);
        }
    }
}

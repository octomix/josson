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

package com.octomix.josson.exception;

/**
 * Thrown to indicate that a dataset is unresolvable during the resolution process.
 */
public class UnresolvedDatasetException extends Exception {

    /**
     * The unresolvable dataset name.
     */
    private final String datasetName;

    /**
     * Thrown to indicate that a dataset is unresolvable during the resolution process.
     *
     * @param datasetName unresolved dataset name
     */
    public UnresolvedDatasetException(final String datasetName) {
        super("Unresolved dataset " + datasetName);
        this.datasetName = datasetName;
    }

    /**
     * Get the unresolved dataset name.
     *
     * @return Unresolved dataset name
     */
    public String getDatasetName() {
        return datasetName;
    }
}

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
package com.octomix.josson.exception;

import java.util.Set;

public class NoValuePresentException extends Exception {

    private final Set<String> datasetNames;
    private final Set<String> placeholders;
    private final String content;

    public NoValuePresentException(Set<String> datasetNames, Set<String> placeholders) {
        this(datasetNames, placeholders, null);
    }

    public NoValuePresentException(Set<String> datasetNames, Set<String> placeholders, String content) {
        super((datasetNames == null ? "" : "Unresolved data set " + datasetNames + ". ")
                + (placeholders == null ? "" : "Unresolvable placeholders " + placeholders + ". "));
        this.datasetNames = datasetNames;
        this.placeholders = placeholders;
        this.content = content;
    }

    public Set<String> getDatasetNames() {
        return datasetNames;
    }

    public Set<String> getPlaceholders() {
        return placeholders;
    }

    public String getContent() {
        return content;
    }
}

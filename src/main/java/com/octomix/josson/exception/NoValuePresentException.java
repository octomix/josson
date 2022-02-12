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

import java.util.Set;

/**
 * Thrown to indicate that Jossons cannot resolve all the placeholders.
 */
public class NoValuePresentException extends Exception {

    private final Set<String> datasetNames;
    private final Set<String> placeholders;
    private final String content;

    /**
     * Thrown to indicate that Jossons cannot resolve all the placeholders.
     *
     * @param datasetNames unresolved dataset names
     * @param placeholders unresolvable placeholders
     */
    public NoValuePresentException(final Set<String> datasetNames, final Set<String> placeholders) {
        this(datasetNames, placeholders, null);
    }

    /**
     * Thrown to indicate that Jossons cannot resolve all the placeholders.
     *
     * @param datasetNames unresolved dataset names
     * @param placeholders unresolvable placeholders
     * @param content template content that includes unresolvable placeholders
     */
    public NoValuePresentException(final Set<String> datasetNames, final Set<String> placeholders,
                                   final String content) {
        super((datasetNames == null ? "" : "Unresolved datasets " + datasetNames + ". ")
                + (placeholders == null ? "" : "Unresolvable placeholders " + placeholders + ". "));
        this.datasetNames = datasetNames;
        this.placeholders = placeholders;
        this.content = content;
    }

    /**
     * @return Unresolved dataset names
     */
    public Set<String> getDatasetNames() {
        return datasetNames;
    }

    /**
     * @return Unresolvable placeholders
     */
    public Set<String> getPlaceholders() {
        return placeholders;
    }

    /**
     * In this content, all unresolvable placeholders are quoted with `**` to replace the original double curly braces.
     *
     * @return Template content that includes unresolvable placeholders
     */
    public String getContent() {
        return content;
    }
}

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

import static com.octomix.josson.commons.StringUtils.EMPTY;

/**
 * Thrown to indicate that Jossons cannot resolve all the placeholders.
 */
public class NoValuePresentException extends Exception {

    private final Set<String> datasetNames;

    private final Set<String> placeholders;

    private final String content;

    private boolean isAntiInject = false;

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
        super((datasetNames == null ? EMPTY : String.format("Unresolved datasets %s.", datasetNames))
            + (placeholders == null ? EMPTY : String.format("Unresolvable placeholders %s.", placeholders)));
        this.datasetNames = datasetNames;
        this.placeholders = placeholders;
        this.content = content;
    }

    /**
     * Get the unresolved dataset names.
     *
     * @return Unresolved dataset names
     */
    public Set<String> getDatasetNames() {
        return datasetNames;
    }

    /**
     * Get the unresolvable placeholders.
     *
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

    /**
     * To set whether {@code content} contains anti-injection markers.
     *
     * @param isAntiInject Indicate whether {@code content} contains anti-injection markers
     * @return {@code this}
     */
    public NoValuePresentException isAntiInject(final boolean isAntiInject) {
        this.isAntiInject = isAntiInject;
        return this;
    }

    /**
     * Get whether {@code content} contains anti-injection markers
     *
     * @return is {@code content} contains anti-injection markers
     */
    public boolean isAntiInject() {
        return isAntiInject;
    }
}

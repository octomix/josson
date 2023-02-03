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

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;
import com.octomix.josson.commons.StringUtils;
import com.octomix.josson.exception.NoValuePresentException;
import com.octomix.josson.exception.UnresolvedDatasetException;

import java.util.*;
import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * Jossons is a template engine to generate text output.
 */
public class Jossons extends JossonsCore {

    private MarkupLanguage escapingMarkup;

    /**
     * Plain text is the default.
     * Uses {@code escapingMarkup()} to set {@link MarkupLanguage} to XML or HTML.
     */
    public Jossons() {
        escapingMarkup = MarkupLanguage.NONE;
    }

    /**
     * <p>Create a Jossons object with given Jackson ObjectNode.
     * Each element of the ObjectNode will become a member of the default dataset mapping.</p>
     *
     * @param datasets the default datasets
     * @return The new Jossons object
     * @throws IllegalArgumentException if {@code datasets} is not a Jackson ObjectNode
     */
    public static Jossons create(final JsonNode datasets) {
        if (datasets != null && datasets.getNodeType() != JsonNodeType.OBJECT) {
            throw new IllegalArgumentException("Argument is not an object node");
        }
        final Jossons jossons = new Jossons();
        if (datasets != null) {
            datasets.fields().forEachRemaining(entry ->
                    jossons.datasets.put(entry.getKey(), Josson.create(entry.getValue())));
        }
        return jossons;
    }

    /**
     * <p>Create a Jossons object with given JSON content string that deserialized to a Jackson ObjectNode.
     * Each element of the ObjectNode will become a member of the default dataset mapping.</p>
     *
     * @param json the string content for building the JSON tree
     * @return The new Jossons object
     * @throws JsonProcessingException if underlying input contains invalid content
     */
    public static Jossons fromJsonString(final String json) throws JsonProcessingException {
        return create(Josson.readJsonNode(json));
    }

    /**
     * <p>Create a Jossons object with given text-based dataset mapping.
     * Each element of the mapping will become a member of the default dataset mapping.</p>
     *
     * @param mapping the text-based dataset mapping
     * @return The new Jossons object
     */
    public static Jossons fromMap(final Map<String, String> mapping) {
        final Jossons jossons = new Jossons();
        if (mapping != null) {
            mapping.forEach((key, value) ->
                    jossons.datasets.put(key, Josson.create(TextNode.valueOf(value))));
        }
        return jossons;
    }

    /**
     * <p>Create a Jossons object with given integer-based dataset mapping.
     * Each element of the mapping will become a member of the default dataset mapping.</p>
     *
     * @param mapping the integer-based dataset mapping
     * @return The new Jossons object
     */
    public static Jossons fromMapOfInt(final Map<String, Integer> mapping) {
        final Jossons jossons = new Jossons();
        if (mapping != null) {
            mapping.forEach((key, value) ->
                    jossons.datasets.put(key, Josson.create(IntNode.valueOf(value))));
        }
        return jossons;
    }

    /**
     * Set the escaping operation for markup language.
     *
     * @param markupLanguage {@link MarkupLanguage}
     * @return {@code this}
     */
    public Jossons escapingMarkup(final MarkupLanguage markupLanguage) {
        escapingMarkup = markupLanguage;
        return this;
    }

    /**
     * <p>To indicate that placeholder query in the template is not escaped.</p>
     *
     * @return {@code this}
     */
    public Jossons placeholderIsNotEscaped() {
        needUnescapeQuery = false;
        return this;
    }

    /**
     * Put dataset to the dataset mapping.
     *
     * @param key the mapping key
     * @param value the mapping Josson object as the data value
     * @return {@code this}
     */
    public Jossons putDataset(final String key, final Josson value) {
        datasets.put(key, value);
        return this;
    }

    /**
     * Put dataset to the dataset mapping.
     *
     * @param key the mapping key
     * @param value the mapping Josson object as the data value
     * @return {@code this}
     */
    public Jossons putDataset(final String key, final JsonNode value) {
        datasets.put(key, Josson.create(value));
        return this;
    }

    /**
     * Get the stored dataset mapping for placeholder resolution.
     *
     * @return The stored dataset mapping
     */
    public Map<String, Josson> getDatasets() {
        return datasets;
    }

    /**
     * <p>Uses the stored dataset mapping to merge and fill all placeholders in a template.
     * Placeholder is start with "{{" and end with "}}" as the quote marks.</p>
     * <p>Any unresolvable placeholder will raise {@code NoValuePresentException} with the incomplete merged text
     * content. All unresolvable placeholders are quoted with "**" to replace the original "{{" and "}}".</p>
     *
     * @param template the template to be executed
     * @return The merged text content
     * @throws NoValuePresentException if any placeholder was unable to resolve
     */
    public String fillInPlaceholder(final String template) throws NoValuePresentException {
        if (StringUtils.isBlank(template)) {
            return template;
        }
        return fillInPlaceholderLoop(template, escapingMarkup, false);
    }

    /**
     * <p>Uses the stored dataset mapping and with the help of on demand callback dataset resolver
     * to merge and fill all placeholders in a template.
     * Placeholder is start with "{{" and end with "}}" as the quote marks.</p>
     * <p>An unresolved dataset will trigger {@code dictionaryFinder} callback that shall return either:
     * (1) a value.
     * (2) a Jossons query that retrieve data from other datasets.
     * (3) a database query statement that will further trigger {@code dataFinder} callback.</p>
     * (4) a join operation query to merge two datasets.
     * <p>Any unresolvable placeholder will raise {@code NoValuePresentException} with the incomplete merged text
     * content. All unresolvable placeholders are quoted with "**" to replace the original "{{" and "}}".</p>
     *
     * @param template the template to be executed
     * @param dictionaryFinder a callback function to return solution query statement
     * @param dataFinder a callback function to process database query statement
     * @param progress a {@code ResolverProgress} object to log the resolver progress steps
     * @return The merged text content
     * @throws NoValuePresentException if any placeholder was unable to resolve
     * @throws NullPointerException if {@code dictionaryFinder} is null
     */
    public String fillInPlaceholderWithResolver(final String template, final Function<String, String> dictionaryFinder,
                                                final BiFunction<String, String, Josson> dataFinder,
                                                final ResolverProgress progress) throws NoValuePresentException {
        if (StringUtils.isBlank(template)) {
            return template;
        }
        Objects.requireNonNull(dictionaryFinder);
        try {
            return fillInPlaceholderWithResolver(template, dictionaryFinder, dataFinder, escapingMarkup, progress);
        } finally {
            if (progress.isAutoMarkEnd()) {
                progress.markEnd();
            }
        }
    }

    /**
     * <p>Uses the stored dataset mapping and with the help of on demand callback dataset resolver to retrieve data.</p>
     * <p>An unresolved dataset will trigger {@code dictionaryFinder} callback that shall return either:
     * (1) a value.
     * (2) a Jossons query that retrieve data from other datasets.
     * (3) a database query statement that will further trigger {@code dataFinder} callback.</p>
     * (4) a join operation query to merge two datasets.
     *
     * @param query the Jossons query
     * @param dictionaryFinder a callback function to return solution query statement
     * @param dataFinder a callback function to process database query statement
     * @param progress a {@code ResolverProgress} object to log the resolver progress steps
     * @return The resulting Jackson JsonNode
     * @throws NullPointerException if {@code dictionaryFinder} is null
     */
    public JsonNode evaluateQueryWithResolver(final String query, final Function<String, String> dictionaryFinder,
                                              final BiFunction<String, String, Josson> dataFinder,
                                              final ResolverProgress progress) {
        Objects.requireNonNull(dictionaryFinder);
        final JsonNode node = evaluateQueryWithResolverLoop(query, dictionaryFinder, dataFinder, progress);
        progress.addResolvedNode("query result", node);
        return node;
    }

    /**
     * Evaluate a Jossons query to retrieve data from the stored dataset mapping.
     *
     * @param query the Jossons query
     * @return The resulting Jackson JsonNode
     * @throws UnresolvedDatasetException if the query is looking for a non-exists dataset
     */
    public JsonNode evaluateQuery(final String query) throws UnresolvedDatasetException {
        return new OperationStackForDatasets(datasets).evaluateQuery(query);
    }

    /**
     * Evaluate a Jossons statement to retrieve data from the stored dataset mapping.
     *
     * @param statement the Jossons statement
     * @return The resulting Jackson JsonNode
     * @throws UnresolvedDatasetException if the query is looking for a non-exists dataset
     */
    public JsonNode evaluateStatement(final String statement) throws UnresolvedDatasetException {
        return new OperationStackForDatasets(datasets).evaluateStatement(statement);
    }
}

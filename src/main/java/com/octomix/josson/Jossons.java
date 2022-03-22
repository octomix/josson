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

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;
import com.octomix.josson.commons.StringUtils;
import com.octomix.josson.exception.NoValuePresentException;
import com.octomix.josson.exception.UnresolvedDatasetException;

import java.util.*;
import java.util.function.BiFunction;
import java.util.function.Function;

import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.PatternMatcher.*;
import static com.octomix.josson.commons.StringEscapeUtils.unescapeXml;

/**
 * Jossons is a template engine to generate text output.
 */
public class Jossons {

    private static final String UNRESOLVABLE_PLACEHOLDER_MARK = "**";

    private final Map<String, Josson> datasets = new HashMap<>();

    private Jossons() {
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
     * Get the stored dataset mapping for placeholder resolution.
     *
     * @return The stored dataset mapping
     */
    public Map<String, Josson> getDatasets() {
        return datasets;
    }

    private boolean buildDataset(final String name, final String query, final Function<String, String> dictionaryFinder,
                                 final BiFunction<String, String, Josson> dataFinder, final ResolverProgress progress) {
        Josson dataset = null;
        final String[] tokens = matchDbQuery(query);
        if (tokens != null) {
            progress.addResolvingFrom(name, query);
            final String collectionName = (tokens[0].isEmpty() ? name : tokens[0]) + tokens[1];
            dataset = dataFinder.apply(collectionName, tokens[2]);
        } else {
            final JoinDatasets joinDatasets = matchJoinDatasetOperation(query);
            if (joinDatasets == null) {
                return false;
            }
            progress.addResolvingFrom(name, query);
            try {
                dataset = joinDatasets(joinDatasets, dictionaryFinder, dataFinder, progress);
            } catch (IllegalArgumentException e) {
                progress.addStep("Join operation failed - " + e.getMessage());
            }
        }
        progress.addResolvedDataset(name, dataset);
        putDataset(name, dataset);
        return true;
    }

    private Josson joinDatasets(final JoinDatasets datasets, final Function<String, String> dictionaryFinder,
                                final BiFunction<String, String, Josson> dataFinder, final ResolverProgress progress) {
        final JsonNode leftNode = evaluateQueryWithResolverLoop(
                datasets.getLeftDataset().getQuery(), dictionaryFinder, dataFinder, progress);
        if (leftNode == null) {
            throw new IllegalArgumentException("unresolvable left side");
        }
        if (!leftNode.isContainerNode()) {
            throw new IllegalArgumentException("left side is not a container node");
        }
        final JsonNode rightNode = evaluateQueryWithResolverLoop(
                datasets.getRightDataset().getQuery(), dictionaryFinder, dataFinder, progress);
        if (rightNode == null) {
            throw new IllegalArgumentException("unresolvable right side");
        }
        if (!rightNode.isContainerNode()) {
            throw new IllegalArgumentException("right side is not a container node");
        }
        final JsonNode joinedNode = datasets.joinNodes(leftNode, rightNode);
        if (joinedNode == null) {
            throw new IllegalArgumentException("invalid data");
        }
        return Josson.create(joinedNode);
    }

    /**
     * <p>XML template version of {@code fillInPlaceholder()}.
     * Unescapes placeholder before the resolution process of that placeholder.
     * Useful to merge docx template document.</p>
     *
     * @param template the XML template to be executed
     * @return The merged text content
     * @see #fillInPlaceholder
     * @throws NoValuePresentException if any placeholder was unable to resolve
     */
    public String fillInXmlPlaceholder(final String template) throws NoValuePresentException {
        if (StringUtils.isBlank(template)) {
            return template;
        }
        return fillInPlaceholderLoop(template, true);
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
        return fillInPlaceholderLoop(template, false);
    }

    private String fillInPlaceholderLoop(String template, final boolean isXml) throws NoValuePresentException {
        final Set<String> unresolvedDatasets = new HashSet<>();
        final Set<String> unresolvedPlaceholders = new HashSet<>();
        final StringBuilder sb = new StringBuilder();
        final int last = template.length() - 1;
        int offset = 0;
        int placeholderAt = -1;
        boolean textAdded = false;
        for (int i = 0; i < last; i++) {
            if (template.charAt(i) == '{') {
                if (template.charAt(i + 1) == '{') {
                    i++;
                    while (template.charAt(i + 1) == '{' && i < last) {
                        i++;
                    }
                    placeholderAt = i - 1;
                    sb.append(template, offset, placeholderAt);
                    offset = placeholderAt;
                }
            } else if (placeholderAt >= 0 && template.charAt(i) == '}' && template.charAt(i + 1) == '}') {
                String query = template.substring(placeholderAt + 2, i);
                if (isXml) {
                    final StringBuilder rebuild = new StringBuilder();
                    for (String token : separateXmlTags(query)) {
                        if (token.charAt(0) == '<') {
                            sb.append(token);
                        } else {
                            rebuild.append(token);
                        }
                    }
                    query = unescapeXml(rebuild.toString());
                }
                try {
                    query = StringUtils.strip(query);
                    final JsonNode node = evaluateQuery(query);
                    if (node == null) {
                        unresolvedPlaceholders.add(query);
                        putDataset(query, null);
                        sb.append(UNRESOLVABLE_PLACEHOLDER_MARK).append(query).append(UNRESOLVABLE_PLACEHOLDER_MARK);
                    } else if (node.isValueNode()) {
                        sb.append(node.asText());
                        // Remember even if it is an empty string
                        textAdded = true;
                    } else {
                        sb.append(node);
                    }
                } catch (UnresolvedDatasetException e) {
                    unresolvedDatasets.add(e.getDatasetName());
                    sb.append("{{").append(query).append("}}");
                }
                placeholderAt = -1;
                offset = ++i + 1;
            }
        }
        if (sb.length() == 0 && !textAdded) {
            return template;
        }
        if (placeholderAt >= 0) {
            unresolvedPlaceholders.add("Lack of closing tag: "
                    + StringUtils.abbreviate(template.substring(placeholderAt), 0, 20));
            sb.append(UNRESOLVABLE_PLACEHOLDER_MARK).append(template, placeholderAt + 2, template.length());
        } else {
            sb.append(template, offset, template.length());
        }
        template = sb.toString();
        if (!unresolvedDatasets.isEmpty() || !unresolvedPlaceholders.isEmpty()) {
            throw new NoValuePresentException(unresolvedDatasets, unresolvedPlaceholders, template);
        }
        return fillInPlaceholderLoop(template, isXml);
    }

    /**
     * <p>XML template version of {@code fillInPlaceholderWithResolver()}.
     * Unescapes placeholder before the resolution process of that placeholder.
     * Useful to merge docx template document.</p>
     *
     * @param template the template to be executed
     * @param dictionaryFinder a callback function to return solution query statement
     * @param dataFinder a callback function to process database query statement
     * @param progress a {@code ResolverProgress} object to log the resolver progress steps
     * @return The merged text content
     * @see #fillInPlaceholderWithResolver
     * @throws NoValuePresentException if any placeholder was unable to resolve
     */
    public String fillInXmlPlaceholderWithResolver(final String template,
                                                   final Function<String, String> dictionaryFinder,
                                                   final BiFunction<String, String, Josson> dataFinder,
                                                   final ResolverProgress progress) throws NoValuePresentException {
        if (StringUtils.isBlank(template)) {
            return template;
        }
        try {
            return fillInPlaceholderWithResolver(template, dictionaryFinder, dataFinder, true, progress);
        } finally {
            if (progress.isAutoMarkEnd()) {
                progress.markEnd();
            }
        }
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
     */
    public String fillInPlaceholderWithResolver(final String template, final Function<String, String> dictionaryFinder,
                                                final BiFunction<String, String, Josson> dataFinder,
                                                final ResolverProgress progress) throws NoValuePresentException {
        if (StringUtils.isBlank(template)) {
            return template;
        }
        try {
            return fillInPlaceholderWithResolver(template, dictionaryFinder, dataFinder, false, progress);
        } finally {
            if (progress.isAutoMarkEnd()) {
                progress.markEnd();
            }
        }
    }

    private String fillInPlaceholderWithResolver(String template, final Function<String, String> dictionaryFinder,
                                                 final BiFunction<String, String, Josson> dataFinder,
                                                 final boolean isXml,
                                                 final ResolverProgress progress) throws NoValuePresentException {
        final Set<String> unresolvablePlaceholders = new HashSet<>();
        final Set<String> unresolvedDatasetNames = new HashSet<>();
        final List<String> checkInfiniteLoop = new ArrayList<>();
        for (; ; progress.nextRound()) {
            try {
                if (!unresolvedDatasetNames.isEmpty()) {
                    throw new NoValuePresentException(new HashSet<>(unresolvedDatasetNames), null);
                }
                template = fillInPlaceholderLoop(template, isXml);
                break;
            } catch (NoValuePresentException e) {
                if (e.getPlaceholders() == null) {
                    unresolvedDatasetNames.clear();
                } else {
                    unresolvablePlaceholders.addAll(e.getPlaceholders());
                    template = e.getContent();
                }
                final Map<String, String> namedQueries = new HashMap<>();
                e.getDatasetNames().forEach(name -> {
                    checkInfiniteLoop.add(name);
                    final int half = checkInfiniteLoop.size() / 2;
                    int i = checkInfiniteLoop.size() - 2;
                    for (int j = i; j >= half; j--) {
                        if (checkInfiniteLoop.get(j).equals(name)) {
                            for (int k = j - 1; i >= j; i--, k--) {
                                if (!checkInfiniteLoop.get(k).equals(checkInfiniteLoop.get(i))) {
                                    break;
                                }
                            }
                            if (i < j) {
                                unresolvablePlaceholders.add(name);
                                putDataset(name, null);
                                return;
                            }
                            break;
                        }
                    }
                    String findQuery = dictionaryFinder.apply(name);
                    if (findQuery == null) {
                        putDataset(name, null);
                        return;
                    }
                    try {
                        findQuery = fillInPlaceholderLoop(findQuery, false);
                        if (!buildDataset(name, findQuery, dictionaryFinder, dataFinder, progress)) {
                            namedQueries.put(name, findQuery);
                            unresolvedDatasetNames.remove(name);
                        }
                    } catch (NoValuePresentException ex) {
                        if (ex.getPlaceholders().isEmpty()) {
                            ex.getDatasetNames().stream()
                                    .filter(s -> !namedQueries.containsKey(s))
                                    .forEach(unresolvedDatasetNames::add);
                        } else {
                            unresolvablePlaceholders.addAll(ex.getPlaceholders());
                            unresolvablePlaceholders.add(name);
                            putDataset(name, null);
                        }
                    }
                });
                if (!namedQueries.isEmpty()) {
                    progress.addStep("Resolving " + namedQueries);
                    namedQueries.forEach((name, findQuery) -> {
                        try {
                            final JsonNode node = evaluateQuery(findQuery);
                            if (node == null) {
                                unresolvablePlaceholders.add(name);
                                putDataset(name, null);
                            } else {
                                putDataset(name, Josson.create(node));
                                unresolvedDatasetNames.remove(name);
                                progress.addResolvedNode(name, node);
                            }
                        } catch (UnresolvedDatasetException ex) {
                            unresolvedDatasetNames.add(ex.getDatasetName());
                        }
                    });
                }
            }
        }
        if (!unresolvablePlaceholders.isEmpty()) {
            progress.addUnresolvableStep("placeholders " + unresolvablePlaceholders);
            throw new NoValuePresentException(null, unresolvablePlaceholders, template);
        }
        return template;
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
     */
    public JsonNode evaluateQueryWithResolver(final String query, final Function<String, String> dictionaryFinder,
                                              final BiFunction<String, String, Josson> dataFinder,
                                              final ResolverProgress progress) {
        final JsonNode node = evaluateQueryWithResolverLoop(query, dictionaryFinder, dataFinder, progress);
        progress.addQueryResult(node);
        return node;
    }

    private JsonNode evaluateQueryWithResolverLoop(final String query, final Function<String, String> dictionaryFinder,
                                                   final BiFunction<String, String, Josson> dataFinder,
                                                   final ResolverProgress progress) {
        for (; ; progress.nextRound()) {
            try {
                return evaluateQuery(query);
            } catch (UnresolvedDatasetException e) {
                final String name = e.getDatasetName();
                JsonNode node = null;
                String findQuery = dictionaryFinder.apply(name);
                if (findQuery != null) {
                    try {
                        findQuery = fillInPlaceholderWithResolver(
                                findQuery, dictionaryFinder, dataFinder, false, progress);
                        if (buildDataset(name, findQuery, dictionaryFinder, dataFinder, progress)) {
                            continue;
                        }
                        progress.addResolvingFrom(name, findQuery);
                        node = evaluateQueryWithResolverLoop(findQuery, dictionaryFinder, dataFinder, progress);
                    } catch (NoValuePresentException ex) {
                        // ignore
                    }
                }
                putDataset(name, node == null ? null : Josson.create(node));
                progress.addResolvedNode(name, node);
            }
        }
    }

    /**
     * Evaluate a Jossons query to retrieve data from the stored dataset mapping.
     *
     * @param query the Jossons query
     * @return The resulting Jackson JsonNode
     * @throws UnresolvedDatasetException if the query is looking for a non-exists dataset
     */
    public JsonNode evaluateQuery(final String query) throws UnresolvedDatasetException {
        String ifTrueValue = null;
        final List<TernaryStep> steps = decomposeTernarySteps(query);
        for (TernaryStep step : steps) {
            JsonNode node = evaluateStatement(step.getStatement());
            if (step.getIfTrueValue() == null) {
                return node;
            }
            ifTrueValue = step.getIfTrueValue();
            if (node != null && !node.isNull()) {
                if (ifTrueValue.isEmpty()) {
                    if (!(node.isTextual() && node.textValue().isEmpty())) {
                        return node;
                    }
                } else if (node.asBoolean()) {
                    node = evaluateStatement(ifTrueValue);
                    if (node != null) {
                        return node;
                    }
                }
            }
        }
        return ifTrueValue == null ? null : TextNode.valueOf("");
    }

    /**
     * Evaluate a Jossons statement to retrieve data from the stored dataset mapping.
     *
     * @param statement the Jossons statement
     * @return The resulting Jackson JsonNode
     * @throws UnresolvedDatasetException if the query is looking for a non-exists dataset
     */
    public JsonNode evaluateStatement(final String statement) throws UnresolvedDatasetException {
        try {
            return toValueNode(statement);
        } catch (NumberFormatException e) {
            // continue
        }
        try {
            return new OperationStackForDatasets(datasets).evaluate(statement);
        } catch (UnsupportedOperationException e) {
            if (e.getCause() instanceof UnresolvedDatasetException) {
                throw (UnresolvedDatasetException) e.getCause();
            }
            throw e;
        }
    }
}

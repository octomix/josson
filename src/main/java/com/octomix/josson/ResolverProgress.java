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

import java.util.ArrayList;
import java.util.List;

/**
 * Record all the resolution progress steps.
 */
public class ResolverProgress {

    private ResolverDebugLevel debugLevel = ResolverDebugLevel.SHOW_CONTENT_OF_VALUE_NODE_ONLY;

    private boolean autoMarkEnd = true;

    private int round = 1;

    private boolean roundStarted;

    private final List<String> steps = new ArrayList<>();

    /**
     * Create a {@code ResolverProgress} for Jossons to record resolution progress steps.
     */
    public ResolverProgress() {
    }

    /**
     * Create a {@code ResolverProgress} for Jossons to record resolution progress steps.
     * The given subject is added and to be the 1st progress step entry.
     *
     * @param subject the 1st progress step entry
     */
    public ResolverProgress(final String subject) {
        steps.add(subject);
    }

    /**
     * Set the resolver debug level.
     * Default value is {@code SHOW_CONTENT_OF_VALUE_NODE_ONLY}
     *
     * @param level the {@code ResolverDebugLevel}
     * @return {@code this}
     */
    public ResolverProgress debugLevel(final ResolverDebugLevel level) {
        debugLevel = level;
        return this;
    }

    /**
     * Get the current resolver debug level.
     * Default value is {@code SHOW_CONTENT_OF_VALUE_NODE_ONLY}
     *
     * @return Current resolver debug level
     */
    public ResolverDebugLevel getDebugLevel() {
        return debugLevel;
    }

    /**
     * Set the auto-add "End" flag.
     * By default, the last step "End" is added automatically.
     *
     * @param auto whether the last step "End" is added automatically
     * @return {@code this}
     */
    public ResolverProgress autoMarkEnd(final boolean auto) {
        autoMarkEnd = auto;
        return this;
    }

    /**
     * Ge the current status of the auto-add "End" flag
     * By default, the last step "End" is added automatically.
     *
     * @return Current auto-add "End" flag
     */
    public boolean isAutoMarkEnd() {
        return autoMarkEnd;
    }

    /**
     * Programmatically add the "End" step.
     */
    public void markEnd() {
        addStep("End");
    }

    /**
     * Get the progress steps after a resolution process.
     *
     * @return The resolution progress steps
     */
    public List<String> getSteps() {
        return steps;
    }

    /**
     * Reset the resolution progress steps.
     */
    public void reset() {
        steps.clear();
        round = 1;
        roundStarted = false;
    }

    /**
     * Goto next round.
     */
    void nextRound() {
        if (roundStarted) {
            roundStarted = false;
            round++;
        }
    }

    /**
     * Add a resolution step.
     *
     * @param name the resolving dataset name
     * @param query the query statement
     */
    void addResolvingFrom(final String name, final String query) {
        addStep(String.format("Resolving %s from %s", name, query));
    }

    /**
     * Add a resolution step.
     *
     * @param name the related dataset name
     * @param node the resolved {@code JsonNode}, {@code null} means the dataset is unresolvable.
     */
    void addResolvedNode(final String name, final JsonNode node) {
        if (node == null) {
            addUnresolvableStep(name);
        } else {
            addResolvedStep(name, resolvedValue(node));
        }
    }

    /**
     * Add a resolution step.
     *
     * @param name the related dataset name
     * @param dataset the resolved {@code Josson}, {@code null} means the dataset is unresolvable.
     */
    void addResolvedDataset(final String name, final Josson dataset) {
        if (dataset == null || dataset.getNode() == null) {
            addUnresolvableStep(name);
        } else {
            addResolvedStep(name, simplifyResolvedValue(dataset.getNode()));
        }
    }

    /**
     * Add a query result to the resolution steps.
     *
     * @param node the resolved {@code JsonNode}
     */
    void addQueryResult(final JsonNode node) {
        addStep("Query result = " + (node == null ? "null" : resolvedValue(node)));
    }

    /**
     * Add a unresolvable resolution step.
     *
     * @param name the unresolvable dataset name
     */
    void addUnresolvableStep(final String name) {
        addStep("Unresolvable " + name);
    }

    /**
     * Add a resolved resolution step.
     *
     * @param name the resolved dataset name
     * @param value the resolved value
     */
    void addResolvedStep(final String name, final String value) {
        addStep(String.format("Resolved %s=%s", name, value));
    }

    /**
     * Add a resolution step.
     *
     * @param step the resolution step detail
     */
    void addStep(final String step) {
        steps.add(String.format("Round %d : %s", round, step.replaceAll("\n", "\\\\n")));
        roundStarted = true;
    }

    private String resolvedValue(final JsonNode node) {
        switch (debugLevel) {
            case SHOW_CONTENT_UP_TO_ARRAY_NODE:
                if (node.isArray()) {
                    return node.toString();
                }
                // fallthrough
            case SHOW_CONTENT_UP_TO_OBJECT_NODE:
                if (node.isObject()) {
                    return node.toString();
                }
                // fallthrough
        }
        return simplifyResolvedValue(node);
    }

    private String simplifyResolvedValue(final JsonNode node) {
        if (node.isObject()) {
            return String.format("Object with %d elements", node.size());
        }
        if (node.isArray()) {
            return String.format("Array with %d elements", node.size());
        }
        return node.toString();
    }
}

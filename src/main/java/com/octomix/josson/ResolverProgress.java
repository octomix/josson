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
     * @return Current status of the auto-add "End" flag
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

    void nextRound() {
        if (roundStarted) {
            roundStarted = false;
            round++;
        }
    }

    void addResolvingFrom(final String name, final String query) {
        addStep("Resolving " + name + " from " + query);
    }

    void addResolvedNode(final String name, final JsonNode node) {
        if (node == null) {
            addStep("Unresolvable " + name);
        } else {
            addStep("Resolved " + name + " = " + resolvedValue(node));
        }
    }

    void addResolvedDataset(final String name, final Josson dataset) {
        if (dataset == null || dataset.getNode() == null) {
            addStep("Unresolvable " + name);
        } else {
            addStep("Resolved " + name + " = " + simplifyResolvedValue(dataset.getNode()));
        }
    }

    void addQueryResult(final JsonNode node) {
        addStep("Query result = " + (node == null ? "null" : resolvedValue(node)));
    }

    void addStep(final String step) {
        steps.add("Round " + round + " : " + step);
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
            return "Object with " + node.size() + " elements";
        }
        if (node.isArray()) {
            return "Array with " + node.size() + " elements";
        }
        return node.toString();
    }
}

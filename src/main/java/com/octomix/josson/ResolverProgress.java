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
import java.util.stream.Collectors;

/**
 * Record all the resolution progress steps.
 */
public class ResolverProgress {

    private ResolverDebugLevel debugLevel = ResolverDebugLevel.SHOW_CONTENT_OF_VALUE_NODE_ONLY;

    private boolean autoMarkEnd = true;

    private int round = 1;

    private boolean roundStarted;

    private final List<Step> steps = new ArrayList<>();

    private enum StepType {
        SUBJECT,
        MESSAGE,
        RESOLVING,
        RESOLVED,
        UNRESOLVABLE,
        DICTIONARY,
        DATA_FINDER
    }

    private static class Step {

        private final StepType stepType;

        private final int round;

        private final String name;

        private final String message;

        private final JsonNode node;

        private Step(final StepType stepType, final int round, final String name, final String message, final JsonNode node) {
            this.stepType = stepType;
            this.round = round;
            this.name = name;
            this.message = message;
            this.node = node;
        }
    }

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
        steps.add(new Step(StepType.SUBJECT, 0, null, subject, null));
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
        addMessageStep("End");
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
     * Reset the resolution progress steps.
     * The given subject is added and to be the 1st progress step entry.
     *
     * @param subject the 1st progress step entry
     */
    public void reset(final String subject) {
        reset();
        steps.add(new Step(StepType.SUBJECT, 0, null, subject, null));
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
     * @param name the related dataset name
     * @param node the resolved {@code JsonNode}, {@code null} means the dataset is unresolvable.
     */
    void addResolvedNode(final String name, final JsonNode node) {
        if (node == null) {
            addUnresolvableStep(name);
        } else {
            addResolvedStep(name, node);
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
            addMessageStep(String.format("Resolved %s to %s", name, simplifyResolvedValue(dataset.getNode())));
        }
    }

    /**
     * Add a resolution message.
     *
     * @param message the message to display
     */
    void addMessageStep(final String message) {
        addStep(new Step(StepType.MESSAGE, round, null, message, null));
    }

    /**
     * Add a resolution step.
     *
     * @param name the resolving dataset name
     * @param query the query statement
     */
    void addResolvingStep(final String name, final String query) {
        if (steps.stream().noneMatch(step -> step.stepType == StepType.RESOLVING && step.name.equals(name))) {
            addStep(new Step(StepType.RESOLVING, round, name, query, null));
        }
    }

    /**
     * Add a matched dictionary entry step.
     *
     * @param name the key of the dictionary entry
     */
    void addDictionaryStep(final String name) {
        addStep(new Step(StepType.DICTIONARY, round, name, null, null));
    }

    /**
     * Add a matched data finder entry step.
     *
     * @param name the key of the data finder entry
     * @param query the data query statement
     */
    void addDataFinderStep(final String name, final String query) {
        addStep(new Step(StepType.DATA_FINDER, round, name, query, null));
    }

    /**
     * Add a resolved resolution step.
     *
     * @param name the resolved dataset name
     * @param node the resolved value
     */
    void addResolvedStep(final String name, final JsonNode node) {
        addStep(new Step(StepType.RESOLVED, round, name, null, node));
    }

    /**
     * Add a unresolvable resolution step.
     *
     * @param name the unresolvable dataset name
     */
    void addUnresolvableStep(final String name) {
        addStep(new Step(StepType.UNRESOLVABLE, round, name, null, null));
    }

    /**
     * Add a resolution step.
     *
     * @param step the resolution step detail
     */
    private void addStep(final Step step) {
        steps.add(step);
        roundStarted = true;
    }

    /**
     * Get the progress steps after a resolution process.
     *
     * @return The resolution progress steps
     */
    public List<String> getSteps() {
        return steps.stream().map(this::formatStep).collect(Collectors.toList());
    }

    private String formatStep(final Step step) {
        String message = null;
        switch (step.stepType) {
            case SUBJECT:
                return step.message;

            case MESSAGE:
                message = step.message;
                break;

            case RESOLVING:
                message = String.format("Resolving %s from %s", step.name, step.message);
                break;

            case RESOLVED:
                message = String.format("Resolved %s = %s", step.name, resolvedValue(step.node));
                break;

            case UNRESOLVABLE:
                message = "Unresolvable " + step.name;
                break;

            case DICTIONARY:
                message = String.format("Located %s in dictionary", step.name);
                break;

            case DATA_FINDER:
                message = String.format("Matched %s to data query %s", step.name, step.message);
                break;
        }
        return String.format("Round %d : %s", step.round, message.replace("\n", "\\\\n"));
    }

    private String resolvedValue(final JsonNode node) {
        if (node == null) {
            return null;
        }
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

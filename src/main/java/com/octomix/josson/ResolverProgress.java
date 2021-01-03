package com.octomix.josson;

import java.util.ArrayList;
import java.util.List;

public class ResolverProgress {

    private int round = 1;
    private final List<String> steps = new ArrayList<>();

    public enum ShowResolvedValueMode {
        VALUE_NODE_ONLY,
        ALL_NODES
    }

    public ResolverProgress() {
    }

    public ResolverProgress(String subject) {
        steps.add(subject);
    }

    public List<String> getSteps() {
        return steps;
    }

    void nextRound() {
        round++;
    }

    void addStep(String step) {
        steps.add("Round " + round + " : " + step);
    }

    public void markCompleted() {
        addStep("Completed");
    }
}

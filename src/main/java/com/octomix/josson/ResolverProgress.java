package com.octomix.josson;

import java.util.ArrayList;
import java.util.List;

public class ResolverProgress {

    private int round = 0;
    private final List<String> steps = new ArrayList<>();

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

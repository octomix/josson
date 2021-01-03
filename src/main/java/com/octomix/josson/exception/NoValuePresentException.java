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
                + (placeholders == null ? "" : "Unresolved placeholders " + placeholders + ". "));
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

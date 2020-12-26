package com.octomix.josson.exception;

import java.util.Set;

public class NoValuePresentException extends Exception {

    private final Set<String> dataSourceNames;
    private final Set<String> placeholders;
    private final String content;

    public NoValuePresentException(Set<String> dataSourceNames, Set<String> placeholders) {
        this(dataSourceNames, placeholders, null);
    }

    public NoValuePresentException(Set<String> dataSourceNames, Set<String> placeholders, String content) {
        super((dataSourceNames == null ? "" : "Unresolved data sources " + dataSourceNames + ". ")
                + (placeholders == null ? "" : "Unresolved placeholders " + placeholders + ". "));
        this.dataSourceNames = dataSourceNames;
        this.placeholders = placeholders;
        this.content = content;
    }

    public Set<String> getDataSourceNames() {
        return dataSourceNames;
    }

    public Set<String> getPlaceholders() {
        return placeholders;
    }

    public String getContent() {
        return content;
    }
}

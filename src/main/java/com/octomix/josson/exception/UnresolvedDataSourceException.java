package com.octomix.josson.exception;

public class UnresolvedDataSourceException extends Exception {

    private final String dataSourceName;

    public UnresolvedDataSourceException(String dataSourceName) {
        super("Unresolved data source " + dataSourceName);
        this.dataSourceName = dataSourceName;
    }

    public String getDataSourceName() {
        return dataSourceName;
    }
}

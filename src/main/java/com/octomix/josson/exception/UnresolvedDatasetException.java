package com.octomix.josson.exception;

public class UnresolvedDatasetException extends Exception {

    private final String datasetName;

    public UnresolvedDatasetException(String datasetName) {
        super("Unresolved data set " + datasetName);
        this.datasetName = datasetName;
    }

    public String getDatasetName() {
        return datasetName;
    }
}

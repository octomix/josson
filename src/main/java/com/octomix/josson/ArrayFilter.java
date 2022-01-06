package com.octomix.josson;

class ArrayFilter {

    enum FilterMode {
        FILTER_FIND_FIRST(' '),
        FILTER_FIND_ALL('*'),
        FILTER_NESTED_ARRAY('@');

        final char symbol;

        FilterMode(char symbol) {
            this.symbol = symbol;
        }

        static FilterMode fromSymbol(char symbol) {
            for (FilterMode mode : values()) {
                if (mode.symbol == symbol) {
                    return mode;
                }
            }
            return null;
        }
    }

    private final String nodeName;
    private final String filter;
    private final FilterMode mode;

    ArrayFilter(String nodeName, String filter, FilterMode mode) {
        this.nodeName = nodeName;
        this.filter = filter;
        this.mode = mode;
    }

    String getNodeName() {
        return nodeName;
    }

    String getFilter() {
        return filter;
    }

    FilterMode getMode() {
        return mode;
    }
}

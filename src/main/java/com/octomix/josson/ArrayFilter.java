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

    private final String arrayName;
    private final String filter;
    private final FilterMode mode;

    ArrayFilter(String arrayName, String filter, FilterMode mode) {
        this.arrayName = arrayName;
        this.filter = filter;
        this.mode = mode;
    }

    String getArrayName() {
        return arrayName;
    }

    String getFilter() {
        return filter;
    }

    FilterMode getMode() {
        return mode;
    }
}

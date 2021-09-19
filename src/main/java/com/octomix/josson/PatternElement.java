package com.octomix.josson;

import java.util.EnumSet;
import java.util.Set;

enum PatternElement {
    STRING_LITERAL,
    SQUARE_BRACKETS,
    PARENTHESES;

    static final Set<PatternElement> ALL_SYNTAX_ELEMENTS = EnumSet.of(
            STRING_LITERAL,
            SQUARE_BRACKETS,
            PARENTHESES
    );
}

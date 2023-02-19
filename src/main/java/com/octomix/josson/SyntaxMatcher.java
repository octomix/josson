/*
 * Copyright 2020-2023 Octomix Software Technology Limited
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

import com.octomix.josson.commons.StringUtils;
import com.octomix.josson.exception.SyntaxErrorException;

import java.util.Arrays;
import java.util.List;

import static com.octomix.josson.JossonCore.QUOTE_SYMBOL;
import static com.octomix.josson.exception.SyntaxErrorException.POS_AT_THE_END;

class SyntaxMatcher {

    /**
     * Pattern enclosure types.
     */
    protected enum Enclosure {

        /**
         * Enclosed by ''.
         */
        STRING_LITERAL,

        /**
         * Enclosed by [].
         */
        SQUARE_BRACKETS,

        /**
         * Enclosed by ().
         */
        PARENTHESES,

        /**
         * Enclosed by "".
         */
        ESCAPE_PATH_NAME;

        static final Enclosure[] ALL_KINDS = new Enclosure[]{
                STRING_LITERAL,
                SQUARE_BRACKETS,
                PARENTHESES,
                ESCAPE_PATH_NAME,
        };
    }

    protected static final char ESCAPE_SYMBOL = '\\';

    protected static final char ESCAPE_PATH_NAME_SYMBOL = '"';

    protected final String input;

    protected final int length;

    protected final int last;

    protected SyntaxMatcher(final String input) {
        this.input = input;
        this.length = input.length();
        this.last = this.length - 1;
    }

    protected static int eatSpaces(final String input, int beg, final int last) {
        for (; beg <= last; beg++) {
            if (!Character.isWhitespace(input.charAt(beg))) {
                break;
            }
        }
        return beg;
    }

    protected int eatSpaces(final int beg) {
        return eatSpaces(input, beg, last);
    }

    private int eatRightSpaces(final int beg, int end) {
        for (; end > beg; end--) {
            if (!Character.isWhitespace(input.charAt(end - 1))) {
                break;
            }
        }
        return end;
    }

    protected String trimOf(final int beg, final int end) {
        return rightTrimOf(eatSpaces(beg), end);
    }

    protected String rightTrimOf(final int beg, final int end) {
        return input.substring(beg, eatRightSpaces(beg, end));
    }

    protected int skipEnclosure(final int pos, final Enclosure... enclosures) {
        for (Enclosure enclosure : enclosures) {
            final int end;
            switch (enclosure) {
                case STRING_LITERAL:
                    end = matchStringLiteral(pos);
                    break;
                case SQUARE_BRACKETS:
                    end = matchSquareBrackets(pos);
                    break;
                case PARENTHESES:
                    end = matchParentheses(pos);
                    break;
                case ESCAPE_PATH_NAME:
                    end = matchEscapePathName(pos);
                    break;
                default:
                    end = 0;
            }
            if (end > 0) {
                return end;
            }
        }
        return pos;
    }

    private int findBracketEnd(int pos, final char expectedEnd) {
        while (++pos <= last) {
            switch (input.charAt(pos)) {
                case ')':
                    if (expectedEnd == ']') {
                        throw new SyntaxErrorException(input, "Invalid ')'", pos);
                    }
                    return pos;
                case ']':
                    if (expectedEnd == ')') {
                        throw new SyntaxErrorException(input, "Invalid ']'", pos);
                    }
                    return pos;
                case '(':
                    pos = findBracketEnd(pos, ')');
                    continue;
                case '[':
                    pos = findBracketEnd(pos, ']');
                    continue;
            }
            pos = skipEnclosure(pos, Enclosure.STRING_LITERAL);
        }
        throw new SyntaxErrorException(input, String.format("Missing '%c'", expectedEnd), POS_AT_THE_END);
    }

    protected int matchStringLiteral(int pos) {
        if (input.charAt(pos) != QUOTE_SYMBOL) {
            return 0;
        }
        while (++pos < last) {
            if (input.charAt(pos) == QUOTE_SYMBOL) {
                if (input.charAt(pos + 1) != QUOTE_SYMBOL) {
                    return pos;
                }
                pos++;
            }
        }
        if (pos == last && input.charAt(pos) == QUOTE_SYMBOL) {
            return pos;
        }
        throw new SyntaxErrorException(input, String.format("Expected quote symbol (%c)", QUOTE_SYMBOL), pos);
    }

    protected int matchSquareBrackets(final int pos) {
        if (input.charAt(pos) != '[') {
            return 0;
        }
        return findBracketEnd(pos, ']');
    }

    protected int matchParentheses(final int pos) {
        if (input.charAt(pos) != '(') {
            return 0;
        }
        return findBracketEnd(pos, ')');
    }

    private int matchEscapePathName(int pos) {
        if (input.charAt(pos) != ESCAPE_PATH_NAME_SYMBOL) {
            return 0;
        }
        while (++pos <= last) {
            if (input.charAt(pos) == ESCAPE_PATH_NAME_SYMBOL) {
                return pos;
            }
            if (input.charAt(pos) == ESCAPE_SYMBOL) {
                if (pos == last) {
                    break;
                }
                switch (input.charAt(pos + 1)) {
                    case ESCAPE_SYMBOL:
                    case ESCAPE_PATH_NAME_SYMBOL:
                        pos++;
                        break;
                }
            }
        }
        throw new SyntaxErrorException(input, String.format("Expected quote symbol (%c)", ESCAPE_PATH_NAME_SYMBOL), pos);
    }

    protected int skipDatasetQuery(int pos) {
        for (; pos <= last && "=!<>&|)".indexOf(input.charAt(pos)) < 0; pos++) {
            if (input.charAt(pos) == '-' && pos++ == last) {
                // skip "->"
                break;
            }
            pos = skipEnclosure(pos, Enclosure.ALL_KINDS);
        }
        return pos;
    }

    protected void matchCombineOperation(int pos, final CombineOperand leftOperand, final CombineOperator operator,
                                         final List<CombineOperation> operations) {
        int end = skipDatasetQuery(pos);
        final CombineOperand rightOperand = matchCombineOperand(pos, end, true);
        operations.add(new CombineOperation(leftOperand, operator, rightOperand));
        pos = eatSpaces(end);
        if (pos <= last) {
            if (input.charAt(pos) != '|') {
                throw new SyntaxErrorException(input, "Expecting pipe operator '|'", pos);
            }
            end = skipDatasetQuery(++pos);
            final CombineOperand nextLeftOperand = matchCombineOperand(pos, end, false);
            pos = CombineOperator.findEndingPos(input, end, last);
            final CombineOperator nextOperator = CombineOperator.fromSymbol(input.substring(end, pos));
            if (nextOperator == null) {
                throw new SyntaxErrorException(input, "Invalid join or set operator", end);
            }
            matchCombineOperation(pos, nextLeftOperand, nextOperator, operations);
        }
    }

    protected CombineOperand matchCombineOperand(final int pos, final int end, final boolean needQuery) {
        final CombineOperand operand = new SyntaxMatcher(trimOf(pos, end)).matchCombineOperand();
        if (needQuery) {
            if (!operand.hasQuery()) {
                throw new SyntaxErrorException(input, "Missing query statement", pos);
            }
        } else {
            if (operand.hasQuery()) {
                throw new SyntaxErrorException(input, "Unnecessary query statement", pos);
            }
        }
        return operand;
    }

    private CombineOperand matchCombineOperand() {
        String query = null;
        for (int pos = 0, beg = 0; pos < length; pos++) {
            switch (input.charAt(pos)) {
                case '{':
                    if (query != null) {
                        throw new SyntaxErrorException(input, "Invalid '{'", pos);
                    }
                    query = rightTrimOf(beg, pos);
                    beg = pos + 1;
                    continue;
                case '}':
                    if (query == null) {
                        throw new SyntaxErrorException(input, "Invalid '}'", pos);
                    }
                    final String ending = rightTrimOf(pos + 1, length);
                    if (!ending.isEmpty()) {
                        throw new SyntaxErrorException(input, String.format("Invalid '%s'", ending), pos + 1);
                    }
                    final String[] keys = trimOf(beg, pos).split(",");
                    if (Arrays.stream(keys).anyMatch(StringUtils::isBlank)) {
                        throw new SyntaxErrorException(input, "Missing join key");
                    }
                    return new CombineOperand(query, keys);
            }
            pos = skipEnclosure(pos, Enclosure.ALL_KINDS);
        }
        if (query != null) {
            throw new SyntaxErrorException(input, "Missing '}'", POS_AT_THE_END);
        }
        return new CombineOperand(input, null);
    }

    boolean matchObjectOrArrayJson() {
        if (last >= 1) {
            final int beg = eatSpaces(0);
            if (beg < last) {
                final char charBeg = input.charAt(beg);
                if (charBeg == '{' || charBeg == '[') {
                    final int end = eatRightSpaces(beg, last);
                    if (end > beg) {
                        return charBeg == '{' && input.charAt(end) == '}' || charBeg == '[' && input.charAt(end) == ']';
                    }
                }
            }
        }
        return false;
    }
}

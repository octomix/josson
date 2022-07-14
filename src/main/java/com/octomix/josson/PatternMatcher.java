/*
 * Copyright 2020-2022 Octomix Software Technology Limited
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

import java.util.*;

import static com.octomix.josson.ArrayFilter.FilterMode;
import static com.octomix.josson.ArrayFilter.FilterMode.*;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.commons.StringUtils.EMPTY;
import static com.octomix.josson.exception.SyntaxErrorException.POS_AT_THE_END;

/**
 * A syntax analyser.
 */
final class PatternMatcher {

    /**
     * Pattern enclosure types.
     */
    private enum Enclosure {

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
        PARENTHESES;

        static final Enclosure[] ALL_KINDS = new Enclosure[]{
            STRING_LITERAL,
            SQUARE_BRACKETS,
            PARENTHESES,
        };
    }

    private PatternMatcher() {
    }

    private static boolean isNotSpace(char ch) {
        switch (ch) {
            case ' ':
            case '\n':
            case '\r':
            case '\t':
            case '\f':
                return false;
        }
        return true;
    }

    private static int eatSpaces(final String input, int beg, final int last) {
        for (; beg <= last; beg++) {
            if (isNotSpace(input.charAt(beg))) {
                break;
            }
        }
        return beg;
    }

    private static String trimOf(final String input, final int beg, final int end) {
        return rightTrimOf(input, eatSpaces(input, beg, end - 1), end);
    }

    private static String rightTrimOf(final String input, final int beg, int end) {
        for (; end > beg; end--) {
            if (isNotSpace(input.charAt(end - 1))) {
                break;
            }
        }
        return input.substring(beg, end);
    }

    private static int skipEnclosure(final String input, final int pos, final int last, final Enclosure... enclosures) {
        for (Enclosure enclosure : enclosures) {
            int end = 0;
            switch (enclosure) {
                case STRING_LITERAL:
                    end = matchStringLiteral(input, pos, last);
                    break;
                case SQUARE_BRACKETS:
                    end = matchSquareBrackets(input, pos, last);
                    break;
                case PARENTHESES:
                    end = matchParentheses(input, pos, last);
                    break;
            }
            if (end > 0) {
                return end;
            }
        }
        return pos;
    }

    private static int matchStringLiteral(final String input, int pos, final int last) {
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

    private static int matchSquareBrackets(final String input, final int pos, final int last) {
        if (input.charAt(pos) != '[') {
            return 0;
        }
        return findBracketEnd(input, pos, last, ']');
    }

    private static int matchParentheses(final String input, final int pos, final int last) {
        if (input.charAt(pos) != '(') {
            return 0;
        }
        return findBracketEnd(input, pos, last, ')');
    }

    private static int findBracketEnd(final String input, int pos, final int last, final char expectedEnd) {
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
                    pos = findBracketEnd(input, pos, last, ')');
                    continue;
                case '[':
                    pos = findBracketEnd(input, pos, last, ']');
                    continue;
            }
            pos = skipEnclosure(input, pos, last, Enclosure.STRING_LITERAL);
        }
        throw new SyntaxErrorException(input, String.format("Missing '%c'", expectedEnd), POS_AT_THE_END);
    }

    private static int skipDatasetQuery(final String input, int pos, final int last) {
        for (; pos <= last && "=!<>&|)".indexOf(input.charAt(pos)) < 0; pos++) {
            if (input.charAt(pos) == '-' && pos++ == last) {
                // skip "->"
                break;
            }
            pos = skipEnclosure(input, pos, last, Enclosure.ALL_KINDS);
        }
        return pos;
    }

    static String[] matchDatasetQuery(final String input) {
        final int last = input.length() - 1;
        int pos = eatSpaces(input, 0, last);
        for (int beg = pos; pos < last; pos++) {
            switch (input.charAt(pos)) {
                case '[':
                case '<':
                    return null;
                case '(':
                    pos = skipEnclosure(input, pos, last, Enclosure.PARENTHESES);
                    break;
                case '-':
                    return input.charAt(pos + 1) != '>' ? null
                            : new String[]{rightTrimOf(input, beg, pos), trimOf(input, pos + 2, last + 1)};
            }
        }
        return null;
    }

    static String[] matchDbQuery(final String input) {
        final int last = input.length() - 1;
        int pos = 0;
        int arrayEnd = 0;
        String name = null;
        for (; pos < last; pos++) {
            if (input.charAt(pos) == '?') {
                if (arrayEnd > 0) {
                    if (eatSpaces(input, arrayEnd + 1, pos) != pos) {
                        return null;
                    }
                } else {
                    name = trimOf(input, 0, pos);
                }
                break;
            }
            final int end = matchSquareBrackets(input, pos, last);
            if (end > 0) {
                if (arrayEnd > 0 || eatSpaces(input, pos + 1, end) != end) {
                    return null;
                }
                arrayEnd = end;
                name = trimOf(input, 0, pos);
            } else if (matchStringLiteral(input, pos, last) > 0 || matchParentheses(input, pos, last) > 0) {
                return null;
            }
        }
        if (pos >= last) {
            return null;
        }
        final String query = trimOf(input, pos + 1, last + 1);
        if (query.length() >= 2) {
            final char firstChar = query.charAt(0);
            switch (firstChar) {
                case '{':
                case '[':
                    final char lastChar = query.charAt(query.length() - 1);
                    if (firstChar == '{' && lastChar == '}' || firstChar == '[' && lastChar == ']') {
                        return new String[]{name, arrayEnd == 0 ? EMPTY : "[]", query};
                    }
            }
        }
        return null;
    }

    static ArrayFilter matchFilterQuery(final String input) {
        final int last = input.length() - 1;
        for (int pos = 0; pos <= last; pos++) {
            final int end = matchSquareBrackets(input, pos, last);
            if (end > 0) {
                final String arrayName = rightTrimOf(input, 0, pos);
                final String filter = trimOf(input, pos + 1, end);
                pos = eatSpaces(input, end + 1, last);
                FilterMode mode = FILTRATE_FIND_FIRST;
                if (pos <= last) {
                    mode = fromSymbol(input.charAt(pos));
                    if (mode != null) {
                        pos = eatSpaces(input, ++pos, last);
                    }
                    if (pos <= last) {
                        throw new SyntaxErrorException(input, "Invalid filter expression", pos);
                    }
                }
                if (mode != FILTRATE_DIVERT_ALL && filter.isEmpty()) {
                    throw new SyntaxErrorException(input, "Missing filter expression", end);
                }
                return new ArrayFilter(arrayName, filter, mode);
            }
            pos = skipEnclosure(input, pos, last, Enclosure.STRING_LITERAL, Enclosure.PARENTHESES);
        }
        final FilterMode mode = fromSymbol(input.charAt(last));
        if (mode == FILTRATE_COLLECT_ALL || mode == FILTRATE_DIVERT_ALL) {
            return new ArrayFilter(rightTrimOf(input, 0, last), null, mode);
        }
        return new ArrayFilter(input, null, FILTRATE_COLLECT_ALL);
    }

    static String[] matchFunctionAndArgument(final String input, final boolean allowDivert) {
        final int last = input.length() - 1;
        for (int pos = 0; pos <= last; pos++) {
            final int end = matchParentheses(input, pos, last);
            if (end > 0) {
                final String ending = trimOf(input, end + 1, last + 1);
                if (!ending.isEmpty()
                        && (!allowDivert || ending.length() > 1 || ending.charAt(0) != FILTRATE_DIVERT_ALL.getSymbol())) {
                    throw new SyntaxErrorException(input, String.format("Invalid '%s' after function", ending), end);
                }
                final String name = rightTrimOf(input, 0, pos);
                if (name.isEmpty()) {
                    throw new SyntaxErrorException(input, "Missing function name");
                }
                return new String[]{name, trimOf(input, pos + 1, end)};
            } else if (matchStringLiteral(input, pos, last) > 0 || matchSquareBrackets(input, pos, last) > 0) {
                return null;
            }
        }
        return null;
    }

    static JoinAndSetOperator matchJoinOrSetOperation(final String input) {
        final int last = input.length() - 1;
        int pos = skipDatasetQuery(input, 0, last);
        final String leftQuery = trimOf(input, 0, pos);
        if (pos >= last || "<>".indexOf(input.charAt(pos)) < 0) {
            return null;
        }
        final int beg = pos++;
        for (; pos <= last; pos++) {
            if ("<=>!+-un".indexOf(input.charAt(pos)) < 0) {
                break;
            }
        }
        final JoinAndSetOperator operator = JoinAndSetOperator.fromSymbol(input.substring(beg, pos));
        if (operator == null) {
            throw new SyntaxErrorException(input, "Invalid operator", beg);
        }
        final int end = skipDatasetQuery(input, pos, last);
        final String rightQuery = trimOf(input, pos, last + 1);
        pos = eatSpaces(input, end, last);
        if (pos <= last) {
            throw new SyntaxErrorException(input, "Too many arguments for join operation", pos);
        }
        final JoinAndSetOperand leftDataset = matchJoinDatasetQuery(leftQuery);
        final JoinAndSetOperand rightDataset = matchJoinDatasetQuery(rightQuery);
        try {
            return operator.init(leftDataset, rightDataset);
        } catch (IllegalArgumentException e) {
            throw new SyntaxErrorException(input, e.getMessage());
        }
    }

    private static JoinAndSetOperand matchJoinDatasetQuery(final String input) {
        final int last = input.length() - 1;
        String query = null;
        for (int pos = 0, beg = 0; pos <= last; pos++) {
            switch (input.charAt(pos)) {
                case '{':
                    if (query != null || pos == 0) {
                        throw new SyntaxErrorException(input, "Invalid '{'", pos);
                    }
                    query = rightTrimOf(input, beg, pos);
                    beg = pos + 1;
                    continue;
                case '}':
                    if (query == null) {
                        throw new SyntaxErrorException(input, "Invalid '}'", pos);
                    }
                    final String ending = rightTrimOf(input, pos + 1, last + 1);
                    if (!ending.isEmpty()) {
                        throw new SyntaxErrorException(input, String.format("Invalid '%s'", ending), pos + 1);
                    }
                    final String[] keys = trimOf(input, beg, pos).split(",");
                    if (Arrays.stream(keys).anyMatch(StringUtils::isBlank)) {
                        throw new SyntaxErrorException(input, "Missing join key");
                    }
                    return new JoinAndSetOperand(query, keys);
            }
            pos = skipEnclosure(input, pos, last, Enclosure.ALL_KINDS);
        }
        if (query != null) {
            throw new SyntaxErrorException(input, "Missing '}'", POS_AT_THE_END);
        }
        return new JoinAndSetOperand(input, null);
    }

    static List<String> decomposePaths(final String input) {
        final List<String> paths = new ArrayList<>();
        final int last = input.length() - 1;
        String isInt = null;
        for (int pos = 0; pos <= last; pos = eatSpaces(input, ++pos, last)) {
            final int beg = pos;
            do {
                if (input.charAt(pos) == '.') {
                    break;
                }
                pos = skipEnclosure(input, pos, last, Enclosure.ALL_KINDS);
            } while (++pos <= last);
            final String path = trimOf(input, beg, pos);
            if (path.isEmpty()) {
                throw new SyntaxErrorException(input, "Invalid '.'", pos);
            }
            try {
                if (isInt == null) {
                    if (pos > last) {
                        paths.add(path);
                    } else {
                        try {
                            Integer.parseInt(path);
                            isInt = path;
                        } catch (NumberFormatException e) {
                            paths.add(path);
                        }
                    }
                } else {
                    if (StringUtils.isNumeric(path)) {
                        paths.add(String.format("%s.%s", isInt, path));
                    } else {
                        paths.add(isInt);
                        paths.add(path);
                    }
                    isInt = null;
                }
            } catch (IllegalArgumentException e) {
                throw new SyntaxErrorException(input, e.getMessage(), beg);
            }
        }
        return paths;
    }

    static List<OperationStep> decomposeStatement(final String input) {
        final List<OperationStep> steps = new ArrayList<>();
        final int last = input.length() - 1;
        int pos = 0;
        while (pos <= last) {
            int beg = pos;
            while ("=!<>&|~".indexOf(input.charAt(pos)) >= 0) {
                if (pos++ == last) {
                    throw new SyntaxErrorException(input, "Invalid syntax", POS_AT_THE_END);
                }
            }
            final int end = pos - 1;
            if (end > beg && input.charAt(end) == '!') {
                pos = end;
            }
            final String token = input.substring(beg, pos);
            final Operator operator = Operator.fromSymbol(token);
            if (operator == null) {
                throw new SyntaxErrorException(input, "Invalid operator: " + token, beg);
            }
            beg = eatSpaces(input, pos, last);
            switch (input.charAt(beg)) {
                case '(':
                case ')':
                    pos = beg + 1;
                    break;
                default:
                    pos = skipDatasetQuery(input, beg, last);
                    break;
            }
            final String expression = rightTrimOf(input, beg, pos);
            steps.add(new OperationStep(operator, expression));
            pos = eatSpaces(input, pos, last);
        }
        return steps;
    }

    static List<String> decomposeFunctionParameters(final String input, final int min, final int max) {
        final List<String> params = new ArrayList<>();
        final int last = input.length() - 1;
        for (int pos = 0; pos <= last; pos++) {
            final int beg = pos;
            do {
                if (input.charAt(pos) == ',') {
                    break;
                }
                pos = skipEnclosure(input, pos, last, Enclosure.ALL_KINDS);
            } while (++pos <= last);
            final String param = trimOf(input, beg, pos);
            if (param.isEmpty()) {
                if (max < 0) {
                    throw new SyntaxErrorException(input, "Argument cannot be empty", beg);
                }
                if (params.size() < min) {
                    break;
                }
            } else if (params.size() == max) {
                throw new SyntaxErrorException(input, String.format("Exceeded maximum of %d arguments", max));
            }
            params.add(param);
        }
        if (min > 0 && params.size() < min) {
            throw new SyntaxErrorException(input,
                    String.format("Expected %s%d arguments", min == max ? EMPTY : "at least ", min));
        }
        for (int i = params.size() - 1; i >= 0; i--) {
            if (params.get(i).isEmpty()) {
                params.remove(i);
            } else {
                break;
            }
        }
        return params;
    }

    static List<TernaryStep> decomposeTernarySteps(final String input) {
        final List<TernaryStep> steps = new ArrayList<>();
        final int len = input.length();
        final int last = len - 1;
        for (int pos = 0, beg = 0, endIf = -1; pos <= len; pos++) {
            if (pos == len || input.charAt(pos) == ':') {
                final String condition;
                String value = null;
                if (endIf < 0) {
                    condition = trimOf(input, beg, pos);
                } else {
                    condition = trimOf(input, beg, endIf);
                    value = trimOf(input, endIf + 1, pos);
                    endIf = -1;
                }
                if (condition.isEmpty()) {
                    throw new SyntaxErrorException(input, "Missing argument", beg);
                }
                steps.add(new TernaryStep(condition, value));
                beg = pos + 1;
            } else if (input.charAt(pos) == '?') {
                endIf = pos;
            } else {
                pos = skipEnclosure(input, pos, last, Enclosure.ALL_KINDS);
            }
        }
        return steps;
    }

    static Pair<String, String> decomposeNameAndPath(final String input) throws UnknownFormatConversionException {
        final int len = input.length();
        final int last = len - 1;
        for (int pos = 0; pos < len; pos++) {
            if (input.charAt(pos) == ':') {
                final String name = trimOf(input, 0, pos);
                if (name.isEmpty()) {
                    throw new SyntaxErrorException(input, "Missing name");
                }
                checkElementName(name);
                final String path = trimOf(input, pos + 1, len);
                return Pair.of(name, path.isEmpty() ? null : path);
            } else {
                pos = skipEnclosure(input, pos, last, Enclosure.ALL_KINDS);
            }
        }
        return Pair.of(getLastElementName(input), input);
    }

    static String getLastElementName(final String path) {
        final List<String> paths = decomposePaths(path);
        if (paths.isEmpty()) {
            throw new UnknownFormatConversionException("undefined");
        }
        String funcName = null;
        for (int i = paths.size() - 1; i >= 0; i--) {
            final String[] funcAndArgs = matchFunctionAndArgument(paths.get(i), true);
            if (funcAndArgs == null) {
                return matchFilterQuery(paths.get(i)).getNodeName();
            }
            if (funcName == null) {
                funcName = funcAndArgs[0];
            }
        }
        throw new UnknownFormatConversionException("_" + funcName);
    }

    static void checkElementName(final String name) {
        if (name.contains(".")) {
            throw new SyntaxErrorException(name, "Illegal '.' in element name");
        }
    }
}

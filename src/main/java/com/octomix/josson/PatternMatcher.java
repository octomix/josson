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

import java.util.*;

import static com.octomix.josson.ArrayFilter.FilterMode;
import static com.octomix.josson.ArrayFilter.FilterMode.*;
import static com.octomix.josson.JossonCore.*;

class PatternMatcher {

    private enum Enclosure {
        STRING_LITERAL,
        SQUARE_BRACKETS,
        PARENTHESES;

        static final Enclosure[] ALL_KINDS = new Enclosure[]{
            STRING_LITERAL,
            SQUARE_BRACKETS,
            PARENTHESES
        };
    }

    private static class AtPositionException extends IllegalArgumentException {
        AtPositionException(String input, String message, int pos) {
            super(message + (pos == -1 ? " at the end" : " at position " + pos) + ": " + input);
        }
    }
    
    private static int eatSpaces(String input, int beg, int last) {
        for (;beg <= last; beg++) {
            if (input.charAt(beg) != ' ') {
                break;
            }
        }
        return beg;
    }

    private static String trimOf(String input, int beg, int end) {
        return rightTrimOf(input, eatSpaces(input, beg, end - 1), end);
    }

    private static String rightTrimOf(String input, int beg, int end) {
        for (;end > beg; end--) {
            if (input.charAt(end - 1) != ' ') {
                break;
            }
        }
        return input.substring(beg, end);
    }

    private static int skipEnclosure(String input, int pos, int last, Enclosure... enclosures) {
        for (Enclosure enclosure : enclosures) {
            int end;
            switch (enclosure) {
                case STRING_LITERAL:
                    if ((end = matchStringLiteral(input, pos, last)) > 0) {
                        return end;
                    }
                    break;
                case SQUARE_BRACKETS:
                    if ((end = matchSquareBrackets(input, pos, last)) > 0) {
                        return end;
                    }
                    break;
                case PARENTHESES:
                    if ((end = matchParentheses(input, pos, last)) > 0) {
                        return end;
                    }
                    break;
            }
        }
        return pos;
    }

    private static int matchStringLiteral(String input, int pos, int last) {
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
        throw new AtPositionException(input, "Expected quote symbol (" + QUOTE_SYMBOL + ")", pos);
    }

    private static int matchSquareBrackets(String input, int pos, int last) {
        if (input.charAt(pos) != '[') {
            return 0;
        }
        return findBracketEnd(input, pos, last, ']');
    }

    private static int matchParentheses(String input, int pos, int last) {
        if (input.charAt(pos) != '(') {
            return 0;
        }
        return findBracketEnd(input, pos, last, ')');
    }

    private static int findBracketEnd(String input, int pos, int last, char expectedEnd) {
        while (++pos <= last) {
            switch (input.charAt(pos)) {
                case ')':
                    if (expectedEnd == ']') {
                        throw new AtPositionException(input, "Invalid ')'", pos);
                    }
                    return pos;
                case ']':
                    if (expectedEnd == ')') {
                        throw new AtPositionException(input, "Invalid ']'", pos);
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
        throw new AtPositionException(input, "Missing '" + expectedEnd + "'", -1);
    }

    private static int skipDatasetQuery(String input, int pos, int last) {
        for (;pos <= last && "=!<>&|)".indexOf(input.charAt(pos)) < 0; pos++) {
            if (input.charAt(pos) == '-' && pos++ == last) {
                // skip "->"
                break;
            }
            pos = skipEnclosure(input, pos, last, Enclosure.ALL_KINDS);
        }
        return pos;
    }

    static String[] matchDatasetQuery(String input) {
        int last = input.length() - 1;
        int pos = eatSpaces(input, 0, last);
        for (int beg = pos; pos < last; pos++) {
            switch (input.charAt(pos)) {
                case '[':
                case '<':
                    return null;
                case '-':
                    return input.charAt(pos + 1) != '>' ? null :
                            new String[]{rightTrimOf(input, beg, pos), trimOf(input, pos + 2, last + 1)};
            }
        }
        return null;
    }

    static String[] matchDbQuery(String input) {
        int last = input.length() - 1;
        int pos = 0;
        int arrayEnd = 0;
        String name = null;
        for (;pos < last; pos++) {
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
            int end = matchSquareBrackets(input, pos, last);
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
        String query = trimOf(input, pos + 1, last + 1);
        if (query.length() >= 2 &&
                ((query.charAt(0) == '{' && query.charAt(query.length() - 1) == '}') ||
                 (query.charAt(0) == '[' && query.charAt(query.length() - 1) == ']'))) {
            return new String[]{name, arrayEnd == 0 ? "" : "[]", query};
        }
        return null;
    }

    static ArrayFilter matchFilterQuery(String input) {
        int last = input.length() - 1;
        for (int pos = 0; pos <= last; pos++) {
            int end = matchSquareBrackets(input, pos, last);
            if (end > 0) {
                String arrayName = rightTrimOf(input, 0, pos);
                String filter = trimOf(input, pos + 1, end);
                pos = eatSpaces(input, end + 1, last);
                FilterMode mode = FILTRATE_FIND_FIRST;
                if (pos <= last) {
                    mode = fromSymbol(input.charAt(pos));
                    if (mode != null) {
                        pos = eatSpaces(input, ++pos, last);
                    }
                    if (pos <= last) {
                        throw new AtPositionException(input, "Invalid filter expression", pos);
                    }
                }
                if (mode != FILTRATE_DIVERT_ALL && filter.isEmpty()) {
                    throw new AtPositionException(input, "Missing filter expression", end);
                }
                return new ArrayFilter(arrayName, filter, mode);
            }
            pos = skipEnclosure(input, pos, last, Enclosure.STRING_LITERAL, Enclosure.PARENTHESES);
        }
        FilterMode mode = fromSymbol(input.charAt(last));
        if (mode == FILTRATE_COLLECT_ALL || mode == FILTRATE_DIVERT_ALL) {
            return new ArrayFilter(rightTrimOf(input, 0, last), null, mode);
        }
        return new ArrayFilter(input, null, FILTRATE_COLLECT_ALL);
    }

    static FuncDispatcher matchFunctionAndArgument(String input) {
        int last = input.length() - 1;
        for (int pos = 0; pos <= last; pos++) {
            int end = matchParentheses(input, pos, last);
            if (end > 0) {
                String ending = rightTrimOf(input, end + 1, last + 1);
                if (!ending.isEmpty()) {
                    throw new AtPositionException(input, "Invalid '" + ending +"'", end);
                }
                String name = rightTrimOf(input, 0, pos);
                if (name.isEmpty()) {
                    throw new IllegalArgumentException("Missing function name: " + input);
                }
                return new FuncDispatcher(name, trimOf(input, pos + 1, end));
            } else if (matchStringLiteral(input, pos, last) > 0 || matchSquareBrackets(input, pos, last) > 0) {
                return null;
            }
        }
        return null;
    }

    static JoinDatasets matchJoinDatasetOperation(String input) {
        int last = input.length() - 1;
        int pos = skipDatasetQuery(input, 0, last);
        String leftQuery = trimOf(input, 0, pos);
        int beg = pos;
        for (;pos <= last; pos++) {
            if ("<=>".indexOf(input.charAt(pos)) < 0) {
                break;
            }
        }
        JoinDatasets.JoinOperator operator = JoinDatasets.JoinOperator.fromSymbol(input.substring(beg, pos));
        if (operator == null) {
            return null;
        }
        int end = skipDatasetQuery(input, pos, last);
        String rightQuery = trimOf(input, pos, last + 1);
        pos = eatSpaces(input, end, last);
        if (pos <= last) {
            throw new AtPositionException(input, "Too many arguments for join operation", pos);
        }
        JoinDatasets.Dataset leftDataset = matchJoinDatasetQuery(leftQuery);
        JoinDatasets.Dataset rightDataset = matchJoinDatasetQuery(rightQuery);
        if (leftDataset.getKeys().length != rightDataset.getKeys().length) {
            throw new IllegalArgumentException("Mismatch key count: " + input);
        }
        return new JoinDatasets(leftDataset, operator, rightDataset);
    }

    private static JoinDatasets.Dataset matchJoinDatasetQuery(String input) {
        int last = input.length() - 1;
        String query = null;
        for (int pos = 0, beg = 0; pos <= last; pos++) {
            switch (input.charAt(pos)) {
                case '{':
                    if (query != null || pos == 0) {
                        throw new AtPositionException(input, "Invalid '{'", pos);
                    }
                    query = rightTrimOf(input, beg, pos);
                    beg = pos + 1;
                    continue;
                case '}':
                    if (query == null) {
                        throw new AtPositionException(input, "Invalid '}'", pos);
                    }
                    String ending = rightTrimOf(input, pos + 1, last + 1);
                    if (!ending.isEmpty()) {
                        throw new AtPositionException(input, "Invalid '" + ending +"'", pos + 1);
                    }
                    String[] keys = trimOf(input, beg, pos).split(",");
                    if (Arrays.stream(keys).anyMatch(StringUtils::isBlank)) {
                        throw new IllegalArgumentException("Missing join key: " + input);
                    }
                    return new JoinDatasets.Dataset(query, keys);
            }
            pos = skipEnclosure(input, pos, last, Enclosure.ALL_KINDS);
        }
        if (query != null) {
            throw new AtPositionException(input, "Missing '}'", -1);
        }
        throw new IllegalArgumentException("Invalid join dataset query: " + input);
    }

    static List<String> decomposePaths(String input) {
        List<String> paths = new ArrayList<>();
        int last = input.length() - 1;
        String isInt = null;
        for (int pos = 0; pos <= last; pos = eatSpaces(input, ++pos, last)) {
            int beg = pos;
            do {
                if (input.charAt(pos) == '.') {
                    break;
                }
                pos = skipEnclosure(input, pos, last, Enclosure.ALL_KINDS);
            } while (++pos <= last);
            String path = trimOf(input, beg, pos);
            if (path.isEmpty()) {
                throw new AtPositionException(input, "Invalid '.'", pos);
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
                        paths.add(isInt + "." + path);
                    } else {
                        paths.add(isInt);
                        paths.add(path);
                    }
                    isInt = null;
                }
            } catch (IllegalArgumentException e) {
                throw new AtPositionException(input, e.getMessage(), beg);
            }
        }
        return paths;
    }

    static List<OperationStep> decomposeStatement(String input) {
        List<OperationStep> steps = new ArrayList<>();
        int last = input.length() - 1;
        int pos = 0;
        while (pos <= last) {
            int beg = pos;
            while ("=!<>&|".indexOf(input.charAt(pos)) >= 0) {
                if (pos++ == last) {
                    throw new AtPositionException(input, "Invalid syntax", -1);
                }
            }
            String token = input.substring(beg, pos);
            Operator operator = Operator.fromSymbol(token);
            if (operator == null) {
                throw new AtPositionException(input, "Invalid operator: " + token, beg);
            }
            beg = eatSpaces(input, pos, last);
            switch (input.charAt(beg)) {
                case '(':
                case ')':
                    pos = beg + 1;
                    break;
                default:
                    pos = skipDatasetQuery(input, beg, last);
            }
            String expression = rightTrimOf(input, beg, pos);
            steps.add(new OperationStep(operator, expression));
            pos = eatSpaces(input, pos, last);
        }
        return steps;
    }

    static List<String> decomposeFunctionParameters(String input, int minCount, int maxCount) {
        List<String> params = new ArrayList<>();
        int last = input.length() - 1;
        for (int pos = 0; pos <= last; pos++) {
            int beg = pos;
            do {
                if (input.charAt(pos) == ',') {
                    break;
                }
                pos = skipEnclosure(input, pos, last, Enclosure.ALL_KINDS);
            } while (++pos <= last);
            String param = trimOf(input, beg, pos);
            if (param.isEmpty()) {
                if (maxCount < 0) {
                    throw new AtPositionException(input, "Argument cannot be empty", beg);
                }
                if (params.size() < minCount) {
                    break;
                }
            } else if (params.size() == maxCount) {
                throw new IllegalArgumentException("Exceeded maximum of " + maxCount + " arguments: " + input);
            }
            params.add(param);
        }
        if (minCount > 0 && params.size() < minCount) {
            throw new IllegalArgumentException(
                    "Expected" + ((minCount == maxCount) ? "" : " at least ") + minCount + " arguments: " + input);
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

    static List<TernaryStep> decomposeTernarySteps(String input) {
        List<TernaryStep> steps = new ArrayList<>();
        int len = input.length();
        int last = len - 1;
        for (int pos = 0, beg = 0, endIf = -1; pos <= len; pos++) {
            if (pos == len || input.charAt(pos) == ':') {
                String condition;
                String value = null;
                if (endIf < 0) {
                    condition = trimOf(input, beg, pos);
                } else {
                    condition = trimOf(input, beg, endIf);
                    value = trimOf(input, endIf + 1, pos);
                    endIf = -1;
                }
                if (condition.isEmpty()) {
                    throw new AtPositionException(input, "Missing argument", beg);
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

    static List<String> separateXmlTags(String input) {
        List<String> tokens = new ArrayList<>();
        int len = input.length();
        int text = 0;
        int tag = -1;
        for (int pos = 0; pos < len; pos++) {
            switch (input.charAt(pos)) {
                case '<':
                    if (text >= 0) {
                        if (text < pos) {
                            tokens.add(input.substring(text, pos));
                        }
                        text = -1;
                        tag = pos;
                    }
                    break;
                case '>':
                    if (tag >= 0) {
                        pos++;
                        if (pos == len || input.charAt(pos) != '<') {
                            tokens.add(input.substring(tag, pos));
                            tag = -1;
                            text = pos;
                        }
                    }
                    break;
            }
        }
        if (text >= 0 && text < len) {
            tokens.add(input.substring(text, len));
        }
        return tokens;
    }

    static String getLastElementName(String path) {
        List<String> paths = decomposePaths(path);
        for (int i = paths.size() - 1; i >= 0 ; i--) {
            if (matchFunctionAndArgument(paths.get(i)) == null) {
                return matchFilterQuery(paths.get(i)).getNodeName();
            }
        }
        return "undefined";
    }

    static void checkElementName(String name) {
        if (name.contains(".")) {
            throw new IllegalArgumentException("Illegal '.' in element name '" + name + "'");
        }
    }
}

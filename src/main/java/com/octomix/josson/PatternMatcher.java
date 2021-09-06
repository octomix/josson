package com.octomix.josson;

import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;

class PatternMatcher {

    private static final int STRING_LITERAL_ELEMENT = 0x01;
    private static final int SQUARE_BRACKETS_ELEMENT = 0x02;
    private static final int PARENTHESES_ELEMENT = 0x04;
    private static final int ALL_SYNTAX_ELEMENTS = STRING_LITERAL_ELEMENT | SQUARE_BRACKETS_ELEMENT | PARENTHESES_ELEMENT;

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

    private static int skipSyntaxElement(String input, int pos, int last, int elements) {
        int end;
        if ((elements & STRING_LITERAL_ELEMENT) != 0 && (end = matchStringLiteral(input, pos, last)) > 0) {
            return end;
        }
        if ((elements & SQUARE_BRACKETS_ELEMENT) != 0 && (end = matchSquareBrackets(input, pos, last)) > 0) {
            return end;
        }
        if ((elements & PARENTHESES_ELEMENT) != 0 && (end = matchParentheses(input, pos, last)) > 0) {
            return end;
        }
        return pos;
    }

    private static int matchStringLiteral(String input, int pos, int last) {
        if (input.charAt(pos) != '\'') {
            return 0;
        }
        while (++pos < last) {
            if (input.charAt(pos) == '\'') {
                if (input.charAt(pos + 1) != '\'') {
                    return pos;
                }
                pos++;
            }
        }
        if (pos == last && input.charAt(pos) == '\'') {
            return pos;
        }
        throw new IllegalArgumentException("Expected \"'\" at position " + pos + ": " + input);
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
                        throw new IllegalArgumentException("Invalid ')' at position " + pos + ": " + input);
                    }
                    return pos;
                case ']':
                    if (expectedEnd == ')') {
                        throw new IllegalArgumentException("Invalid ']' at position " + pos + ": " + input);
                    }
                    return pos;
                case '(':
                    pos = findBracketEnd(input, pos, last, ')');
                    continue;
                case '[':
                    pos = findBracketEnd(input, pos, last, ']');
                    continue;
            }
            pos = skipSyntaxElement(input, pos, last, STRING_LITERAL_ELEMENT);
        }
        throw new IllegalArgumentException("Missing '" + expectedEnd + "' at the end: " + input);
    }

    static String[] matchJsonQuery(String input) {
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

    static String[] matchFilterQuery(String input) {
        int last = input.length() - 1;
        for (int pos = 0; pos <= last; pos++) {
            int end = matchSquareBrackets(input, pos, last);
            if (end > 0) {
                String[] tokens = new String[3];
                tokens[0] = rightTrimOf(input, 0, pos);
                if (tokens[0].startsWith("@")) {
                    throw new IllegalArgumentException("Invalid filter syntax: " + input);
                }
                tokens[1] = trimOf(input, pos + 1, end);
                if (tokens[1].isEmpty()) {
                    throw new IllegalArgumentException("Missing filter expression: " + input);
                }
                pos = eatSpaces(input, end + 1, last);
                if (pos <= last) {
                    char filterIndicator = input.charAt(pos);
                    if (filterIndicator == '*' || filterIndicator == '@') {
                        pos = eatSpaces(input, ++pos, last);
                    }
                    if (pos <= last) {
                        throw new IllegalArgumentException("Invalid filter syntax at position " + pos + ": " + input);
                    }
                    tokens[2] = String.valueOf(filterIndicator);
                }
                return tokens;
            }
            pos = skipSyntaxElement(input, pos, last, STRING_LITERAL_ELEMENT | PARENTHESES_ELEMENT);
        }
        char filterIndicator = input.charAt(last);
        if (filterIndicator == '*' || filterIndicator == '@') {
            return new String[]{rightTrimOf(input, 0, last), null, String.valueOf(filterIndicator)};
        }
        return new String[]{input, null, "*"};
    }

    static String[] matchFunctionAndArgument(String input) {
        int last = input.length() - 1;
        int beg = last >= 0 && input.charAt(0) == '@' ? eatSpaces(input, 1, last) : 0;
        for (int pos = 0; pos <= last; pos++) {
            int end = matchParentheses(input, pos, last);
            if (end > 0) {
                String ending = rightTrimOf(input, end + 1, last + 1);
                if (!ending.isEmpty()) {
                    throw new IllegalArgumentException("Invalid '" + ending + "' at position " + end + ": " + input);
                }
                String[] tokens = new String[3];
                tokens[0] = rightTrimOf(input, beg, pos);
                if (tokens[0].isEmpty()) {
                    throw new IllegalArgumentException("Missing function name: " + input);
                }
                tokens[1] = trimOf(input, pos + 1, end);
                tokens[2] = beg > 0 ? "@" : null;
                return tokens;
            } else if (matchStringLiteral(input, pos, last) > 0 || matchSquareBrackets(input, pos, last) > 0) {
                return null;
            }
        }
        if (beg > 0) {
            throw new IllegalArgumentException("Invalid syntax: " + input);
        }
        return null;
    }

    static String[] matchJoinOperation(String input) {
        int last = input.length() - 1;
        String[] tokens = null;
        for (int pos = 0, beg = 0; pos <= last; pos++) {
            switch (input.charAt(pos)) {
                case '{':
                    if (tokens != null || pos == 0) {
                        throw new IllegalArgumentException("Invalid '{' at position " + pos + ": " + input);
                    }
                    tokens = new String[2];
                    tokens[0] = rightTrimOf(input, beg, pos);
                    beg = pos + 1;
                    continue;
                case '}':
                    if (tokens == null) {
                        throw new IllegalArgumentException("Invalid '}' at position " + pos + ": " + input);
                    }
                    tokens[1] = trimOf(input, beg, pos);
                    String ending = rightTrimOf(input, ++pos, last + 1);
                    if (!ending.isEmpty()) {
                        throw new IllegalArgumentException("Invalid '" + ending + "' at position " + pos + ": " + input);
                    }
                    return tokens;
            }
            pos = skipSyntaxElement(input, pos, last, ALL_SYNTAX_ELEMENTS);
        }
        if (tokens != null) {
            throw new IllegalArgumentException("Missing '}' at the end: " + input);
        }
        return null;
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
                pos = skipSyntaxElement(input, pos, last, ALL_SYNTAX_ELEMENTS);
            } while (++pos <= last);
            String path = trimOf(input, beg, pos);
            if (path.isEmpty()) {
                throw new IllegalArgumentException("Invalid '.' at position " + pos + ": " + input);
            }
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
        }
        return paths;
    }

    static List<String[]> decomposeConditions(String input) {
        List<String[]> conditions = new ArrayList<>();
        int last = input.length() - 1;
        int pos = 0;
        while (pos <= last) {
            String[] condition = new String[2];
            int beg = pos;
            while ("=!<>&|".indexOf(input.charAt(pos)) >= 0) {
                if (pos++ == last) {
                    throw new IllegalArgumentException("Invalid syntax at the end: " + input);
                }
            }
            condition[0] = input.substring(beg, pos);
            beg = eatSpaces(input, pos, last);
            switch (input.charAt(beg)) {
                case '(':
                case ')':
                    pos = beg + 1;
                    break;
                default:
                    for (pos = beg; pos <= last && "=!<>&|".indexOf(input.charAt(pos)) < 0; pos++) {
                        if (input.charAt(pos) == '-') {
                            // skip "->"
                            if (pos++ == last) {
                                break;
                            }
                        }
                        pos = skipSyntaxElement(input, pos, last, ALL_SYNTAX_ELEMENTS);
                    }
            }
            condition[1] = rightTrimOf(input, beg, pos);
            conditions.add(condition);
            pos = eatSpaces(input, pos, last);
        }
        return conditions;
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
                pos = skipSyntaxElement(input, pos, last, ALL_SYNTAX_ELEMENTS);
            } while (++pos <= last);
            String param = trimOf(input, beg, pos);
            if (param.isEmpty()) {
                if (maxCount < 0) {
                    throw new IllegalArgumentException("Argument cannot be empty at position " + beg + ": " + input);
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
                    "Expected" + ((minCount == maxCount) ? "" : " at least") + minCount + " arguments: " + input);
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

    static List<String[]> decomposeTernarySteps(String input) {
        List<String[]> steps = new ArrayList<>();
        int len = input.length();
        int last = len - 1;
        for (int pos = 0, beg = 0, endIf = -1; pos <= len; pos++) {
            if (pos == len || input.charAt(pos) == ':') {
                String[] step = new String[2];
                if (endIf < 0) {
                    step[0] = trimOf(input, beg, pos);
                } else {
                    step[0] = trimOf(input, beg, endIf);
                    step[1] = trimOf(input, endIf + 1, pos);
                    endIf = -1;
                }
                if (step[0].isEmpty()) {
                    throw new IllegalArgumentException("Missing argument at position " + beg + ": " + input);
                }
                steps.add(step);
                beg = pos + 1;
            } else if (input.charAt(pos) == '?') {
                endIf = pos;
            } else {
                pos = skipSyntaxElement(input, pos, last, ALL_SYNTAX_ELEMENTS);
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
}

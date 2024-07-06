/*
 * Copyright 2020-2024 Octomix Software Technology Limited
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
import java.util.function.Function;

import static com.octomix.josson.ArrayFilter.FilterMode;
import static com.octomix.josson.ArrayFilter.FilterMode.*;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Utils.*;
import static com.octomix.josson.commons.StringUtils.EMPTY;
import static com.octomix.josson.exception.SyntaxErrorException.POS_AT_THE_END;

/**
 * A syntax analyser.
 */
final class SyntaxDecomposer extends SyntaxMatcher {

    SyntaxDecomposer(final String input) {
        super(input);
    }

    String[] deDatasetQuery() {
        int pos = eatSpaces(0);
        for (int beg = pos; pos < last; pos++) {
            switch (input.charAt(pos)) {
                case '[':
                case '<':
                    return null;
                case '(':
                    pos = skipEnclosure(pos, Enclosure.PARENTHESES);
                    break;
                case '-':
                    return input.charAt(pos + 1) != '>' ? null
                            : new String[]{rightTrimOf(beg, pos), trimOf(pos + 2, length)};
            }
        }
        return null;
    }

    String[] deDbQuery() {
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
                    name = trimOf(0, pos);
                }
                break;
            }
            final int end = matchSquareBrackets(pos);
            if (end > 0) {
                if (arrayEnd > 0 || eatSpaces(input, pos + 1, end) != end) {
                    return null;
                }
                arrayEnd = end;
                name = trimOf(0, pos);
            } else if (matchStringLiteral(pos) > 0 || matchParentheses(pos) > 0) {
                return null;
            }
        }
        if (pos >= last) {
            return null;
        }
        final String query = trimOf(pos + 1, length);
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

    ArrayFilter deFilterQuery() {
        if (length == 0) {
            return new ArrayFilter(null, null, FILTRATE_COLLECT_ALL);
        }
        for (int pos = 0; pos <= last; pos++) {
            final int end = matchSquareBrackets(pos);
            if (end > 0) {
                final String arrayName = rightTrimOf(0, pos);
                final String filter = trimOf(pos + 1, end);
                pos = eatSpaces(end + 1);
                FilterMode mode = FILTRATE_FIND_FIRST;
                if (pos <= last) {
                    mode = fromSymbol(input.charAt(pos));
                    if (mode != null) {
                        pos = eatSpaces(++pos);
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
            pos = skipEnclosure(pos, Enclosure.STRING_LITERAL, Enclosure.PARENTHESES);
        }
        final FilterMode mode = fromSymbol(input.charAt(last));
        if (mode == FILTRATE_COLLECT_ALL || mode == FILTRATE_DIVERT_ALL) {
            return new ArrayFilter(rightTrimOf(0, last), null, mode);
        }
        return new ArrayFilter(input, null, FILTRATE_COLLECT_ALL);
    }

    String[] deWildcardLevelsAndFilter() {
        int beg = eatSpaces(1);
        if (beg > last) {
            return null;
        }
        final int pos = matchParentheses(beg);
        if (pos > 0) {
            final String levels = trimOf(beg + 1, pos);
            beg = eatSpaces(pos + 1);
            if (beg > last) {
                return new String[]{levels, null};
            }
        } else {
            final char ch = input.charAt(beg);
            if ((beg < last && ch == '[')
                    || (beg == last && (FILTRATE_COLLECT_ALL.equals(ch) || FILTRATE_DIVERT_ALL.equals(ch)))) {
                return new String[]{null, input.substring(beg)};
            }
        }
        throw new SyntaxErrorException(input, "Invalid wildcard syntax");
    }

    String[] deFunctionAndArgument(final boolean allowDivert) {
        for (int pos = 0; pos <= last; pos++) {
            final int end = matchParentheses(pos);
            if (end > 0) {
                final String ending = trimOf(end + 1, length);
                if (!ending.isEmpty()
                        && (!allowDivert || ending.length() > 1 || !FILTRATE_DIVERT_ALL.equals(ending.charAt(0)))) {
                    throw new SyntaxErrorException(input, String.format("Invalid '%s' after function", ending), end);
                }
                final String name = rightTrimOf(0, pos);
                if (name.isEmpty()) {
                    throw new SyntaxErrorException(input, "Missing function name");
                }
                return new String[]{name, trimOf(pos + 1, end)};
            } else if (matchStringLiteral(pos) > 0 || matchSquareBrackets(pos) > 0) {
                return null;
            }
        }
        return null;
    }

    List<CombineOperation> deCombineOperations() {
        final int pos = skipDatasetQuery(0);
        if (pos >= last || "<>".indexOf(input.charAt(pos)) < 0) {
            return null;
        }
        final int end = CombineOperator.findEndingPos(input, pos, last);
        final CombineOperator operator = CombineOperator.fromSymbol(input.substring(pos, end));
        if (operator == null) {
            return null;
        }
        final CombineOperand leftOperand = matchCombineOperand(0, pos, true);
        final List<CombineOperation> operations = new ArrayList<>();
        matchCombineOperation(end, leftOperand, operator, operations);
        return operations;
    }

    List<String> dePathSteps() {
        final List<String> steps = new ArrayList<>();
        int ups = -1;
        String isInt = null;
        for (int pos = 0; pos <= last; pos = eatSpaces(++pos)) {
            final int beg = pos;
            do {
                if (input.charAt(pos) == PATH_DELIMITER) {
                    break;
                }
                pos = eatSpaces(skipEnclosure(pos, Enclosure.ALL_KINDS) + 1);
            } while (pos <= last);
            final String step = trimOf(beg, pos);
            if (step.isEmpty()) {
                if (steps.isEmpty()) {
                    ups++;
                    continue;
                }
                throw new SyntaxErrorException(input, "Invalid '.'", pos);
            }
            if (step.startsWith(CURRENT_NODE)) {
                if (step.length() > 1 || !steps.isEmpty()) {
                    throw new SyntaxErrorException(step);
                }
                continue;
            }
            try {
                if (isInt == null) {
                    if (pos > last || parseInteger(step) == null) {
                        final String name = unescapePathName(step);
                        if (name != null) {
                            steps.add(name);
                        }
                    } else {
                        isInt = step;
                    }
                } else {
                    if (StringUtils.isNumeric(step)) {
                        steps.add(String.format("%s.%s", isInt, step));
                    } else {
                        steps.add(isInt);
                        steps.add(step);
                    }
                    isInt = null;
                }
            } catch (IllegalArgumentException e) {
                throw new SyntaxErrorException(input, e.getMessage(), beg);
            }
        }
        if (ups > 0) {
            steps.add(0, StringUtils.repeat(PATH_DELIMITER, ups));
        }
        return steps;
    }

    List<OperationStep> deOperationSteps() {
        final List<OperationStep> steps = new ArrayList<>();
        int pos = 0;
        while (pos <= last) {
            int beg = pos;
            while (true) {
                final char ch = input.charAt(pos);
                if ("=!<>&|".indexOf(ch) < 0 && !(ch == '~' && pos > beg)) {
                    break;
                }
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
            beg = eatSpaces(pos);
            switch (input.charAt(beg)) {
                case '(':
                case ')':
                    pos = beg + 1;
                    break;
                default:
                    pos = skipDatasetQuery(beg);
                    break;
            }
            final String expression = rightTrimOf(beg, pos);
            steps.add(new OperationStep(operator, expression));
            pos = eatSpaces(pos);
        }
        return steps;
    }

    List<String> deFunctionParameters(final int min, final int max) {
        final List<String> params = new ArrayList<>();
        for (int pos = 0; pos <= last; pos++) {
            final int beg = pos;
            do {
                if (input.charAt(pos) == ',') {
                    break;
                }
                pos = skipEnclosure(pos, Enclosure.ALL_KINDS);
            } while (++pos <= last);
            final String param = trimOf(beg, pos);
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

    List<TernaryStep> deTernarySteps() {
        final List<TernaryStep> steps = new ArrayList<>();
        for (int pos = 0, beg = 0, endIf = -1; pos <= length; pos++) {
            if (pos == length || input.charAt(pos) == ':') {
                final String condition;
                String value = null;
                if (endIf < 0) {
                    condition = trimOf(beg, pos);
                } else {
                    condition = trimOf(beg, endIf);
                    value = trimOf(endIf + 1, pos);
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
                pos = skipEnclosure(pos, Enclosure.ALL_KINDS);
            }
        }
        return steps;
    }

    String[] deNameAndPath(final Function<String, String> getUnknownName) {
        String name = null;
        String path = input;
        for (int pos = 0; pos < length; pos++) {
            if (input.charAt(pos) == ':') {
                name = trimOf(0, pos);
                if (name.isEmpty()) {
                    throw new SyntaxErrorException(input, "Missing field name");
                }
                name = unescapePathName(name);
                path = trimOf(pos + 1, length);
                if (path.startsWith(EVALUATE_KEY_NAME)) {
                    name = EVALUATE_KEY_NAME + name;
                    path = path.substring(eatSpaces(path, EVALUATE_KEY_NAME.length(), path.length() - 1));
                    if (path.isEmpty()) {
                        throw new SyntaxErrorException(input, "Missing field value");
                    }
                }
                break;
            }
            pos = skipEnclosure(pos, Enclosure.ALL_KINDS);
        }
        final boolean unresolvableAsNull = path.startsWith(UNRESOLVABLE_AS_NULL);
        if (unresolvableAsNull) {
            path = path.substring(eatSpaces(path, UNRESOLVABLE_AS_NULL.length(), path.length() - 1));
        }
        if (name == null) {
            try {
                name = getLastElementName(input);
            } catch (UnknownFormatConversionException e) {
                name = getUnknownName == null ? e.getConversion() : getUnknownName.apply(e.getConversion());
            }
        } else if (unresolvableAsNull && path.isEmpty()) {
            if (name.startsWith(EVALUATE_KEY_NAME)) {
                throw new SyntaxErrorException(input, "Missing field value");
            }
            path = name;
        }
        return new String[]{name, path.isEmpty() ? null : path, unresolvableAsNull ? UNRESOLVABLE_AS_NULL : null};
    }
}

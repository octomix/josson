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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;
import com.octomix.josson.commons.StringUtils;
import com.octomix.josson.exception.SyntaxErrorException;

import java.time.ZoneId;
import java.util.*;

import static com.octomix.josson.ArrayFilter.FilterMode;
import static com.octomix.josson.ArrayFilter.FilterMode.*;
import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.PatternMatcher.*;
import static com.octomix.josson.Utils.*;
import static com.octomix.josson.commons.StringUtils.EMPTY;

/**
 * Static core functions for Josson.
 */
final class JossonCore {

    static final TextNode EMPTY_STRING_NODE = new TextNode(EMPTY);

    static final int NON_ARRAY_INDEX = -1;

    static final char PATH_DELIMITER = '.';

    static final char VARIABLE_PREFIX_SYMBOL = '$';

    static final char QUOTE_SYMBOL = '\'';

    static final String ENTRY_KEY_NAME = "key";

    static final String ENTRY_VALUE_NAME = "value";

    static final String GROUP_VALUE_NAME = "elements";

    static final String CURRENT_NODE = "?";

    static final String VAR_ARGS = "...";

    private static final char WILDCARD_SYMBOL = '*';

    private static final char MATCHES_SYMBOL = '~';

    private static final char COLLECT_BRANCHES_SYMBOL = '@';

    private static final char INDEX_PREFIX_SYMBOL = '#';

    private static final String ROOT_NODE = "$";

    private static final String PARENT_ARRAY_NODE = "@";

    private static final String ZERO_BASED_INDEX = INDEX_PREFIX_SYMBOL + "";

    private static final String ONE_BASED_INDEX = INDEX_PREFIX_SYMBOL + "#";

    private static final String UPPERCASE_INDEX = INDEX_PREFIX_SYMBOL + "A";

    private static final String LOWERCASE_INDEX = INDEX_PREFIX_SYMBOL + "a";

    private static final String UPPER_ROMAN_INDEX = INDEX_PREFIX_SYMBOL + "R";

    private static final String LOWER_ROMAN_INDEX = INDEX_PREFIX_SYMBOL + "r";

    private static final String NEGATIVE_SIGN = "-";

    private static Locale locale = Locale.getDefault();

    private static ZoneId zoneId = ZoneId.systemDefault();

    private JossonCore() {
    }

    static void setLocale(final Locale locale) {
        if (locale != null) {
            JossonCore.locale = locale;
        }
    }

    static Locale getLocale() {
        return JossonCore.locale;
    }

    static void setZoneId(final ZoneId zoneId) {
        if (zoneId != null) {
            JossonCore.zoneId = zoneId;
        }
    }

    static ZoneId getZoneId() {
        return zoneId;
    }

    static String getNodeAsText(final PathTrace path, final String expression) {
        return getNodeAsText(path, NON_ARRAY_INDEX, expression);
    }

    static String getNodeAsText(final PathTrace path, final int index, final String expression) {
        final PathTrace result = getPathByExpression(path, index, expression);
        return result == null ? EMPTY : result.node().asText();
    }

    static String getNodeAsTextExceptNull(final PathTrace path, final int index, final String expression) {
        final PathTrace result = getPathByExpression(path, index, expression);
        return nodeIsNull(result) ? null : result.node().asText();
    }

    static boolean getNodeAsBoolean(final PathTrace path, final int index, final String expression) {
        final PathTrace result = getPathByExpression(path, index, expression);
        return result != null && result.node().asBoolean();
    }

    static int getNodeAsInt(final PathTrace path, final String expression) {
        return getNodeAsInt(path, NON_ARRAY_INDEX, expression);
    }

    static int getNodeAsInt(final PathTrace path, final int index, final String expression) {
        final PathTrace result = getPathByExpression(path, index, expression);
        return result == null || !result.node().isValueNode() ? 0 : result.node().asInt();
    }

    static JsonNode getNodeByExpression(final JsonNode node, final String expression) {
        return getNodeByExpression(node, NON_ARRAY_INDEX, expression);
    }

    static JsonNode getNodeByExpression(final JsonNode node, final int index, final String expression) {
        PathTrace path = getPathByExpression(PathTrace.from(node), index, expression);
        return path == null ? null : path.node();
    }

    static JsonNode getNodeByExpression(final PathTrace path, final String expression) {
        PathTrace result = getPathByExpression(path, expression);
        return result == null ? null : result.node();
    }

    static JsonNode getNodeByExpression(final PathTrace path, final int index, final String expression) {
        PathTrace result = getPathByExpression(path, index, expression);
        return result == null ? null : result.node();
    }

    static PathTrace getPathByExpression(final PathTrace path, final String expression) {
        return getPathByExpression(path, NON_ARRAY_INDEX, expression);
    }

    static PathTrace getPathByExpression(final PathTrace path, final int index, final String expression) {
        if (path == null) {
            return null;
        }
        final List<String> steps = decomposePath(expression);
        if (steps.isEmpty()) {
            return index >= 0 && path.node().isArray() ? path.push(path.node().get(index)) : path;
        }
        final String step = steps.get(0);
        switch (step) {
            case ROOT_NODE:
                steps.remove(0);
                return getPathBySteps(path.root(), steps);
            case PARENT_ARRAY_NODE:
                steps.remove(0);
                return getPathBySteps(path, steps);
        }
        switch (step.charAt(0)) {
            case PATH_DELIMITER:
                steps.remove(0);
                return getPathBySteps(path.pop(step.length()), steps);
            case VARIABLE_PREFIX_SYMBOL:
                steps.remove(0);
                JsonNode value = path.getVariable(step);
                return getPathBySteps(PathTrace.from(value == null ? NullNode.getInstance() : value), steps);
            case INDEX_PREFIX_SYMBOL:
                steps.remove(0);
                switch (step) {
                    case ZERO_BASED_INDEX:
                        return getPathBySteps(PathTrace.from(IntNode.valueOf(index)), steps);
                    case ONE_BASED_INDEX:
                        return getPathBySteps(PathTrace.from(IntNode.valueOf(index + 1)), steps);
                    case UPPERCASE_INDEX:
                        return getPathBySteps(PathTrace.from(TextNode.valueOf(toAlphabetIndex(index, 'A'))), steps);
                    case LOWERCASE_INDEX:
                        return getPathBySteps(PathTrace.from(TextNode.valueOf(toAlphabetIndex(index, 'a'))), steps);
                    case UPPER_ROMAN_INDEX:
                        return getPathBySteps(PathTrace.from(TextNode.valueOf(toRomanIndex(index, true))), steps);
                    case LOWER_ROMAN_INDEX:
                        return getPathBySteps(PathTrace.from(TextNode.valueOf(toRomanIndex(index, false))), steps);
                }
                throw new IllegalArgumentException("Invalid index type: " + step);
        }
        if (index >= 0 && path.node().isArray()) {
            return getPathBySteps(path.push(path.node().get(index)), steps);
        }
        return getPathBySteps(path, steps);
    }

    private static String toAlphabetIndex(final int number, final int base) {
        if (number < 0) {
            return NEGATIVE_SIGN + toAlphabetIndex(-number - 1, base);
        }
        final int quot = number / 26;
        return (quot == 0 ? EMPTY : toAlphabetIndex(quot - 1, base)) + (char) (base + number % 26);
    }

    private static String toRomanIndex(int number, final boolean isUpper) {
        if (number < 0) {
            return NEGATIVE_SIGN + toRomanIndex(-number - 1, isUpper);
        }
        number++;
        final int[] numbers = {1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1};
        final String[] uppers = {"M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"};
        final String[] lowers = {"m", "cm", "d", "cd", "c", "xc", "l", "xl", "x", "ix", "v", "iv", "i"};
        final StringBuilder result = new StringBuilder();
        for (int i = 0; i < numbers.length && number > 0; i++) {
            while (number >= numbers[i]) {
                number -= numbers[i];
                result.append(isUpper ? uppers[i] : lowers[i]);
            }
        }
        return result.toString();
    }

    private static PathTrace getPathBySteps(PathTrace path, List<String> steps) {
        if (path != null && !steps.isEmpty()) {
            try {
                path = path.push(literalToValueNode(steps.get(0)));
                steps.remove(0);
            } catch (NumberFormatException ignore) {
            }
            while (path != null && path.node() != null && !steps.isEmpty()) {
                final List<String> nextSteps = new ArrayList<>();
                path = getPathBySteps(path, steps, nextSteps);
                steps = nextSteps;
            }
        }
        if (path != null && path.node() == null) {
            return null;
        }
        return path;
    }

    private static PathTrace getPathBySteps(PathTrace path, final List<String> steps, final List<String> nextSteps) {
        if (steps == null || steps.isEmpty() || path == null) {
            return path;
        }
        String step = steps.get(0);
        switch (step.charAt(0)) {
            case WILDCARD_SYMBOL:
                if (path.node().isEmpty())  {
                    return null;
                }
                final String[] levelsAndFilter = matchWildcardLevelsAndFilter(step);
                if (levelsAndFilter == null) {
                    steps.remove(0);
                    if (path.node().isObject()) {
                        return wildcardAny(path, steps, nextSteps);
                    }
                    return wildcardAny(wildcardArrayNodeToList(path), steps, nextSteps, 1);
                }
                if (levelsAndFilter[0] != null) {
                    steps.remove(0);
                    final int levels = levelsAndFilter[0].isEmpty() ? 0 : getNodeAsInt(path, levelsAndFilter[0]);
                    return wildcardAny(
                            path.node().isObject() ? Collections.singletonList(path) : wildcardArrayNodeToList(path),
                            steps, nextSteps, levels);
                }
                final String filter = levelsAndFilter[1];
                if (filter.length() == 1) {
                    if (FILTRATE_COLLECT_ALL.equals(filter.charAt(0))) {
                        step = "toarray()";
                    } else {
                        step = "toarray()@";
                    }
                } else {
                    step = "entries()";
                    steps.add(1, filter);
                    steps.add(2, ENTRY_VALUE_NAME);
                }
                break;
            case MATCHES_SYMBOL:
                final char lastChar = step.charAt(step.length() - 1);
                final String filterMode = FILTRATE_COLLECT_ALL.equals(lastChar) || FILTRATE_DIVERT_ALL.equals(lastChar)
                        ? String.valueOf(lastChar) : EMPTY;
                final String strLiteral = StringUtils.strip(step.substring(1, step.length() - filterMode.length()));
                if (strLiteral.charAt(0) != QUOTE_SYMBOL || strLiteral.charAt(strLiteral.length() - 1) != QUOTE_SYMBOL) {
                    throw new SyntaxErrorException(step);
                }
                step = "entries()";
                steps.add(1, "[" + ENTRY_KEY_NAME + ".matches(" + strLiteral + ")]" + filterMode);
                steps.add(2, ENTRY_VALUE_NAME);
                break;
            case COLLECT_BRANCHES_SYMBOL:
                step = StringUtils.strip(step.substring(1));
                nextSteps.addAll(steps);
                if (step.isEmpty()) {
                    nextSteps.remove(0);
                } else {
                    nextSteps.set(0, step);
                }
                steps.clear();
                return path;
            case INDEX_PREFIX_SYMBOL:
                throw new SyntaxErrorException(step);
        }
        steps.remove(0);
        final String[] funcAndArgs = matchFunctionAndArgument(step, true);
        if (funcAndArgs != null) {
            if (FILTRATE_DIVERT_ALL.equals(step.charAt(step.length() - 1))) {
                steps.add(0, "[]" + FILTRATE_DIVERT_ALL.getSymbol());
            }
            return getPathBySteps(new FuncDispatcher(funcAndArgs[0], funcAndArgs[1]).apply(path), steps, nextSteps);
        }
        final ArrayFilter filter = matchFilterQuery(step);
        final JsonNode node;
        if (filter.getFilter() == null && filter.getMode() != FILTRATE_DIVERT_ALL) {
            if (path.node().isValueNode()) {
                return null;
            }
            if (path.node().isArray()) {
                return forEachElement(path, filter.getNodeName(), filter.getMode(), steps, nextSteps);
            }
            node = path.node().get(filter.getNodeName());
        } else {
            if (!filter.getNodeName().isEmpty()) {
                path = getPathByExpression(path, filter.getNodeName());
            }
            node = filter.evaluateFilter(path, filter.getFilter());
        }
        if (node == null) {
            return null;
        }
        if (node.isArray()) {
            return forEachElement(path.push(node), null, filter.getMode(), steps, nextSteps);
        }
        return getPathBySteps(path.push(node), steps, nextSteps);
    }

    private static List<PathTrace> wildcardArrayNodeToList(final PathTrace path) {
        final List<PathTrace> array = new ArrayList<>();
        for (JsonNode elem : path.node()) {
            if (elem.isObject()) {
                array.add(path.push(elem));
            }
        }
        return array;
    }

    private static PathTrace wildcardAny(final PathTrace path, final List<String> steps, final List<String> nextSteps) {
        for (JsonNode elem : path.node()) {
            final PathTrace result = getPathBySteps(path.push(elem), new ArrayList<>(steps), new ArrayList<>(nextSteps));
            if (!nodeIsNull(result)) {
                return result;
            }
        }
        return null;
    }

    private static PathTrace wildcardAny(final List<PathTrace> paths, final List<String> steps,
                                         final List<String> nextSteps, final int levels) {
        for (PathTrace path : paths) {
            final PathTrace matched = wildcardAny(path, steps, nextSteps);
            if (matched != null) {
                return matched;
            }
        }
        if (levels == 1) {
            return null;
        }
        final List<PathTrace> nextLevel = new ArrayList<>();
        for (PathTrace path : paths) {
            for (Iterator<JsonNode> it = path.node().elements(); it.hasNext(); ) {
                JsonNode elem = it.next();
                if (elem.isObject()) {
                    nextLevel.add(path.push(elem));
                }
            }
        }
        return wildcardAny(nextLevel, steps, nextSteps, levels - 1);
    }

    private static PathTrace forEachElement(final PathTrace path, final String elem, final FilterMode mode,
                                            final List<String> steps, final List<String> nextSteps) {
        final ArrayNode array = MAPPER.createArrayNode();
        if (mode != FILTRATE_DIVERT_ALL) {
            for (JsonNode each : path.node()) {
                final JsonNode addNode = elem == null ? each : each.get(elem);
                if (addNode != null) {
                    if (mode == FILTRATE_COLLECT_ALL && addNode.isArray()) {
                        array.addAll((ArrayNode) addNode);
                    } else {
                        array.add(addNode);
                    }
                }
            }
        } else if (!path.node().isEmpty()) {
            List<String> nextNextKeys = null;
            for (JsonNode each : path.node()) {
                final List<String> tempKeys = new ArrayList<>();
                addArrayElement(array, getPathBySteps(path.push(elem == null ? each : each.get(elem)), new ArrayList<>(steps), tempKeys));
                if (!tempKeys.isEmpty()) {
                    nextNextKeys = tempKeys;
                }
            }
            if (nextSteps.isEmpty() && nextNextKeys != null) {
                nextSteps.addAll(nextNextKeys);
            }
            steps.clear();
            steps.addAll(nextSteps);
            nextSteps.clear();
        }
        return getPathBySteps(path.push(array), steps, nextSteps);
    }
}

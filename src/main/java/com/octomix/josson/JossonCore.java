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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;
import com.octomix.josson.commons.StringUtils;
import com.octomix.josson.exception.SyntaxErrorException;
import org.mariuszgromada.math.mxparser.License;
import org.mariuszgromada.math.mxparser.mXparser;

import java.time.ZoneId;
import java.util.*;
import java.util.function.Consumer;

import static com.octomix.josson.ArrayFilter.FilterMode;
import static com.octomix.josson.ArrayFilter.FilterMode.*;
import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.Utils.*;
import static com.octomix.josson.commons.StringUtils.EMPTY;

/**
 * Static core functions for Josson.
 */
final class JossonCore {

    static final TextNode EMPTY_STRING_NODE = new TextNode(EMPTY);

    static final int NON_ARRAY_INDEX = -1;

    static final char PATH_DELIMITER = '.';

    static final char QUOTE_SYMBOL = '\'';

    static final String ENTRY_KEY_NAME = "key";

    static final String ENTRY_VALUE_NAME = "value";

    static final String GROUP_VALUE_NAME = "elements";

    static final String CURRENT_NODE = "?";

    static final String EVALUATE_KEY_NAME = ":";

    static final String UNRESOLVABLE_AS_NULL = "+";

    static final String VAR_ARGS = "??";

    private static final char WILDCARD_SYMBOL = '*';

    private static final char MATCHES_SYMBOL = '~';

    private static final char COLLECT_BRANCHES_SYMBOL = '@';

    private static final char INDEX_PREFIX_SYMBOL = '#';

    private static final char VARIABLE_PREFIX_SYMBOL = '$';

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

    static int minArraySizeToUseMultiThread = 100;

    static int threadPoolSize = 4;

    static boolean retainArrayOrder = true;

    static final String WILDCARD_COLLECT_ALL = String.valueOf(WILDCARD_SYMBOL) + FILTRATE_COLLECT_ALL.getSymbol();

    static {
        License.iConfirmNonCommercialUse("Josson");
        mXparser.disableImpliedMultiplicationMode();
    }

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

    static String getCustomFunctionName(final String name) {
        Objects.requireNonNull(name, "Function name must not be null");
        final String[] funcAndArgs = new SyntaxDecomposer(name).deFunctionAndArgument(false);
        if (funcAndArgs == null || !funcAndArgs[1].isEmpty()) {
            throw new SyntaxErrorException(name, "Invalid custom function declaration");
        }
        if (funcAndArgs[0].charAt(0) != VARIABLE_PREFIX_SYMBOL) {
            throw new SyntaxErrorException(funcAndArgs[0], "Custom function name must start with '$'");
        }
        return funcAndArgs[0];
    }

    static void checkVariableName(final String name) {
        if (name == null || name.length() < 2 || name.charAt(0) != VARIABLE_PREFIX_SYMBOL) {
            throw new SyntaxErrorException(name, "Variable name must start with '$' and has at least 1 character after");
        }
        if (new SyntaxDecomposer(name).deFunctionAndArgument(false) != null) {
            throw new SyntaxErrorException(name, "Variable name cannot define as a function");
        }
    }

    static String getNodeAsText(final PathTrace path, final String expression) {
        return getNodeAsText(path, NON_ARRAY_INDEX, expression);
    }

    static String getNodeAsText(final PathTrace path, final int index, final String expression) {
        final JsonNode result = getNodeByExpression(path, index, expression);
        return result == null ? EMPTY : result.asText();
    }

    static String getNodeAsTextExceptNull(final PathTrace path, final int index, final String expression) {
        final JsonNode result = getNodeByExpression(path, index, expression);
        return nodeIsNull(result) ? null : result.asText();
    }

    static boolean getNodeAsBoolean(final PathTrace path, final int index, final String expression) {
        final JsonNode result = getNodeByExpression(path, index, expression);
        return result != null && result.asBoolean();
    }

    static int getNodeAsInt(final PathTrace path, final String expression) {
        return getNodeAsInt(path, NON_ARRAY_INDEX, expression);
    }

    static int getNodeAsInt(final PathTrace path, final int index, final String expression) {
        final JsonNode result = getNodeByExpression(path, index, expression);
        return result == null || !result.isValueNode() ? 0 : result.asInt();
    }

    static JsonNode getNodeByExpression(final JsonNode node, final String expression,
                                        final Map<String, CustomFunction> customFunctions,
                                        final Map<String, JsonNode> variables) {
        return getNodeByExpression(node, NON_ARRAY_INDEX, expression, customFunctions, variables);
    }

    static JsonNode getNodeByExpression(final JsonNode node, final int index, final String expression,
                                        final Map<String, CustomFunction> customFunctions,
                                        final Map<String, JsonNode> variables) {
        return getPathNode(getPathByExpression(PathTrace.from(node, customFunctions, variables), index, expression));
    }

    static JsonNode getNodeByExpression(final PathTrace path, final String expression) {
        return getPathNode(getPathByExpression(path, expression));
    }

    static JsonNode getNodeByExpression(final PathTrace path, final int index, final String expression) {
        return getPathNode(getPathByExpression(path, index, expression));
    }

    static JsonNode getNodeByExpression(final PathTrace path, final int index, final String expression,
                                        final boolean defaultNullNode) {
        final JsonNode result = getNodeByExpression(path, index, expression);
        return result == null && defaultNullNode ? NullNode.getInstance() : result;
    }

    static PathTrace getPathByExpression(final PathTrace path, final String expression) {
        return getPathByExpression(path, NON_ARRAY_INDEX, expression);
    }

    static PathTrace getPathByExpression(final PathTrace path, final int index, final String expression) {
        if (path == null) {
            return null;
        }
        final List<String> steps = new SyntaxDecomposer(expression).dePathSteps();
        if (steps.isEmpty()) {
            return index > NON_ARRAY_INDEX && path.isArray() ? path.push(path.get(index)) : path;
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
                final String[] funcAndArgs = new SyntaxDecomposer(step).deFunctionAndArgument(false);
                final JsonNode value;
                if (funcAndArgs == null) {
                    value = path.getVariable(step);
                } else {
                    try {
                        final CustomFunction customFunction = path.getCustomFunction(funcAndArgs[0]);
                        if (customFunction == null) {
                            throw new IllegalArgumentException("Undefined");
                        }
                        new SyntaxDecomposer(funcAndArgs[1]).deFunctionParameters(0, 1);
                        value = customFunction.apply(getNodeByExpression(path, index, funcAndArgs[1]), index);
                    } catch (IllegalArgumentException e) {
                        throw new SyntaxErrorException(e.getMessage(), "Custom function " + step);
                    }
                }
                return getPathBySteps(path.clone(value == null ? NullNode.getInstance() : value), steps);
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
        if (index > NON_ARRAY_INDEX && path.isArray()) {
            return getPathBySteps(path.push(path.get(index)), steps);
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

    private static PathTrace getPathBySteps(final PathTrace path, final List<String> steps, final List<String> nextSteps) {
        if (steps == null || steps.isEmpty() || path == null) {
            return path;
        }
        String step = steps.remove(0);
        if (!step.isEmpty()) {
            switch (step.charAt(0)) {
                case WILDCARD_SYMBOL:
                    if (path.isEmpty()) {
                        return null;
                    }
                    final String[] levelsAndFilter = new SyntaxDecomposer(step).deWildcardLevelsAndFilter();
                    if (levelsAndFilter == null) {
                        if (path.isObject()) {
                            return wildcardAny(path, steps, nextSteps);
                        }
                        return wildcardAny(wildcardArrayNodeToList(path), steps, nextSteps, 1);
                    }
                    if (levelsAndFilter[0] != null) {
                        final int levels = levelsAndFilter[0].isEmpty() ? 0 : getNodeAsInt(path, levelsAndFilter[0]);
                        return wildcardAny(
                                path.isObject() ? Collections.singletonList(path) : wildcardArrayNodeToList(path),
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
                        steps.add(0, filter);
                        steps.add(1, ENTRY_VALUE_NAME);
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
                    steps.add(0, "[" + ENTRY_KEY_NAME + ".matches(" + strLiteral + ")]" + filterMode);
                    steps.add(1, ENTRY_VALUE_NAME);
                    break;
                case COLLECT_BRANCHES_SYMBOL:
                    if (nextSteps.isEmpty()) {
                        nextSteps.add(StringUtils.strip(step.substring(1)));
                        nextSteps.addAll(steps);
                    }
                    steps.clear();
                    return path;
                case INDEX_PREFIX_SYMBOL:
                    throw new SyntaxErrorException(step);
            }
        }
        final SyntaxDecomposer decomposer = new SyntaxDecomposer(step);
        final String[] funcAndArgs = decomposer.deFunctionAndArgument(true);
        if (funcAndArgs != null) {
            if (FILTRATE_DIVERT_ALL.equals(step.charAt(step.length() - 1))) {
                steps.add(0, "[]" + FILTRATE_DIVERT_ALL.getSymbol());
            }
            return getPathBySteps(new FunctionDispatcher(funcAndArgs[0], funcAndArgs[1]).apply(path), steps, nextSteps);
        }
        final ArrayFilter filter = decomposer.deFilterQuery();
        JsonNode node;
        if (filter.getNodeName() == null) {
            node = path.node();
        } else if (filter.getFilter() == null && filter.getMode() != FILTRATE_DIVERT_ALL) {
            if (path.isValueNode()) {
                return null;
            }
            if (path.isArray()) {
                node = forEachElement(path, filter.getNodeName(), filter.getMode(), steps, nextSteps);
                return getPathBySteps(path.push(node), steps, nextSteps);
            }
            node = path.get(filter.getNodeName());
        } else if (filter.getNodeName().isEmpty()) {
            node = filter.evaluateFilter(path, filter.getFilter());
        } else {
            node = filter.evaluateFilter(getPathByExpression(path, filter.getNodeName()), filter.getFilter());
        }
        if (node == null) {
            return null;
        }
        if (node.isArray() && !steps.isEmpty()) {
            node = forEachElement(path.push(node), null, filter.getMode(), steps, nextSteps);
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
            if (nodeIsNotNull(result)) {
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
                final JsonNode elem = it.next();
                if (elem.isObject()) {
                    nextLevel.add(path.push(elem));
                }
            }
        }
        return wildcardAny(nextLevel, steps, nextSteps, levels - 1);
    }

    private static ArrayNode forEachElement(final PathTrace path, final String elem, final FilterMode mode,
                                            final List<String> steps, final List<String> nextSteps) {
        final ArrayNode array = MAPPER.createArrayNode();
        if (path.isEmpty()) {
            return array;
        }
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
            return array;
        }
        final List<String> nextNextSteps = new ArrayList<>();
        final int size = path.containerSize();
        if (size < minArraySizeToUseMultiThread || threadPoolSize == 1) {
            for (JsonNode each : path.node()) {
                divertEachElement(path, elem, steps, nextNextSteps, each, (node) -> addArrayElement(array, node));
            }
        } else if (retainArrayOrder) {
            final JsonNode[] orderedNodes = new JsonNode[size];
            submitTasks(size, (i) ->
                divertEachElement(path, elem, steps, nextNextSteps, path.node().get(i), (node) -> orderedNodes[i] = node));
            addArrayElements(array, orderedNodes);
        } else {
            submitTasks(size, (i) ->
                divertEachElement(path, elem, steps, nextNextSteps, path.node().get(i), (node) -> addArrayElement(array, node)));
        }
        steps.clear();
        if (nextSteps.isEmpty()) {
            steps.addAll(nextNextSteps);
        } else {
            steps.addAll(nextSteps);
            nextSteps.clear();
        }
        return array;
    }

    private static void divertEachElement(final PathTrace path, final String elem, final List<String> steps,
                                          final List<String> nextSteps, final JsonNode each, Consumer<JsonNode> consumer) {
        final PathTrace result = getPathBySteps(path.push(elem == null ? each : each.get(elem)), new ArrayList<>(steps), nextSteps);
        if (result != null) {
            consumer.accept(result.node());
        }
    }
}

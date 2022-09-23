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

    static final char QUOTE_SYMBOL = '\'';

    static final String ENTRY_KEY_NAME = "key";

    static final String ENTRY_VALUE_NAME = "value";

    static final String GROUP_VALUE_NAME = "elements";

    static final String CURRENT_NODE = "?";

    private static final char WILDCARD_SYMBOL = '*';

    private static final char MATCHES_SYMBOL = '~';

    private static final char COLLECT_BRANCHES_SYMBOL = '@';

    private static final char INDEX_PREFIX_SYMBOL = '#';

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

    static String getNodeAsText(final JsonNode node, final String jossonPath) {
        return getNodeAsText(node, NON_ARRAY_INDEX, jossonPath);
    }

    static String getNodeAsText(final JsonNode node, final int index, final String jossonPath) {
        final JsonNode workNode = getNodeByPath(node, index, jossonPath);
        return workNode == null ? EMPTY : workNode.asText();
    }

    static String getNodeAsTextExceptNull(final JsonNode node, final int index, final String jossonPath) {
        final JsonNode workNode = getNodeByPath(node, index, jossonPath);
        return nodeIsNull(workNode) ? null : workNode.asText();
    }

    static boolean getNodeAsBoolean(final JsonNode node, final int index, final String jossonPath) {
        final JsonNode workNode = getNodeByPath(node, index, jossonPath);
        return workNode != null && workNode.asBoolean();
    }

    static int getNodeAsInt(final JsonNode node, final String jossonPath) {
        return getNodeAsInt(node, NON_ARRAY_INDEX, jossonPath);
    }

    static int getNodeAsInt(final JsonNode node, final int index, final String jossonPath) {
        final JsonNode workNode = getNodeByPath(node, index, jossonPath);
        return workNode == null || !workNode.isValueNode() ? 0 : workNode.asInt();
    }

    static JsonNode getNodeByPath(final JsonNode node, final String jossonPath) {
        return getNodeByPath(node, NON_ARRAY_INDEX, jossonPath);
    }

    static JsonNode getNodeByPath(JsonNode node, final int index, final String jossonPath) {
        if (node == null) {
            return null;
        }
        final List<String> keys = decomposePaths(jossonPath);
        if (keys.isEmpty()) {
            return index >= 0 && node.isArray() ? node.get(index) : node;
        }
        final String key = keys.get(0);
        if (key.charAt(0) == INDEX_PREFIX_SYMBOL) {
            final String indexType = keys.remove(0);
            switch (indexType) {
                case ZERO_BASED_INDEX:
                    return getNodeByKeys(IntNode.valueOf(index), keys);
                case ONE_BASED_INDEX:
                    return getNodeByKeys(IntNode.valueOf(index + 1), keys);
                case UPPERCASE_INDEX:
                    return getNodeByKeys(TextNode.valueOf(toAlphabetIndex(index, 'A')), keys);
                case LOWERCASE_INDEX:
                    return getNodeByKeys(TextNode.valueOf(toAlphabetIndex(index, 'a')), keys);
                case UPPER_ROMAN_INDEX:
                    return getNodeByKeys(TextNode.valueOf(toRomanIndex(index, true)), keys);
                case LOWER_ROMAN_INDEX:
                    return getNodeByKeys(TextNode.valueOf(toRomanIndex(index, false)), keys);
            }
            throw new IllegalArgumentException("Invalid index type: " + indexType);
        }
        if (PARENT_ARRAY_NODE.equals(key)) {
            keys.remove(0);
        } else {
            if (isCurrentNodePath(key)) {
                keys.remove(0);
            }
            if (index >= 0 && node.isArray()) {
                node = node.get(index);
            }
        }
        return getNodeByKeys(node, keys);
    }

    private static boolean isCurrentNodePath(final String path) {
        if (!path.startsWith(CURRENT_NODE)) {
            return false;
        }
        if (path.length() > 1) {
            throw new SyntaxErrorException(path);
        }
        return true;
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

    private static JsonNode getNodeByKeys(JsonNode node, List<String> keys) {
        if (node != null && !keys.isEmpty()) {
            try {
                node = literalToValueNode(keys.get(0));
                keys.remove(0);
            } catch (NumberFormatException ignore) {
            }
            while (node != null && !keys.isEmpty()) {
                final List<String> nextKeys = new ArrayList<>();
                node = getNodeByKeys(node, keys, nextKeys);
                keys = nextKeys;
            }
        }
        return node;
    }

    private static JsonNode getNodeByKeys(JsonNode node, final List<String> keys, final List<String> nextKeys) {
        if (keys == null || keys.isEmpty() || node == null) {
            return node;
        }
        String key = keys.get(0);
        switch (key.charAt(0)) {
            case WILDCARD_SYMBOL:
                if (node.isEmpty())  {
                    return null;
                }
                final String[] levelsAndFilter = matchWildcardLevelsAndFilter(key);
                if (levelsAndFilter == null) {
                    keys.remove(0);
                    if (node.isObject()) {
                        return wildcardAny(node, keys, nextKeys);
                    }
                    return wildcardAny(wildcardArrayNodeToList(node), keys, nextKeys, 1);
                }
                if (levelsAndFilter[0] != null) {
                    keys.remove(0);
                    final int levels = levelsAndFilter[0].isEmpty() ? 0 : getNodeAsInt(node, levelsAndFilter[0]);
                    return wildcardAny(
                            node.isObject() ? Collections.singletonList(node) : wildcardArrayNodeToList(node),
                            keys, nextKeys, levels);
                }
                final String filter = levelsAndFilter[1];
                if (filter.length() == 1) {
                    if (FILTRATE_COLLECT_ALL.equals(filter.charAt(0))) {
                        key = "toarray()";
                    } else {
                        key = "toarray()@";
                    }
                } else {
                    key = "entries()";
                    keys.add(1, filter);
                    keys.add(2, ENTRY_VALUE_NAME);
                }
                break;
            case MATCHES_SYMBOL:
                final char lastChar = key.charAt(key.length() - 1);
                final String filterMode = FILTRATE_COLLECT_ALL.equals(lastChar) || FILTRATE_DIVERT_ALL.equals(lastChar)
                        ? String.valueOf(lastChar) : EMPTY;
                final String strLiteral = StringUtils.strip(key.substring(1, key.length() - filterMode.length()));
                if (strLiteral.charAt(0) != QUOTE_SYMBOL || strLiteral.charAt(strLiteral.length() - 1) != QUOTE_SYMBOL) {
                    throw new SyntaxErrorException(key);
                }
                key = "entries()";
                keys.add(1, "[" + ENTRY_KEY_NAME + ".matches(" + strLiteral + ")]" + filterMode);
                keys.add(2, ENTRY_VALUE_NAME);
                break;
            case COLLECT_BRANCHES_SYMBOL:
                key = StringUtils.strip(key.substring(1));
                nextKeys.addAll(keys);
                if (key.isEmpty()) {
                    nextKeys.remove(0);
                } else {
                    nextKeys.set(0, key);
                }
                keys.clear();
                return node;
            case INDEX_PREFIX_SYMBOL:
                throw new SyntaxErrorException(key);
        }
        if (isCurrentNodePath(key)) {
            throw new SyntaxErrorException(key);
        }
        keys.remove(0);
        final String[] funcAndArgs = matchFunctionAndArgument(key, true);
        if (funcAndArgs != null) {
            if (FILTRATE_DIVERT_ALL.equals(key.charAt(key.length() - 1))) {
                keys.add(0, "[]" + FILTRATE_DIVERT_ALL.getSymbol());
            }
            return getNodeByKeys(new FuncDispatcher(funcAndArgs[0], funcAndArgs[1]).apply(node), keys, nextKeys);
        }
        final ArrayFilter filter = matchFilterQuery(key);
        if (filter.getFilter() == null && filter.getMode() != FILTRATE_DIVERT_ALL) {
            if (node.isValueNode()) {
                return null;
            }
            if (node.isArray()) {
                return forEachElement((ArrayNode) node, filter.getNodeName(), filter.getMode(), keys, nextKeys);
            }
            node = node.get(filter.getNodeName());
        } else {
            if (!filter.getNodeName().isEmpty()) {
                node = getNodeByPath(node, filter.getNodeName());
            }
            node = filter.evaluateFilter(node, filter.getFilter());
        }
        if (node == null) {
            return null;
        }
        if (node.isArray()) {
            return forEachElement((ArrayNode) node, null, filter.getMode(), keys, nextKeys);
        }
        return getNodeByKeys(node, keys, nextKeys);
    }


    private static List<JsonNode> wildcardArrayNodeToList(final JsonNode node) {
        final List<JsonNode> array = new ArrayList<>();
        for (JsonNode elem : node) {
            if (elem.isObject()) {
                array.add(elem);
            }
        }
        return array;
    }

    private static JsonNode wildcardAny(final JsonNode node, final List<String> keys, final List<String> nextKeys) {
        for (JsonNode elem : node) {
            final JsonNode tryNode = getNodeByKeys(elem, new ArrayList<>(keys), new ArrayList<>(nextKeys));
            if (!nodeIsNull(tryNode)) {
                return tryNode;
            }
        }
        return null;
    }

    private static JsonNode wildcardAny(final List<JsonNode> nodes, final List<String> keys,
                                        final List<String> nextKeys, final int levels) {
        for (JsonNode node : nodes) {
            final JsonNode matched = wildcardAny(node, keys, nextKeys);
            if (matched != null) {
                return matched;
            }
        }
        if (levels == 1) {
            return null;
        }
        final List<JsonNode> nextLevel = new ArrayList<>();
        for (JsonNode node : nodes) {
            for (Iterator<JsonNode> it = node.elements(); it.hasNext(); ) {
                JsonNode elem = it.next();
                if (elem.isObject()) {
                    nextLevel.add(elem);
                }
            }
        }
        return wildcardAny(nextLevel, keys, nextKeys, levels - 1);
    }

    private static JsonNode forEachElement(final ArrayNode node, final String elem, final FilterMode mode,
                                           final List<String> keys, final List<String> nextKeys) {
        final ArrayNode array = MAPPER.createArrayNode();
        if (mode != FILTRATE_DIVERT_ALL) {
            for (JsonNode each : node) {
                final JsonNode addNode = elem == null ? each : each.get(elem);
                if (addNode != null) {
                    if (mode == FILTRATE_COLLECT_ALL && addNode.isArray()) {
                        array.addAll((ArrayNode) addNode);
                    } else {
                        array.add(addNode);
                    }
                }
            }
        } else if (!node.isEmpty()) {
            List<String> nextNextKeys = null;
            for (JsonNode each : node) {
                final List<String> tempKeys = new ArrayList<>();
                addArrayElement(array, getNodeByKeys(elem == null ? each : each.get(elem), new ArrayList<>(keys), tempKeys));
                if (!tempKeys.isEmpty()) {
                    nextNextKeys = tempKeys;
                }
            }
            if (nextKeys.isEmpty() && nextNextKeys != null) {
                nextKeys.addAll(nextNextKeys);
            }
            keys.clear();
            keys.addAll(nextKeys);
            nextKeys.clear();
        }
        return getNodeByKeys(array, keys, nextKeys);
    }
}

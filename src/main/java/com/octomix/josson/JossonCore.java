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

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeParseException;
import java.time.temporal.ChronoUnit;
import java.util.*;

import static com.octomix.josson.ArrayFilter.FilterMode;
import static com.octomix.josson.ArrayFilter.FilterMode.*;
import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.PatternMatcher.*;
import static com.octomix.josson.commons.StringUtils.EMPTY;

/**
 * Static core functions for Josson.
 */
final class JossonCore {

    static final char QUOTE_SYMBOL = '\'';

    static final String CURRENT_NODE = "?";

    private static final String PARENT_ARRAY_NODE = "@";

    private static final char COLLECT_BRANCHES_SYMBOL = '@';

    private static final char INDEX_PREFIX_SYMBOL = '#';

    private static final String ZERO_BASED_INDEX = INDEX_PREFIX_SYMBOL + "";

    private static final String ONE_BASED_INDEX = INDEX_PREFIX_SYMBOL + "#";

    private static final String UPPERCASE_INDEX = INDEX_PREFIX_SYMBOL + "A";

    private static final String LOWERCASE_INDEX = INDEX_PREFIX_SYMBOL + "a";

    private static final String UPPER_ROMAN_INDEX = INDEX_PREFIX_SYMBOL + "R";

    private static final String LOWER_ROMAN_INDEX = INDEX_PREFIX_SYMBOL + "r";

    private static Locale locale = Locale.getDefault();

    private static ZoneId zoneId = ZoneId.systemDefault();

    static final int NON_ARRAY_INDEX = -1;

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

    static String unquoteString(final String quotedString) {
        final int last = quotedString.length() - 1;
        if (last < 1 || quotedString.charAt(0) != QUOTE_SYMBOL || quotedString.charAt(last) != QUOTE_SYMBOL) {
            throw new IllegalArgumentException("Argument is not a valid string literal: " + quotedString);
        }
        return quotedString.substring(1, last).replace("''", "'");
    }

    static String csvQuote(final String input) {
        final String quote = "\"";
        final String result;
        final boolean needQuote;
        if (input.contains(quote)) {
            needQuote = true;
            result = input.replace(quote, "\"\"");
        } else {
            needQuote = input.contains(",");
            result = input;
        }
        if (needQuote) {
            return quote + result + quote;
        }
        return result;
    }

    static JsonNode getImplicitVariable(final String name) {
        if (name.charAt(0) == '$') {
            switch (StringUtils.stripStart(name.substring(1), null).toLowerCase()) {
                case EMPTY:
                    return BooleanNode.TRUE;
                case "now":
                    return TextNode.valueOf(LocalDateTime.now().toString());
                case "today":
                    return TextNode.valueOf(LocalDate.now().atStartOfDay().toString());
                case "yesterday":
                    return TextNode.valueOf(LocalDate.now().atStartOfDay().minusDays(1).toString());
                case "tomorrow":
                    return TextNode.valueOf(LocalDate.now().atStartOfDay().plusDays(1).toString());
            }
        }
        return null;
    }

    static boolean isCurrentNodePath(final String path) {
        return isPathSymbol(path, CURRENT_NODE);
    }

    static boolean isParentArrayPath(final String path) {
        return isPathSymbol(path, PARENT_ARRAY_NODE);
    }

    private static boolean isPathSymbol(final String path, final String pathSymbol) {
        if (path.startsWith(pathSymbol)) {
            if (path.length() > 1) {
                throw new SyntaxErrorException(path);
            }
            return true;
        }
        return false;
    }

    static ValueNode toValueNode(final String literal) throws NumberFormatException {
        if (StringUtils.isEmpty(literal)) {
            return null;
        }
        if ("null".equalsIgnoreCase(literal)) {
            return NullNode.getInstance();
        }
        if ("true".equalsIgnoreCase(literal)) {
            return BooleanNode.TRUE;
        }
        if ("false".equalsIgnoreCase(literal)) {
            return BooleanNode.FALSE;
        }
        if (literal.charAt(0) == QUOTE_SYMBOL) {
            return TextNode.valueOf(unquoteString(literal));
        }
        if (literal.indexOf('.') < 0) {
            return IntNode.valueOf(Integer.parseInt(literal));
        }
        return DoubleNode.valueOf(Double.parseDouble(literal));
    }

    static boolean asBoolean(final JsonNode node) {
        return node != null && (node.isContainerNode() ? node.size() > 0 : node.asBoolean());
    }

    static LocalDateTime toLocalDateTime(final JsonNode node) {
        try {
            return LocalDateTime.parse(node.asText());
        } catch (DateTimeParseException e) {
            throw new IllegalArgumentException(e.getMessage());
        }
    }

    static LocalDateTime toLocalDate(final JsonNode node) {
        return toLocalDateTime(node).truncatedTo(ChronoUnit.DAYS);
    }

    static LocalDateTime offsetToLocalDateTime(final JsonNode node) {
        return toOffsetDateTime(node).atZoneSameInstant(zoneId).toLocalDateTime();
    }

    static OffsetDateTime toOffsetDateTime(final JsonNode node) {
        try {
            return OffsetDateTime.parse(node.asText());
        } catch (DateTimeParseException e) {
            throw new IllegalArgumentException(e.getMessage());
        }
    }

    static OffsetDateTime localToOffsetDateTime(final JsonNode node) {
        final LocalDateTime dateTime = toLocalDateTime(node);
        return dateTime.atOffset(zoneId.getRules().getOffset(dateTime));
    }

    static String getNodeAsText(final JsonNode node, final int index, final String jossonPath) {
        final JsonNode workNode = getNodeByPath(node, index, jossonPath);
        return workNode == null ? EMPTY : workNode.asText();
    }

    static int getNodeAsInt(final JsonNode node, final String jossonPath) {
        return getNodeAsInt(node, NON_ARRAY_INDEX, jossonPath);
    }

    static int getNodeAsInt(final JsonNode node, final int index, final String jossonPath) {
        final JsonNode workNode = getNodeByPath(node, index, jossonPath);
        if (workNode != null && workNode.isValueNode()) {
            if (workNode.isTextual()) {
                try {
                    return Integer.parseInt(workNode.asText());
                } catch (NumberFormatException e) {
                    return 0;
                }
            }
            return workNode.asInt();
        }
        return 0;
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
        if (isParentArrayPath(key)) {
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

    static boolean nodeHasValue(final JsonNode node) {
        return node != null && !node.isNull() && node.isValueNode();
    }

    static Object valueAsObject(final JsonNode node) {
        if (node.isIntegralNumber()) {
            return node.asInt();
        }
        if (node.isNumber()) {
            return node.asDouble();
        }
        return node.asText();
    }

    static Object[] valuesAsObjects(JsonNode node, final int index, final List<String> paramList) {
        Object[] objects = null;
        final int size = paramList.size();
        if (size == 0) {
            if (index >= 0) {
                node = node.get(index);
            }
            if (nodeHasValue(node)) {
                objects = new Object[]{valueAsObject(node)};
            }
        } else {
            objects = new Object[size];
            for (int i = 0; i < size; i++) {
                final JsonNode tryNode = getNodeByPath(node, index, paramList.get(i));
                if (!nodeHasValue(tryNode)) {
                    return null;
                }
                objects[i] = valueAsObject(tryNode);
            }
        }
        return objects;
    }

    private static String toAlphabetIndex(final int number, final int base) {
        if (number < 0) {
            return "-" + toAlphabetIndex(-number - 1, base);
        }
        final int quot = number / 26;
        return (quot == 0 ? EMPTY : toAlphabetIndex(quot - 1, base)) + (char) (base + number % 26);
    }

    private static String toRomanIndex(int number, final boolean isUpper) {
        if (number < 0) {
            return "-" + toRomanIndex(-number - 1, isUpper);
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
                node = toValueNode(keys.get(0));
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
        if (keys == null || keys.isEmpty()) {
            return node;
        }
        if (node == null || node.isNull()) {
            return null;
        }
        String key = keys.get(0);
        switch (key.charAt(0)) {
            case INDEX_PREFIX_SYMBOL:
                throw new SyntaxErrorException(key);
            case COLLECT_BRANCHES_SYMBOL:
                key = key.substring(1).trim();
                nextKeys.addAll(keys);
                if (key.isEmpty()) {
                    nextKeys.remove(0);
                } else {
                    nextKeys.set(0, key);
                }
                keys.clear();
                return node;
        }
        if (isCurrentNodePath(key)) {
            throw new SyntaxErrorException(key);
        }
        keys.remove(0);
        final String[] funcAndArgs = matchFunctionAndArgument(key, true);
        if (funcAndArgs != null) {
            if (key.charAt(key.length()-1) == FILTRATE_DIVERT_ALL.getSymbol()) {
                keys.add(0, "[]" + FILTRATE_DIVERT_ALL.getSymbol());
            }
            return getNodeByKeys(new FuncDispatcher(funcAndArgs[0], funcAndArgs[1]).apply(node), keys, nextKeys);
        }
        if (node.isValueNode()) {
            return null;
        }
        final ArrayFilter filter = matchFilterQuery(key);
        if (filter.getFilter() == null && filter.getMode() != FILTRATE_DIVERT_ALL) {
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

    private static JsonNode forEachElement(final ArrayNode node, final String elem, final FilterMode mode,
                                           final List<String> keys, final List<String> nextKeys) {
        final ArrayNode array = MAPPER.createArrayNode();
        if (mode == FILTRATE_DIVERT_ALL) {
            List<String> nextNextKeys = null;
            for (int i = 0; i < node.size(); i++) {
                nextNextKeys = new ArrayList<>();
                final JsonNode addNode = getNodeByKeys(
                        elem == null ? node.get(i) : node.get(i).get(elem), new ArrayList<>(keys), nextNextKeys);
                if (addNode != null) {
                    array.add(addNode);
                }
            }
            if (nextNextKeys != null) {
                if (nextKeys.isEmpty() && !nextNextKeys.isEmpty()) {
                    nextKeys.addAll(nextNextKeys);
                }
                keys.clear();
                keys.addAll(nextKeys);
                nextKeys.clear();
            }
        } else {
            for (int i = 0; i < node.size(); i++) {
                final JsonNode addNode = elem == null ? node.get(i) : node.get(i).get(elem);
                if (addNode != null) {
                    if (mode == FILTRATE_COLLECT_ALL && addNode.isArray()) {
                        array.addAll((ArrayNode) addNode);
                    } else {
                        array.add(addNode);
                    }
                }
            }
        }
        return getNodeByKeys(array, keys, nextKeys);
    }
}

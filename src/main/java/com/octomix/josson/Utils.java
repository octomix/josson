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

import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.format.DateTimeParseException;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.UnknownFormatConversionException;

import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.PatternMatcher.*;

class Utils {

    private Utils() {
    }

    static String quoteText(final String text) {
        return String.format("'%s'", text.replace("'", "''"));
    }

    static String unquoteText(final String quotedText) {
        final int last = quotedText.length() - 1;
        if (last < 1 || quotedText.charAt(0) != QUOTE_SYMBOL || quotedText.charAt(last) != QUOTE_SYMBOL) {
            throw new IllegalArgumentException("Argument is not a valid string literal: " + quotedText);
        }
        return quotedText.substring(1, last).replace("''", "'");
    }

    static ValueNode literalToValueNode(final String literal) throws NumberFormatException {
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
            return TextNode.valueOf(unquoteText(literal));
        }
        if (literal.indexOf('.') < 0) {
            return IntNode.valueOf(Integer.parseInt(literal));
        }
        return DoubleNode.valueOf(Double.parseDouble(literal));
    }

    static String valueNodeToLiteral(final JsonNode node) {
        return node.isTextual() ? quoteText(node.asText()) : node.asText();
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
        return toOffsetDateTime(node).atZoneSameInstant(getZoneId()).toLocalDateTime();
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
        return dateTime.atOffset(getZoneId().getRules().getOffset(dateTime));
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

    static void mergeObjects(final ObjectNode o1, final JsonNode o2) {
        o2.fields().forEachRemaining(field -> {
            JsonNode o1value = o1.get(field.getKey());
            if (o1value != null && o1value.isObject() && field.getValue().isObject()) {
                mergeObjects((ObjectNode) o1value, field.getValue());
            } else {
                o1.set(field.getKey(), field.getValue());
            }
        });
    }

    static String[] evaluateNameAndPath(final String[] nameAndPath, final JsonNode node, final int index) {
        if (nameAndPath[0].startsWith(":")) {
            final String name = getNodeAsText(node, index, nameAndPath[0].substring(1));
            checkElementName(name);
            return new String[]{name, nameAndPath[1]};
        }
        return nameAndPath;
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

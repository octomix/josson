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

import java.time.Instant;
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

    static Integer parseInteger(final String literal) {
        try {
            return Integer.parseInt(literal);
        } catch (NumberFormatException e) {
            return null;
        }
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
        if (literal.indexOf('.') >= 0) {
            return DoubleNode.valueOf(Double.parseDouble(literal));
        }
        try {
            return IntNode.valueOf(Integer.parseInt(literal));
        } catch (NumberFormatException e) {
            return LongNode.valueOf(Long.parseLong(literal));
        }
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

    static LocalDateTime epochMilliToLocalDateTime(final JsonNode node) {
        return Instant.ofEpochMilli(node.asLong()).atZone(getZoneId()).toLocalDateTime();
    }

    static LocalDateTime epochSecondToLocalDateTime(final JsonNode node) {
        return Instant.ofEpochSecond(node.asLong()).atZone(getZoneId()).toLocalDateTime();
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

    static OffsetDateTime epochMilliToOffsetDateTime(final JsonNode node) {
        return Instant.ofEpochMilli(node.asLong()).atZone(getZoneId()).toOffsetDateTime();
    }

    static OffsetDateTime epochSecondToOffsetDateTime(final JsonNode node) {
        return Instant.ofEpochSecond(node.asLong()).atZone(getZoneId()).toOffsetDateTime();
    }

    static long localDateTimeToEpochMilli(final JsonNode node) {
        return toLocalDateTime(node).atZone(getZoneId()).toInstant().toEpochMilli();
    }

    static long offsetDateTimeToEpochMilli(final JsonNode node) {
        return toOffsetDateTime(node).toInstant().toEpochMilli();
    }

    static long localDateTimeToEpochSecond(final JsonNode node) {
        return toLocalDateTime(node).atZone(getZoneId()).toEpochSecond();
    }

    static long offsetDateTimeToEpochSecond(final JsonNode node) {
        return toOffsetDateTime(node).toEpochSecond();
    }

    static boolean nodeIsNull(final PathTrace path) {
        return path == null || path.node() == null || path.node().isNull();
    }

    static boolean nodeIsNull(final JsonNode node) {
        return node == null || node.isNull();
    }

    static boolean nodeHasValue(final PathTrace path) {
        return path != null && nodeHasValue(path.node());
    }

    static boolean nodeHasValue(final JsonNode node) {
        return node != null && node.isValueNode() && !node.isNull();
    }

    static Object valueAsObject(final JsonNode node) {
        if (node.isInt()) {
            return node.asInt();
        }
        if (node.isLong()) {
            return node.asLong();
        }
        if (node.isNumber()) {
            return node.asDouble();
        }
        return node.asText();
    }

    static Object[] valuesAsObjects(PathTrace path, final int index, final List<String> paramList) {
        Object[] objects = null;
        final int size = paramList.size();
        if (size == 0) {
            if (index >= 0) {
                path = path.push(path.node().get(index));
            }
            if (nodeHasValue(path)) {
                objects = new Object[]{valueAsObject(path.node())};
            }
        } else {
            objects = new Object[size];
            for (int i = 0; i < size; i++) {
                final JsonNode result = getNodeByExpression(path, index, paramList.get(i));
                if (!nodeHasValue(result)) {
                    return null;
                }
                objects[i] = valueAsObject(result);
            }
        }
        return objects;
    }

    static void mergeObjects(final ObjectNode o1, final JsonNode o2) {
        o2.fields().forEachRemaining(field -> {
            final JsonNode o1value = o1.get(field.getKey());
            if (o1value != null && o1value.isObject() && field.getValue().isObject()) {
                mergeObjects((ObjectNode) o1value, field.getValue());
            } else if (o1value != null && o1value.isArray() && field.getValue().isArray()) {
                ((ArrayNode) o1value).addAll((ArrayNode) field.getValue());
            } else {
                o1.set(field.getKey(), field.getValue());
            }
        });
    }

    static void addArrayElement(final ArrayNode array, final JsonNode node) {
        if (node != null) {
            array.add(node);
        }
    }

    static String[] evaluateNameAndPath(final String[] nameAndPath, final PathTrace path, final int index) {
        if (nameAndPath[0].startsWith(":")) {
            return new String[]{getNodeAsText(path, index, nameAndPath[0].substring(1)), nameAndPath[1]};
        }
        return nameAndPath;
    }

    static String getLastElementName(final String path) {
        final List<String> steps = decomposePath(path);
        if (steps.isEmpty()) {
            throw new UnknownFormatConversionException("undefined");
        }
        String funcName = null;
        for (int i = steps.size() - 1; i >= 0; i--) {
            final String[] funcAndArgs = matchFunctionAndArgument(steps.get(i), true);
            if (funcAndArgs == null) {
                return matchFilterQuery(steps.get(i)).getNodeName();
            }
            if (funcName == null) {
                funcName = funcAndArgs[0];
            }
        }
        throw new UnknownFormatConversionException("_" + funcName);
    }
}

package com.octomix.josson;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.IntNode;
import com.fasterxml.jackson.databind.node.TextNode;

import java.time.temporal.ChronoField;
import java.time.temporal.ChronoUnit;
import java.util.List;

import static com.octomix.josson.GetFuncParam.getParamPath;
import static com.octomix.josson.GetFuncParam.getParamPathAndStrings;
import static com.octomix.josson.Josson.getNode;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Mapper.MAPPER;

class FuncDate {
    static JsonNode funcAmPmOfDay(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isTextual()) {
                    array.add(TextNode.valueOf(toLocalDateTime(valueNode).get(ChronoField.AMPM_OF_DAY) == 0 ? "AM" : "PM"));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(toLocalDateTime(node).get(ChronoField.AMPM_OF_DAY) == 0 ? "AM" : "PM");
    }

    static JsonNode funcChronometry(JsonNode node, String params, ChronoField field) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isTextual()) {
                    array.add(IntNode.valueOf(toLocalDateTime(valueNode).get(field)));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return IntNode.valueOf(toLocalDateTime(node).get(field));
    }

    static JsonNode funcDatePlus(JsonNode node, String params, ChronoUnit unit) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        if (pathAndParams.hasKey()) {
            node = getNode(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        long amount = Long.parseLong(getNodeAsText(node, pathAndParams.getValue().get(0)));
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isTextual()) {
                    array.add(TextNode.valueOf(toLocalDateTime(valueNode).plus(amount, unit).toString()));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(toLocalDateTime(node).plus(amount, unit).toString());
    }

    static JsonNode funcDateMinus(JsonNode node, String params, ChronoUnit unit) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        if (pathAndParams.hasKey()) {
            node = getNode(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        long amount = Long.parseLong(getNodeAsText(node, pathAndParams.getValue().get(0)));
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isTextual()) {
                    array.add(TextNode.valueOf(toLocalDateTime(valueNode).minus(amount, unit).toString()));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(toLocalDateTime(node).minus(amount, unit).toString());
    }

    static JsonNode funcDateTruncateTo(JsonNode node, String params, ChronoUnit unit) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isTextual()) {
                    array.add(TextNode.valueOf(toLocalDateTime(valueNode).truncatedTo(unit).toString()));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(toLocalDateTime(node).truncatedTo(unit).toString());
    }

    static JsonNode funcDateTruncateToMonth(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isTextual()) {
                    array.add(TextNode.valueOf(toLocalDateTime(valueNode).truncatedTo(ChronoUnit.DAYS)
                            .withDayOfMonth(1).toString()));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(toLocalDateTime(node).truncatedTo(ChronoUnit.DAYS)
                .withDayOfMonth(1).toString());
    }

    static JsonNode funcDateTruncateToYear(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isTextual()) {
                    array.add(TextNode.valueOf(toLocalDateTime(valueNode).truncatedTo(ChronoUnit.DAYS)
                            .withDayOfYear(1).toString()));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(toLocalDateTime(node).truncatedTo(ChronoUnit.DAYS)
                .withDayOfYear(1).toString());
    }

    static JsonNode funcDateWith(JsonNode node, String params, ChronoField field) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        if (pathAndParams.hasKey()) {
            node = getNode(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        long amount = Long.parseLong(getNodeAsText(node, pathAndParams.getValue().get(0)));
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isTextual()) {
                    array.add(TextNode.valueOf(toLocalDateTime(valueNode).with(field, amount).toString()));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(toLocalDateTime(node).with(field, amount).toString());
    }

    static JsonNode funcDayEnd(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isTextual()) {
                    array.add(TextNode.valueOf(toLocalDateTime(valueNode).truncatedTo(ChronoUnit.DAYS)
                            .plusDays(1).minusNanos(1).toString()));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(toLocalDateTime(node).truncatedTo(ChronoUnit.DAYS)
                .plusDays(1).minusNanos(1).toString());
    }

    static JsonNode funcMonthEnd(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isTextual()) {
                    array.add(TextNode.valueOf(toLocalDateTime(valueNode).truncatedTo(ChronoUnit.DAYS)
                            .withDayOfMonth(1).plusMonths(1).minusNanos(1).toString()));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(toLocalDateTime(node).truncatedTo(ChronoUnit.DAYS)
                .withDayOfMonth(1).plusMonths(1).minusNanos(1).toString());
    }

    static JsonNode funcYearEnd(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isTextual()) {
                    array.add(TextNode.valueOf(toLocalDateTime(valueNode).truncatedTo(ChronoUnit.DAYS)
                            .withDayOfYear(1).plusYears(1).minusNanos(1).toString()));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(toLocalDateTime(node).truncatedTo(ChronoUnit.DAYS)
                .withDayOfYear(1).plusYears(1).minusNanos(1).toString());
    }

    static JsonNode funcLengthOfMonth(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isTextual()) {
                    array.add(IntNode.valueOf(toLocalDateTime(valueNode).toLocalDate().lengthOfMonth()));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return IntNode.valueOf(toLocalDateTime(node).toLocalDate().lengthOfMonth());
    }

    static JsonNode funcLengthOfYear(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isTextual()) {
                    array.add(IntNode.valueOf(toLocalDateTime(valueNode).toLocalDate().lengthOfYear()));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return IntNode.valueOf(toLocalDateTime(node).toLocalDate().lengthOfYear());
    }

    static JsonNode funcLocalToOffsetDate(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(localToOffsetDateTime(textNode).toString()));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(localToOffsetDateTime(node).toString());
    }

    static JsonNode funcOffsetToLocalDate(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(offsetToLocalDateTime(textNode).toString()));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(offsetToLocalDateTime(node).toString());
    }
}

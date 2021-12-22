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
import static java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME;

public class FuncDate {

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
                    array.add(TextNode.valueOf(toLocalDateTime(valueNode).plus(amount, unit).format(ISO_LOCAL_DATE_TIME)));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(toLocalDateTime(node).plus(amount, unit).format(ISO_LOCAL_DATE_TIME));
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
                    array.add(TextNode.valueOf(toLocalDateTime(valueNode).minus(amount, unit).format(ISO_LOCAL_DATE_TIME)));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(toLocalDateTime(node).minus(amount, unit).format(ISO_LOCAL_DATE_TIME));
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
                    array.add(TextNode.valueOf(toLocalDateTime(valueNode).truncatedTo(unit).format(ISO_LOCAL_DATE_TIME)));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(toLocalDateTime(node).truncatedTo(unit).format(ISO_LOCAL_DATE_TIME));
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
                            .withDayOfMonth(1).format(ISO_LOCAL_DATE_TIME)));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(toLocalDateTime(node).truncatedTo(ChronoUnit.DAYS)
                .withDayOfMonth(1).format(ISO_LOCAL_DATE_TIME));
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
                            .withDayOfYear(1).format(ISO_LOCAL_DATE_TIME)));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(toLocalDateTime(node).truncatedTo(ChronoUnit.DAYS)
                .withDayOfYear(1).format(ISO_LOCAL_DATE_TIME));
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
                    array.add(TextNode.valueOf(toLocalDateTime(valueNode).with(field, amount).format(ISO_LOCAL_DATE_TIME)));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(toLocalDateTime(node).with(field, amount).format(ISO_LOCAL_DATE_TIME));
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
                            .plusDays(1).minusNanos(1).format(ISO_LOCAL_DATE_TIME)));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(toLocalDateTime(node).truncatedTo(ChronoUnit.DAYS)
                .plusDays(1).minusNanos(1).format(ISO_LOCAL_DATE_TIME));
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
                            .withDayOfMonth(1).plusMonths(1).minusNanos(1).format(ISO_LOCAL_DATE_TIME)));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(toLocalDateTime(node).truncatedTo(ChronoUnit.DAYS)
                .withDayOfMonth(1).plusMonths(1).minusNanos(1).format(ISO_LOCAL_DATE_TIME));
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
                            .withDayOfYear(1).plusYears(1).minusNanos(1).format(ISO_LOCAL_DATE_TIME)));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(toLocalDateTime(node).truncatedTo(ChronoUnit.DAYS)
                .withDayOfYear(1).plusYears(1).minusNanos(1).format(ISO_LOCAL_DATE_TIME));
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
}

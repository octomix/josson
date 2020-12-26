package com.octomix.josson.core;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.TextNode;

import java.text.DecimalFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import static com.octomix.josson.core.GetFuncParam.getParamStringLiteral;
import static com.octomix.josson.core.JossonCore.MAPPER;

public class FuncFormat {
    static JsonNode funcFormatDate(JsonNode node, String params) {
        String pattern = getParamStringLiteral(params);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(
                            LocalDateTime.parse(textNode.asText(), DateTimeFormatter.ISO_DATE_TIME)
                                    .format(DateTimeFormatter.ofPattern(pattern))));
                }
            }
            return array;
        } else if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(LocalDateTime.parse(node.asText(), DateTimeFormatter.ISO_DATE_TIME)
                .format(DateTimeFormatter.ofPattern(pattern)));
    }

    static JsonNode funcFormatNumber(JsonNode node, String params) {
        String pattern = getParamStringLiteral(params);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isValueNode()) {
                    array.add(TextNode.valueOf(
                            new DecimalFormat(pattern).format(textNode.asDouble())));
                }
            }
            return array;
        } else if (!node.isValueNode()) {
            return null;
        }
        return TextNode.valueOf(new DecimalFormat(pattern).format(node.asDouble()));
    }

    static JsonNode funcFormatText(JsonNode node, String params) {
        String pattern = getParamStringLiteral(params);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isValueNode()) {
                    array.add(TextNode.valueOf(
                            String.format(pattern, textNode.asText())));
                }
            }
            return array;
        } else if (!node.isValueNode()) {
            return null;
        }
        return TextNode.valueOf(String.format(pattern, node.asText()));
    }
}

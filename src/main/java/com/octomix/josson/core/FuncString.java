package com.octomix.josson.core;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.TextNode;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;

import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.regex.Matcher;

import static com.octomix.josson.core.GetFuncParam.getParamStartEnd;
import static com.octomix.josson.core.GetFuncParam.getParamStringLiteral;
import static com.octomix.josson.core.JossonCore.*;
import static com.octomix.josson.core.JossonCore.getNode;

public class FuncString {
    static JsonNode funcAbbreviate(JsonNode node, String params) {
        ImmutablePair<Integer, Integer> startEnd = getParamStartEnd(params);
        if (startEnd == null) {
            throw new UnsupportedOperationException("Missing function argument");
        }
        int offset = startEnd.left;
        int maxWidth;
        if (startEnd.right < Integer.MAX_VALUE) {
            maxWidth = startEnd.right;
        } else {
            maxWidth = offset;
            offset = 0;
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(StringUtils.abbreviate(textNode.asText(), offset, maxWidth)));
                }
            }
            return array;
        } else if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.abbreviate(node.asText(), offset, maxWidth));
    }

    static JsonNode funcB64Decode(JsonNode node, String params) {
        String value = getParamStringLiteral(params, false);
        if (value != null) {
            return TextNode.valueOf(new String(Base64.getDecoder().decode(value)));
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(
                            new String(Base64.getDecoder().decode(textNode.asText()))));
                }
            }
            return array;
        } else if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(new String(Base64.getDecoder().decode(node.asText())));
    }

    static JsonNode funcB64Encode(JsonNode node, String params) {
        String value = getParamStringLiteral(params, false);
        if (value != null) {
            return TextNode.valueOf(Base64.getEncoder().encodeToString(value.getBytes()));
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(
                            Base64.getEncoder().encodeToString(textNode.asText().getBytes())));
                }
            }
            return array;
        } else if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(Base64.getEncoder().encodeToString(node.asText().getBytes()));
    }

    static JsonNode funcConcat(JsonNode node, String params) {
        if (!node.isArray()) {
            return TextNode.valueOf(funcConcatElement(node, params, 0));
        }
        ArrayNode array = MAPPER.createArrayNode();
        for (int i  = 0; i < node.size(); i++) {
            String text = funcConcatElement(node.get(i), params, i + 1);
            if (text != null) {
                array.add(text);
            }
        }
        return array;
    }

    private static String funcConcatElement(JsonNode node, String params, int index) {
        Matcher m = DECOMPOSE_PARAMETERS.matcher(params);
        StringBuilder sb = new StringBuilder();
        while (m.find()) {
            String param = m.group(0).trim();
            if (StringUtils.isBlank(param)) {
                continue;
            }
            if (param.charAt(0) == '\'') {
                sb.append(unquoteString(param));
            } else if ("#".equals(param)) {
                sb.append(index);
            } else if ("?".equals(param)) {
                sb.append(node.asText());
            } else {
                JsonNode tryNode = getNode(node, param);
                if (tryNode == null || tryNode.isNull() || !tryNode.isValueNode()) {
                    return null;
                }
                sb.append(tryNode.asText());
            }
        }
        return sb.toString();
    }

    static JsonNode funcJoin(JsonNode node, String params) {
        String delimiter = getParamStringLiteral(params, false);
        if (!node.isArray()) {
            return node;
        }
        List<String> texts = new ArrayList<>();
        for (int i = 0; i < node.size(); i++) {
            JsonNode valueNode = node.get(i);
            if (valueNode.isValueNode()) {
                texts.add(valueNode.asText());
            }
        }
        return TextNode.valueOf(String.join(delimiter == null ? "" : delimiter, texts));
    }

    static JsonNode funcSubstr(JsonNode node, String params) {
        ImmutablePair<Integer, Integer> startEnd = getParamStartEnd(params);
        if (startEnd == null) {
            throw new UnsupportedOperationException("Missing function argument");
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(StringUtils.substring(
                            textNode.asText(), startEnd.left, startEnd.right)));
                }
            }
            return array;
        } else if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.substring(node.asText(), startEnd.left, startEnd.right));
    }

    static JsonNode funcToLower(JsonNode node, String params) {
        if (!StringUtils.isBlank(params)) {
            throw new UnsupportedOperationException("Not accept function argument");
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(textNode.asText().toLowerCase()));
                }
            }
            return array;
        } else if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(node.asText().toLowerCase());
    }

    static JsonNode funcToUpper(JsonNode node, String params) {
        if (!StringUtils.isBlank(params)) {
            throw new UnsupportedOperationException("Not accept function argument");
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(textNode.asText().toUpperCase()));
                }
            }
            return array;
        } else if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(node.asText().toUpperCase());
    }
}

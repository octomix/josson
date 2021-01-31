package com.octomix.josson;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.BooleanNode;
import org.apache.commons.lang3.StringUtils;

import java.util.regex.Matcher;

import static com.octomix.josson.GetFuncParam.*;
import static com.octomix.josson.JossonCore.*;

class FuncLogical {
    static BooleanNode funcContains(JsonNode node, String params, boolean ignoreCase, boolean not) {
        Matcher m = DECOMPOSE_PARAMETERS.matcher(params);
        getParamFindNextRequired(m);
        String value = m.group(0).trim();
        getParamNoMore(m);
        if (value.length() > 1 && value.charAt(0) == '\'') {
            value = unquoteString(value);
        } else {
            double num = Double.parseDouble(value);
            if (node.isArray()) {
                for (int i = 0; i < node.size(); i++) {
                    if (node.get(i).isNumber() || node.get(i).isTextual()) {
                        if (node.get(i).asDouble() == num) {
                            return BooleanNode.valueOf(!not);
                        }
                    }
                }
            }
            return BooleanNode.valueOf(not);
        }
        if (node.isTextual()) {
            return BooleanNode.valueOf(not ^ (ignoreCase
                    ? StringUtils.containsIgnoreCase(node.asText(), value)
                    : StringUtils.contains(node.asText(), value)));
        }
        if (node.isObject()) {
            return BooleanNode.valueOf(not ^ node.get(value) != null);
        }
        if (node.isArray()) {
            for (int i = 0; i < node.size(); i++) {
                if (node.get(i).isTextual()) {
                    if (ignoreCase) {
                        if (value.equalsIgnoreCase(node.get(i).asText())) {
                            return BooleanNode.valueOf(!not);
                        }
                    } else if (value.equals(node.get(i).asText())) {
                        return BooleanNode.valueOf(!not);
                    }
                }
            }
            return BooleanNode.valueOf(not);
        }
        return BooleanNode.FALSE;
    }

    static BooleanNode funcEndsWith(JsonNode node, String params, boolean ignoreCase, boolean not) {
        String value = getParamStringLiteral(params);
        if (!node.isTextual()) {
            return BooleanNode.FALSE;
        }
        return BooleanNode.valueOf(not ^ (ignoreCase ?
                StringUtils.endsWithIgnoreCase(node.asText(), value) :
                StringUtils.endsWith(node.asText(), value)));
    }

    static BooleanNode funcEquals(JsonNode node, String params, boolean ignoreCase, boolean not) {
        String value = getParamStringLiteral(params);
        if (!node.isTextual()) {
            return BooleanNode.FALSE;
        }
        return BooleanNode.valueOf(not ^ (ignoreCase ?
                StringUtils.equalsIgnoreCase(node.asText(), value) :
                StringUtils.equals(node.asText(), value)));
    }

    static BooleanNode funcIn(JsonNode node, String params, boolean ignoreCase, boolean not) {
        ArrayNode arrayNode = getParamArray(params, node);
        if (node.isNumber()) {
            double num = node.asDouble();
            for (int i = 0; i < arrayNode.size(); i++) {
                JsonNode value = arrayNode.get(i);
                if (value.isNumber() || value.isTextual()) {
                    if (value.asDouble() == num) {
                        return BooleanNode.valueOf(!not);
                    }
                }
            }
            return BooleanNode.valueOf(not);
        } else if (node.isTextual()) {
            String text = node.asText();
            for (int i = 0; i < arrayNode.size(); i++) {
                JsonNode value = arrayNode.get(i);
                if (value.isNumber() || value.isTextual()) {
                    if (ignoreCase) {
                        if (value.asText().equalsIgnoreCase(text)) {
                            return BooleanNode.valueOf(!not);
                        }
                    } else if (value.asText().equals(text)) {
                        return BooleanNode.valueOf(!not);
                    }
                }
            }
            return BooleanNode.valueOf(not);
        }
        return BooleanNode.FALSE;
    }

    static BooleanNode funcIsNull(JsonNode node, String params, boolean not) {
        getParamNotAccept(params);
        return BooleanNode.valueOf(not ^ node.isNull());
    }

    static BooleanNode funcNot(JsonNode node, String params) {
        getParamNotAccept(params);
        if (!node.isBoolean()) {
            return BooleanNode.FALSE;
        }
        return BooleanNode.valueOf(!node.asBoolean());
    }

    static BooleanNode funcStartsWith(JsonNode node, String params, boolean ignoreCase, boolean not) {
        String value = getParamStringLiteral(params);
        if (!node.isTextual()) {
            return BooleanNode.FALSE;
        }
        return BooleanNode.valueOf(not ^ (ignoreCase ?
                StringUtils.startsWithIgnoreCase(node.asText(), value) :
                StringUtils.startsWith(node.asText(), value)));
    }
}

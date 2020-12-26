package com.octomix.josson.core;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.BooleanNode;
import org.apache.commons.lang3.StringUtils;

import java.util.regex.Matcher;

import static com.octomix.josson.core.GetFuncParam.getParamStringLiteral;
import static com.octomix.josson.core.JossonCore.DECOMPOSE_PARAMETERS;
import static com.octomix.josson.core.JossonCore.unquoteString;

public class FuncLogical {
    static BooleanNode funcContains(JsonNode node, String params, boolean ignoreCase) {
        Matcher m = DECOMPOSE_PARAMETERS.matcher(params);
        if (!m.find()) {
            throw new UnsupportedOperationException("Missing function argument");
        }
        String value = m.group(0).trim();
        if (value.length() > 1 && value.charAt(0) == '\'') {
            value = unquoteString(value);
        } else {
            double num = Double.parseDouble(value);
            if (node.isArray()) {
                for (int i = 0; i < node.size(); i++) {
                    if (node.isNumber() || node.isTextual()) {
                        if (node.asDouble() == num) {
                            return BooleanNode.TRUE;
                        }
                    }
                }
            }
            return BooleanNode.FALSE;
        }
        if (node.isTextual()) {
            return BooleanNode.valueOf(ignoreCase ?
                    StringUtils.containsIgnoreCase(node.asText(), value) :
                    StringUtils.contains(node.asText(), value));
        }
        if (node.isObject()) {
            return BooleanNode.valueOf(node.get(value) != null);
        }
        if (node.isArray()) {
            for (int i = 0; i < node.size(); i++) {
                if (node.isTextual()) {
                    if (ignoreCase) {
                        if (value.equalsIgnoreCase(node.asText())) {
                            return BooleanNode.TRUE;
                        }
                    } else if (value.equals(node.asText())) {
                        return BooleanNode.TRUE;
                    }
                }
            }
        }
        return BooleanNode.FALSE;
    }

    static BooleanNode funcEndsWith(JsonNode node, String params, boolean ignoreCase) {
        String value = getParamStringLiteral(params);
        if (!node.isTextual()) {
            return BooleanNode.FALSE;
        }
        return BooleanNode.valueOf(ignoreCase ?
                StringUtils.endsWithIgnoreCase(node.asText(), value) :
                StringUtils.endsWith(node.asText(), value));
    }

    static BooleanNode funcEquals(JsonNode node, String params, boolean ignoreCase) {
        String value = getParamStringLiteral(params);
        if (!node.isTextual()) {
            return BooleanNode.FALSE;
        }
        return BooleanNode.valueOf(ignoreCase ?
                StringUtils.equalsIgnoreCase(node.asText(), value) :
                StringUtils.equals(node.asText(), value));
    }

    static BooleanNode funcStartsWith(JsonNode node, String params, boolean ignoreCase) {
        String value = getParamStringLiteral(params);
        if (!node.isTextual()) {
            return BooleanNode.FALSE;
        }
        return BooleanNode.valueOf(ignoreCase ?
                StringUtils.startsWithIgnoreCase(node.asText(), value) :
                StringUtils.startsWith(node.asText(), value));
    }
}

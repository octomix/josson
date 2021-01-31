package com.octomix.josson;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.ImmutableTriple;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;

import static com.octomix.josson.Josson.getNode;
import static com.octomix.josson.Josson.readJsonNode;
import static com.octomix.josson.JossonCore.*;

class GetFuncParam {
    static void getParamThrowMissing() {
        throw new UnsupportedOperationException("Missing function argument");
    }

    static void getParamNotAccept(String params) {
        if (!StringUtils.isBlank(params)) {
            throw new UnsupportedOperationException("Not accept function argument");
        }
    }

    static void getParamFindNextRequired(Matcher m) {
        if (!m.find() || StringUtils.isBlank(m.group(0))) {
            getParamThrowMissing();
        }
    }

    static void getParamNoMore(Matcher m) {
        if (m.find()) {
            throw new UnsupportedOperationException("Too many function arguments");
        }
    }

    static ImmutablePair<Integer, Integer> getParamStartEnd(String params) {
        Matcher m = DECOMPOSE_PARAMETERS.matcher(params);
        if (!m.find()) {
            getParamThrowMissing();
        }
        int start = StringUtils.isBlank(m.group(0)) ? 0 : Integer.parseInt(m.group(0));
        int end = m.find() && !StringUtils.isBlank(m.group(0)) ? Integer.parseInt(m.group(0)) : Integer.MAX_VALUE;
        getParamNoMore(m);
        return ImmutablePair.of(start, end);
    }

    static ImmutableTriple<Integer, Integer, Integer> getParamStartEndStep(String params) {
        Matcher m = DECOMPOSE_PARAMETERS.matcher(params);
        if (!m.find()) {
            getParamThrowMissing();
        }
        int start = StringUtils.isBlank(m.group(0)) ? 0 : Integer.parseInt(m.group(0));
        int end = m.find() && !StringUtils.isBlank(m.group(0)) ? Integer.parseInt(m.group(0)) : Integer.MAX_VALUE;
        int step = m.find() && !StringUtils.isBlank(m.group(0)) ? Integer.parseInt(m.group(0)) : 1;
        getParamNoMore(m);
        if (step == 0) {
            step = 1;
        }
        return ImmutableTriple.of(start, end, step);
    }

    static String getParamStringLiteral(String params) {
        return getParamStringLiteral(params, true);
    }

    static String getParamStringLiteral(String params, boolean required) {
        Matcher m = DECOMPOSE_PARAMETERS.matcher(params);
        if (!m.find() || StringUtils.isBlank(m.group(0))) {
            if (required) {
                getParamThrowMissing();
            }
            return null;
        }
        String pattern = m.group(0).trim();
        getParamNoMore(m);
        if (pattern.length() > 1 && pattern.charAt(0) != '\'') {
            throw new UnsupportedOperationException("Argument must be string literal");
        }
        return unquoteString(pattern);
    }

    static ImmutablePair<Integer, String> getParamIntAndString(String params) {
        Matcher m = DECOMPOSE_PARAMETERS.matcher(params);
        getParamFindNextRequired(m);
        Integer num = Integer.parseInt(m.group(0));
        String str = getParamStringLiteral(params.substring(m.end() + 1), false);
        return ImmutablePair.of(num, str);
    }

    static List<ImmutablePair<String, String>> getParamNamePath(String params) {
        List<ImmutablePair<String, String>> elements = new ArrayList<>();
        Matcher m = DECOMPOSE_PARAMETERS.matcher(params);
        while (m.find()) {
            String[] values = m.group(0).split(":", 2);
            if (values.length == 0 || StringUtils.isBlank(values[0])) {
                continue;
            }
            String name = values[0].trim();
            String path = null;
            if (!"?".equals(name)) {
                if (values.length == 1) {
                    path = name;
                    Matcher m2 = DECOMPOSE_PATH.matcher(path);
                    while (m2.find()) {
                        if (!IS_FUNCTION_PATTERN.matcher(m2.group(0)).find()) {
                            Matcher m3 = IS_ARRAY_NODE_QUERY.matcher(m2.group(0));
                            if (m3.find()) {
                                name = m3.group(1).trim();
                            } else {
                                name = m2.group(0).trim();
                            }
                        }
                    }
                } else if (!StringUtils.isBlank(values[1])) {
                    path = values[1].trim();
                }
            }
            elements.add(ImmutablePair.of(name, path));
        }
        return elements;
    }

    static ArrayNode getParamArray(String params, JsonNode node) {
        if (StringUtils.isBlank(params)) {
            getParamThrowMissing();
        }
        try {
            params = params.trim();
            JsonNode arrayNode = params.startsWith("[") ? readJsonNode(params) : getNode(node, params);
            if (arrayNode != null && arrayNode.isArray()) {
                return (ArrayNode) arrayNode;
            }
            throw new Exception("");
        } catch (Exception e) {
            throw new UnsupportedOperationException("Argument must be an array");
        }
    }
}

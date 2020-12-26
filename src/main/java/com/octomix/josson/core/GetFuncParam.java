package com.octomix.josson.core;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.ImmutableTriple;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;

import static com.octomix.josson.core.JossonCore.*;

public class GetFuncParam {
    static ImmutablePair<Integer, Integer> getParamStartEnd(String params) {
        Matcher m = DECOMPOSE_PARAMETERS.matcher(params);
        if (!m.find()) {
            return null;
        }
        int start = StringUtils.isBlank(m.group(0)) ? 0 : Integer.parseInt(m.group(0));
        int end = m.find() && !StringUtils.isBlank(m.group(0)) ? Integer.parseInt(m.group(0)) : Integer.MAX_VALUE;
        return ImmutablePair.of(start, end);
    }

    static ImmutableTriple<Integer, Integer, Integer> getParamStartEndStep(String params) {
        Matcher m = DECOMPOSE_PARAMETERS.matcher(params);
        if (!m.find()) {
            return null;
        }
        int start = StringUtils.isBlank(m.group(0)) ? 0 : Integer.parseInt(m.group(0));
        int end = m.find() && !StringUtils.isBlank(m.group(0)) ? Integer.parseInt(m.group(0)) : Integer.MAX_VALUE;
        int step = m.find() && !StringUtils.isBlank(m.group(0)) ? Integer.parseInt(m.group(0)) : 1;
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
                throw new UnsupportedOperationException("Missing function argument");
            }
            return null;
        }
        String pattern = m.group(0).trim();
        if (pattern.length() > 1 && pattern.charAt(0) != '\'') {
            throw new UnsupportedOperationException("Argument must be string literal");
        }
        return unquoteString(pattern);
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
}

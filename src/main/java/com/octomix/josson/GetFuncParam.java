package com.octomix.josson;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.ImmutableTriple;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.octomix.josson.Josson.getNode;
import static com.octomix.josson.Josson.readJsonNode;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.PatternMatcher.*;

class GetFuncParam {
    static void getParamNotAccept(String params) {
        if (!StringUtils.isBlank(params)) {
            throw new IllegalArgumentException("Not accept function argument");
        }
    }

    static ImmutablePair<Integer, Integer> getParamStartEnd(String params) {
        List<String> paramList = decomposeFunctionParameters(params, 0, 2);
        int start = paramList.get(0).isEmpty() ? 0 : Integer.parseInt(paramList.get(0));
        int end = paramList.size() > 1 ? Integer.parseInt(paramList.get(1)) : Integer.MAX_VALUE;
        return ImmutablePair.of(start, end);
    }

    static ImmutableTriple<Integer, Integer, Integer> getParamStartEndStep(String params) {
        List<String> paramList = decomposeFunctionParameters(params, 0, 3);
        int start = paramList.get(0).isEmpty() ? 0 : Integer.parseInt(paramList.get(0));
        int end = paramList.size() > 1 && !paramList.get(1).isEmpty() ?
                Integer.parseInt(paramList.get(1)) : Integer.MAX_VALUE;
        int step = paramList.size() > 2 ? Integer.parseInt(paramList.get(2)) : 1;
        return ImmutableTriple.of(start, end, step);
    }

    static String getParamStringLiteral(String params) {
        return getParamStringLiteral(params, true);
    }

    static String getParamStringLiteral(String params, boolean required) {
        List<String> paramList = decomposeFunctionParameters(params, required ? 1 : 0, 1);
        return paramList.size() > 0 ? unquoteString(paramList.get(0)) : null;
    }

    static ImmutablePair<Integer, String> getParamIntAndString(String params) {
        List<String> paramList = decomposeFunctionParameters(params, 1, 2);
        return ImmutablePair.of(
                Integer.parseInt(paramList.get(0)),
                paramList.size() > 1 ? unquoteString(paramList.get(1)) : null);
    }

    static ImmutableTriple<String, String, Integer> getParam2StringAndInt(String params) {
        List<String> paramList = decomposeFunctionParameters(params, 2, 3);
        return ImmutableTriple.of(
                unquoteString(paramList.get(0)),
                unquoteString(paramList.get(1)),
                paramList.size() > 2 ? Integer.parseInt(paramList.get(2)) : null);
    }

    static Map<String, String> getParamNamePath(List<String> paramList) {
        Map<String, String> elements = new HashMap<>();
        for (String param : paramList) {
            String[] values = param.split(":", 2);
            String name = values[0].trim();
            String path = null;
            if (!"?".equals(name)) {
                if (values.length == 1) {
                    path = name;
                    List<String> paths = decomposePaths(path);
                    for (int i = paths.size() - 1; i >= 0 ; i--) {
                        if (matchFunctionAndArgument(paths.get(i)) == null) {
                            String[] tokens = matchArrayNodeQuery(paths.get(i));
                            name = tokens == null ? paths.get(i) : tokens[0];
                            break;
                        }
                    }
                } else if (!StringUtils.isBlank(values[1])) {
                    path = values[1].trim();
                }
            }
            elements.put(name, path);
        }
        return elements;
    }

    static ArrayNode getParamArray(String params, JsonNode node) {
        List<String> paramList = decomposeFunctionParameters(params, 1, -1);
        ArrayNode arrayNode = Josson.createArrayNode();
        for (String param : paramList) {
            if (param.startsWith("[")) {
                try {
                    arrayNode.addAll((ArrayNode) readJsonNode(param));
                } catch (JsonProcessingException e) {
                    throw new IllegalArgumentException("Invalid JSON array: " + param);
                }
            } else {
                JsonNode value = getNode(node, param);
                if (value != null) {
                    arrayNode.add(value);
                }
            }
        }
        return arrayNode;
    }
}

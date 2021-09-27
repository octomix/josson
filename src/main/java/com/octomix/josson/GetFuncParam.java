/*
 * Copyright 2020 Octomix Software Technology Limited
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

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.octomix.josson.commons.StringUtils;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static com.octomix.josson.Josson.getNode;
import static com.octomix.josson.Josson.readJsonNode;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.PatternMatcher.*;

class GetFuncParam {
    static String getParamPath(String input) {
        List<String> paramList = decomposeFunctionParameters(input, 0, 1);
        return paramList.isEmpty() ? null : paramList.get(0);
    }

    static Pair<String, List<String>> getParamPathAndStrings(String input, int minCount, int maxCount) {
        List<String> paramList = decomposeFunctionParameters(input, minCount, maxCount + 1);
        String path = paramList.size() > maxCount ? paramList.remove(0) : null;
        return Pair.of(path, paramList);
    }

    static Pair<String, Integer> getParamPathAndInt(String params) {
        List<String> paramList = decomposeFunctionParameters(params, 1, 2);
        String path = paramList.size() > 1 ? paramList.remove(0) : null;
        return Pair.of(path, Integer.parseInt(paramList.get(0)));
    }

    static Pair<String, Integer[]> getParamPathAndStartEnd(String params) {
        List<String> paramList = decomposeFunctionParameters(params, 0, 3);
        String path = paramList.size() > 2 ? paramList.remove(0) : null;
        Integer[] args = new Integer[2];
        args[0] = paramList.get(0).isEmpty() ? 0 : Integer.parseInt(paramList.get(0));
        args[1] = paramList.size() > 1 ? Integer.parseInt(paramList.get(1)) : Integer.MAX_VALUE;
        return Pair.of(path, args);
    }

    static Pair<String, Integer[]> getParamPathAndStartEndStep(String params) {
        List<String> paramList = decomposeFunctionParameters(params, 0, 4);
        String path = paramList.size() > 3 ? paramList.remove(0) : null;
        Integer[] args = new Integer[3];
        args[0] = paramList.get(0).isEmpty() ? 0 : Integer.parseInt(paramList.get(0));
        args[1] = paramList.size() > 1 && !paramList.get(1).isEmpty() ?
                Integer.parseInt(paramList.get(1)) : Integer.MAX_VALUE;
        args[2] = paramList.size() > 2 ? Integer.parseInt(paramList.get(2)) : 1;
        return Pair.of(path, args);
    }

    static Triple<String, Integer, String> getParamPathAndAlignment(String params) {
        List<String> paramList = decomposeFunctionParameters(params, 1, 3);
        String path = paramList.size() > 2 ? paramList.remove(0) : null;
        return Triple.of(
                path, Integer.parseInt(paramList.get(0)),
                paramList.size() > 1 ? unquoteString(paramList.get(1)) : null);
    }

    static Triple<String, String[], Integer> getParamPathAnd2StringAndInt(String params) {
        List<String> paramList = decomposeFunctionParameters(params, 2, 4);
        String path = paramList.size() > 3 ? paramList.remove(0) : null;
        String[] args = new String[]{unquoteString(paramList.get(0)), unquoteString(paramList.get(1))};
        return Triple.of(path, args, paramList.size() > 2 ? Integer.parseInt(paramList.get(2)) : null);
    }

    static Map<String, String> getParamNamePath(List<String> paramList) {
        Map<String, String> elements = new LinkedHashMap<>();
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
                            name = matchFilterQuery(paths.get(i))[0];
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

    static ArrayNode getParamArrayOrItself(String params, JsonNode node) {
        List<String> paramList = decomposeFunctionParameters(params, 0, -1);
        if (paramList.isEmpty()) {
            if (node.isArray()) {
                return (ArrayNode) node;
            }
            return null;
        }
        return getParamArray(paramList, node);
    }

    static ArrayNode getParamArray(String params, JsonNode node) {
        return getParamArray(decomposeFunctionParameters(params, 1, -1), node);
    }

    static ArrayNode getParamArray(List<String> paramList, JsonNode node) {
        ArrayNode array = Josson.createArrayNode();
        for (String param : paramList) {
            if (param.startsWith("[")) {
                try {
                    array.addAll((ArrayNode) readJsonNode(param));
                } catch (JsonProcessingException e) {
                    throw new IllegalArgumentException("Invalid JSON array: " + param);
                }
            } else if (node.isArray()) {
                for (int i = 0; i < node.size(); i++) {
                    JsonNode tryNode = getNode(node.get(i), param);
                    if (tryNode != null) {
                        array.add(tryNode);
                    }
                }
            } else {
                JsonNode tryNode = getNode(node, param);
                if (tryNode != null) {
                    if (tryNode.isArray()) {
                        array.addAll((ArrayNode) tryNode);
                    } else {
                        array.add(tryNode);
                    }
                }
            }
        }
        return array;
    }
}

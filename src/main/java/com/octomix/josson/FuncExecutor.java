/*
 * Copyright 2020-2022 Octomix Software Technology Limited
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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UnknownFormatConversionException;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;

import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.PatternMatcher.*;
import static com.octomix.josson.Utils.addArrayElement;
import static com.octomix.josson.commons.StringUtils.EMPTY;

/**
 * Common logic to execute functions.
 */
final class FuncExecutor {

    /**
     * Unlimited number of parameters with optional data path.
     */
    static final int UNLIMITED_WITH_PATH = -1;

    /**
     * Unlimited number of parameters and not accept data path.
     */
    static final int UNLIMITED_AND_NO_PATH = -2;

    private FuncExecutor() {
    }

    static PathTrace getParamPath(final PathTrace path, final String params) {
        final List<String> paramList = decomposeFunctionParameters(params, 0, 1);
        return paramList.isEmpty() ? path : getPathByExpression(path, paramList.get(0));
    }

    static Pair<PathTrace, List<String>> getParamPathAndStrings(final PathTrace path, final String params,
                                                                final int min, final int max) {
        final List<String> paramList = decomposeFunctionParameters(params, min, max + 1);
        final String expression = max <= UNLIMITED_AND_NO_PATH ? EMPTY : paramList.size() > max ? paramList.remove(0) : null;
        return Pair.of(expression == null ? path : getPathByExpression(path, expression), paramList);
    }

    static Map<String, String> getParamNamePath(final List<String> paramList) {
        final Map<String, String> elements = new LinkedHashMap<>();
        int noNameCount = 0;
        for (String param : paramList) {
            String[] namePath;
            try {
                namePath = decomposeNameAndPath(param);
            } catch (UnknownFormatConversionException e) {
                namePath = new String[]{e.getConversion() + ++noNameCount, param};
            }
            elements.put(namePath[0], namePath[1]);
        }
        return elements;
    }

    static PathTrace getParamArrayOrItselfIsContainer(final PathTrace path, final String params) {
        final List<String> paramList = decomposeFunctionParameters(params, 0, UNLIMITED_WITH_PATH);
        if (paramList.isEmpty()) {
            if (path.node().isContainerNode()) {
                return path;
            }
            return null;
        }
        return getParamArray(path, paramList);
    }

    static PathTrace getParamArrayOrItself(final PathTrace path, final String params) {
        final List<String> paramList = decomposeFunctionParameters(params, 0, UNLIMITED_WITH_PATH);
        if (paramList.isEmpty()) {
            if (path.node().isArray()) {
                return path;
            }
            return null;
        }
        return getParamArray(path, paramList);
    }

    private static PathTrace getParamArray(final PathTrace path, final List<String> paramList) {
        final ArrayNode array = MAPPER.createArrayNode();
        for (String param : paramList) {
            if (path.node().isArray()) {
                for (int i = 0; i < path.node().size(); i++) {
                    addArrayElement(array, getNodeByExpression(path, i, param));
                }
            } else {
                final JsonNode result = getNodeByExpression(path, param);
                if (result != null) {
                    if (result.isArray()) {
                        array.addAll((ArrayNode) result);
                    } else {
                        array.add(result);
                    }
                }
            }
        }
        return path.push(array);
    }

    static PathTrace applyWithoutParam(final PathTrace path, final String params, final Function<PathTrace, PathTrace> action) {
        final PathTrace workNode = getParamPath(path, params);
        return workNode == null ? null : action.apply(workNode);
    }

    static PathTrace applyWithoutParam(final PathTrace path, final String params, final Predicate<JsonNode> isValid,
                                       final BiFunction<Pair<PathTrace, Integer>, List<String>, PathTrace> action) {
        final List<String> paramList = decomposeFunctionParameters(params, 0, 1);
        return applyAction(path, paramList.isEmpty() ? null : paramList.get(0), isValid, action, null);
    }

    static PathTrace applyWithParams(final PathTrace path, final String params, final int min, final int max,
                                     final Predicate<JsonNode> isValid,
                                     final BiFunction<Pair<PathTrace, Integer>, List<String>, PathTrace> action) {
        final List<String> paramList = decomposeFunctionParameters(params, min, max + 1);
        final String expression = max <= UNLIMITED_AND_NO_PATH ? EMPTY : paramList.size() > max ? paramList.remove(0) : null;
        return applyAction(path, expression, isValid, action, paramList);
    }

    static PathTrace applyTextNode(final PathTrace path, final String params,
                                   final Function<PathTrace, String> transform) {
        return applyWithoutParam(path, params, JsonNode::isTextual,
            (data, paramList) -> path.push(TextNode.valueOf(transform.apply(data.getKey()))));
    }

    static PathTrace applyTextNodeToInt(final PathTrace path, final String params,
                                        final Function<PathTrace, Integer> transform) {
        return applyWithoutParam(path, params, JsonNode::isTextual,
            (data, paramList) -> path.push(IntNode.valueOf(transform.apply(data.getKey()))));
    }

    static PathTrace applyTextNodeToLong(final PathTrace path, final String params,
                                         final Function<PathTrace, Long> transform) {
        return applyWithoutParam(path, params, JsonNode::isTextual,
            (data, paramList) -> path.push(LongNode.valueOf(transform.apply(data.getKey()))));
    }

    static PathTrace applyNumberNodeToText(final PathTrace path, final String params,
                                           final Function<PathTrace, String> transform) {
        return applyWithoutParam(path, params, paramPath -> paramPath.isNumber() || paramPath.isTextual(),
            (data, paramList) -> path.push(TextNode.valueOf(transform.apply(data.getKey()))));
    }

    static PathTrace applyNumberNodeToInt(final PathTrace path, final String params,
                                          final Function<PathTrace, Integer> transform) {
        return applyWithoutParam(path, params, paramPath -> paramPath.isNumber() || paramPath.isTextual(),
            (data, paramList) -> path.push(IntNode.valueOf(transform.apply(data.getKey()))));
    }

    static PathTrace applyTextNodeWithParamAsText(final PathTrace path, final String params,
                                                  final BiFunction<String, String, String> transform) {
        return applyWithParams(path, params, 1, 1, JsonNode::isTextual,
            (data, paramList) -> path.push(TextNode.valueOf(
                transform.apply(data.getKey().node().asText(), getNodeAsText(path, data.getValue(), paramList.get(0)))
            )));
    }

    static PathTrace applyTextNodeWithParamAsText(final PathTrace path, final String params, final boolean not,
                                                  final BiFunction<String, String, Boolean> transform) {
        return applyWithParams(path, params, 1, 1, JsonNode::isTextual,
            (data, paramList) -> path.push(BooleanNode.valueOf(
                not ^ transform.apply(data.getKey().node().asText(), getNodeAsText(path, data.getValue(), paramList.get(0)))
            )));
    }

    private static PathTrace applyAction(final PathTrace path, final String expression, final Predicate<JsonNode> isValid,
                                         final BiFunction<Pair<PathTrace, Integer>, List<String>, PathTrace> action,
                                         final List<String> paramList) {
        final PathTrace target = expression == null ? path : getPathByExpression(path, expression);
        if (target == null) {
            return null;
        }
        if (target.node().isArray()) {
            final ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < target.node().size(); i++) {
                PathTrace result = applyAction(path.push(target.node().get(i)), i, isValid, action, paramList);
                array.add(result == null ? null : result.node());
            }
            return path.push(array);
        }
        return applyAction(target, expression == null ? NON_ARRAY_INDEX : 0, isValid, action, paramList);
    }

    private static PathTrace applyAction(final PathTrace path, final int index, final Predicate<JsonNode> isValid,
                                         final BiFunction<Pair<PathTrace, Integer>, List<String>, PathTrace> action,
                                         final List<String> paramList) {
        return isValid == null || isValid.test(path.node()) ? action.apply(Pair.of(path, index), paramList) : null;
    }

    static PathTrace applyWithArrayNode(final PathTrace path, final String params, final Predicate<JsonNode> isValid,
                                        final BiFunction<PathTrace, PathTrace, PathTrace> action) {
        final PathTrace paramArray = getParamArray(path, decomposeFunctionParameters(params, 1, UNLIMITED_WITH_PATH));
        if (paramArray.node().isEmpty()) {
            return null;
        }
        if (path.node().isArray()) {
            final ArrayNode array = MAPPER.createArrayNode();
            path.node().forEach(elem -> {
                PathTrace result = applyAction(path.push(elem), isValid, action, paramArray);
                array.add(result == null ? null : result.node());
            });
            return path.push(array);
        }
        return applyAction(path, isValid, action, paramArray);
    }

    private static PathTrace applyAction(final PathTrace path, final Predicate<JsonNode> isValid,
                                         final BiFunction<PathTrace, PathTrace, PathTrace> action,
                                         final PathTrace paramArray) {
        return isValid == null || isValid.test(path.node()) ? action.apply(path, paramArray) : null;
    }
}

/*
 * Copyright 2020-2025 Choi Wai Man Raymond
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
import com.fasterxml.jackson.databind.node.IntNode;

import java.util.List;

import static com.octomix.josson.FunctionExecutor.*;
import static com.octomix.josson.Josson.readJsonNode;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Utils.*;

/**
 * Programmable functions.
 */
final class FuncProgrammable {

    private FuncProgrammable() {
    }

    static PathTrace funcCaseValue(final PathTrace path, final String params, final boolean ignoreCase) {
        return applyWithParams(path, params, 2, UNLIMITED_AND_NO_PATH, JsonNode::isValueNode,
                (data, paramList) -> {
                    final PathTrace dataPath = data.getKey();
                    final int last = paramList.size() - 1;
                    int i = 0;
                    for (; i < last; i += 2) {
                        final JsonNode node = getNodeByExpression(path, data.getValue(), paramList.get(i));
                        if (node == null || node.isNull()) {
                            if (!dataPath.isNull()) {
                                continue;
                            }
                        } else if ((node.isNumber() || node.isTextual()) && dataPath.isNumber()) {
                            if (node.asDouble() != dataPath.asDouble()) {
                                continue;
                            }
                        } else if (ignoreCase
                                ? !node.asText().equalsIgnoreCase(dataPath.asText())
                                : !node.asText().equals(dataPath.asText())) {
                            continue;
                        }
                        return getPathByExpression(path, data.getValue(), paramList.get(i + 1));
                    }
                    return i == last ? getPathByExpression(path, data.getValue(), paramList.get(i)) : null;
                });
    }

    static PathTrace funcCoalesce(final PathTrace path, final String params) {
        return applyWithParams(path, params, 1, UNLIMITED_AND_NO_PATH, null,
                (data, paramList) -> {
                    final PathTrace dataPath = data.getKey();
                    if (nodeHasValue(dataPath)) {
                        return dataPath;
                    }
                    for (String expression : paramList) {
                        final PathTrace result = getPathByExpression(path, data.getValue(), expression);
                        if (nodeIsNotNull(result)) {
                            return result;
                        }
                    }
                    return null;
                });
    }

    static PathTrace funcCycleValue(final PathTrace path, final String params) {
        return applyWithArrayNode(path, params, Utils::nodeHasValue,
                (dataPath, paramPath) -> {
                    final int size = paramPath.containerSize();
                    final int index = dataPath.asInt() % size;
                    return path.push(paramPath.get(index < 0 ? index + size : index));
                });
    }

    static PathTrace funcEval(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, JsonNode::isTextual,
                (data, paramList) -> path.push(getNodeByExpression(path, data.getKey().asText())));
    }

    static PathTrace funcGet(final PathTrace path, final String params) {
        return path.push(getPathNode(getParamPath(path, params)));
    }

    static PathTrace funcJson(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, JsonNode::isTextual,
                (data, paramList) -> {
                    try {
                        return path.push(readJsonNode(data.getKey().asText()));
                    } catch (JsonProcessingException e) {
                        throw new IllegalArgumentException(e.getMessage());
                    }
                });
    }

    static PathTrace funcIf(final PathTrace path, final String params, final boolean not) {
        return applyWithParams(path, params, 2, 3, null,
                (data, paramList) -> {
                    final PathTrace dataPath = data.getKey();
                    final PathTrace paramPath = data.getValue() < 0 ? dataPath : path;
                    final String query =
                            not ^ asBoolean(getNodeByExpression(paramPath, data.getValue(), paramList.get(0))) ? paramList.get(1)
                                    : paramList.size() > 2 ? paramList.get(2)
                                    : null;
                    return query == null ? null : getPathByExpression(paramPath, data.getValue(), query);
                });
    }

    static PathTrace funcIndexedValue(final PathTrace path, final String params) {
        return applyWithArrayNode(path, params, Utils::nodeHasValue,
                (dataPath, paramPath) -> {
                    final int index = dataPath.asInt();
                    return index >= 0 && index < paramPath.containerSize() ? path.push(paramPath.get(index)) : null;
                });
    }

    static PathTrace funcLet(final PathTrace path, final String params) {
        final List<String[]> nameAndPaths = getParamNamePath(new SyntaxDecomposer(params).deFunctionParameters(1, UNLIMITED_WITH_PATH));
        for (String[] nameAndPath : nameAndPaths) {
            final String[] evalNameAndPath = evaluateNameAndPath(nameAndPath, path, NON_ARRAY_INDEX);
            final JsonNode result = getNodeByExpression(
                    path, NON_ARRAY_INDEX, evalNameAndPath[1], evalNameAndPath[2] != null);
            path.setVariable(evalNameAndPath[0], result);
        }
        return path;
    }

    static PathTrace funcMergeArraysOption(final PathTrace path, final String params) {
        path.setMergeArraysOption(MergeArraysOption.fromValue(params));
        return path;
    }

    static PathTrace funcSteps(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, dataPath -> path.push(IntNode.valueOf(dataPath.steps())));
    }
}

/*
 * Copyright 2020-2024 Octomix Software Technology Limited
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

import java.util.List;

import static com.octomix.josson.FunctionExecutor.UNLIMITED_WITH_PATH;
import static com.octomix.josson.FunctionExecutor.getParamNamePath;
import static com.octomix.josson.JossonCore.NON_ARRAY_INDEX;
import static com.octomix.josson.JossonCore.getNodeByExpression;
import static com.octomix.josson.Utils.evaluateNameAndPath;

/**
 * Functions to change options and setup variables in Path scope.
 * All these functions are not counted as a path step
 */
final public class FuncOptions {

    private FuncOptions() {
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
}

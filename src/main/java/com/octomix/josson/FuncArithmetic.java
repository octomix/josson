/*
 * Copyright 2020-2023 Octomix Software Technology Limited
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
import org.mariuszgromada.math.mxparser.Argument;
import org.mariuszgromada.math.mxparser.Expression;

import java.util.Arrays;
import java.util.List;

import static com.octomix.josson.FuncExecutor.*;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.Utils.nodeHasValue;

/**
 * Arithmetic functions.
 */
final class FuncArithmetic {

    private FuncArithmetic() {
    }

    static PathTrace funcAbs(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, node -> node.isNumber() || node.isTextual(),
            (data, paramList) -> path.push(DoubleNode.valueOf(Math.abs(data.getKey().asDouble()))));
    }

    static PathTrace funcCalc(final PathTrace path, final String params) {
        final List<String> paramList = new SyntaxDecomposer(params).deFunctionParameters(1, UNLIMITED_WITH_PATH);
        String calc = paramList.remove(0);
        final List<String[]> nameAndPaths = getParamNamePath(paramList);
        if (calc.contains(CURRENT_NODE)) {
            calc = calc.replace(CURRENT_NODE, "_THIS_NODE_ ");
            nameAndPaths.add(new String[]{"_THIS_NODE_", CURRENT_NODE, null});
        }
        final Expression calcExpr = new Expression(calc);
        if (path.isArray()) {
            final ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < path.containerSize(); i++) {
                array.add(funcCalc(path, calcExpr, nameAndPaths, i));
            }
            return path.push(array);
        }
        return path.push(funcCalc(path, calcExpr, nameAndPaths, NON_ARRAY_INDEX));
    }

    private static DoubleNode funcCalc(final PathTrace path, final Expression calcExpr,
                                       final List<String[]> nameAndPaths, final int index) {
        calcExpr.removeAllArguments();
        for (String[] nameAndPath : nameAndPaths) {
            if (nameAndPath[1] == null) {
                continue;
            }
            final JsonNode argNode = getNodeByExpression(path, index, nameAndPath[1]);
            if (!nodeHasValue(argNode)) {
                return null;
            }
            calcExpr.addArguments(new Argument(nameAndPath[0], argNode.asDouble()));
        }
        if (!calcExpr.checkSyntax()) {
            for (String missingArg : calcExpr.getMissingUserDefinedArguments()) {
                final JsonNode argNode = getNodeByExpression(path, index, missingArg);
                if (!nodeHasValue(argNode)) {
                    return null;
                }
                calcExpr.addArguments(new Argument(missingArg, argNode.asDouble()));
            }
        }
        if (calcExpr.checkSyntax()) {
            return DoubleNode.valueOf(calcExpr.calculate());
        }
        final StringBuilder sb = new StringBuilder("Calc syntax error.");
        if (calcExpr.getMissingUserDefinedArguments().length > 0) {
            sb.append(" Missing arguments:").append(Arrays.toString(calcExpr.getMissingUserDefinedArguments()));
        }
        if (calcExpr.getMissingUserDefinedFunctions().length > 0) {
            sb.append(" Missing functions:").append(Arrays.toString(calcExpr.getMissingUserDefinedFunctions()));
        }
        if (calcExpr.getMissingUserDefinedUnits().length > 0) {
            sb.append(" Missing units:").append(Arrays.toString(calcExpr.getMissingUserDefinedUnits()));
        }
        throw new IllegalArgumentException(sb.toString());
    }

    static PathTrace funcCeil(final PathTrace path, final String params) {
        return applyNumberNodeToInt(path, params, dataPath -> (int) Math.ceil(dataPath.asDouble()));
    }

    static PathTrace funcFloor(final PathTrace path, final String params) {
        return applyNumberNodeToInt(path, params, dataPath -> (int) Math.floor(dataPath.asDouble()));
    }

    static PathTrace funcMod(final PathTrace path, final String params) {
        return applyWithParams(path, params, 1, 1, node -> node.isNumber() || node.isTextual(),
            (data, paramList) -> {
                final PathTrace dataPath = data.getKey();
                final PathTrace paramPath = data.getValue() < 0 ? dataPath : path;
                final int divisor = getNodeAsInt(paramPath, data.getValue(), paramList.get(0));
                final int result = dataPath.asInt() % divisor;
                return path.push(IntNode.valueOf(result < 0 ? result + divisor : result));
            });
    }

    static PathTrace funcRound(final PathTrace path, final String params) {
        return applyWithParams(path, params, 1, 1, node -> node.isNumber() || node.isTextual(),
            (data, paramList) -> {
                final PathTrace dataPath = data.getKey();
                final PathTrace paramPath = data.getValue() < 0 ? dataPath : path;
                final int precision = getNodeAsInt(paramPath, data.getValue(), paramList.get(0));
                final double magnitude = Math.pow(10, precision);
                final double result = Math.round(dataPath.asDouble() * magnitude) / magnitude;
                return path.push(precision > 0 ? DoubleNode.valueOf(result) : IntNode.valueOf((int) result));
            });
    }
}

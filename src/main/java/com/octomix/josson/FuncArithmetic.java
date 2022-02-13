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
import org.mariuszgromada.math.mxparser.Argument;
import org.mariuszgromada.math.mxparser.Expression;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static com.octomix.josson.FuncExecutor.*;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.PatternMatcher.decomposeFunctionParameters;

/**
 * Arithmetic functions.
 */
class FuncArithmetic {

    private FuncArithmetic() {
    }

    static JsonNode funcAbs(final JsonNode node, final String params) {
        return applyWithoutParam(node, params, jsonNode -> jsonNode.isNumber() || jsonNode.isTextual(),
                jsonNode -> DoubleNode.valueOf(Math.abs(jsonNode.asDouble())));
    }

    static JsonNode funcCalc(final JsonNode node, final String params) {
        final List<String> paramList = decomposeFunctionParameters(params, 1, -1);
        String calc = paramList.remove(0);
        final Map<String, String> args = getParamNamePath(paramList);
        if (calc.contains(CURRENT_NODE)) {
            calc = calc.replace(CURRENT_NODE, "_THIS_NODE_ ");
            args.put("_THIS_NODE_", CURRENT_NODE);
        }
        final Expression expression = new Expression(calc);
        if (node.isArray()) {
            final ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                array.add(funcCalc(node, expression, args, i));
            }
            return array;
        }
        final Double value = funcCalc(node, expression, args, -1);
        if (value == null) {
            return null;
        }
        return DoubleNode.valueOf(value);
    }

    private static Double funcCalc(final JsonNode node, final Expression expression,
                                   final Map<String, String> args, final int index) {
        expression.removeAllArguments();
        for (Map.Entry<String, String> arg : args.entrySet()) {
            final String path = arg.getValue();
            if (path == null) {
                continue;
            }
            final JsonNode tryNode = getNodeByPath(node, index, path);
            if (!nodeHasValue(tryNode)) {
                return null;
            }
            expression.addArguments(new Argument(arg.getKey(), tryNode.asDouble()));
        }
        if (!expression.checkSyntax()) {
            for (String missingArg : expression.getMissingUserDefinedArguments()) {
                final JsonNode tryNode = getNodeByPath(node, index, missingArg);
                if (!nodeHasValue(tryNode)) {
                    return null;
                }
                expression.addArguments(new Argument(missingArg, tryNode.asDouble()));
            }
        }
        if (expression.checkSyntax()) {
            return expression.calculate();
        }
        final StringBuilder sb = new StringBuilder("Calc syntax error.");
        if (expression.getMissingUserDefinedArguments().length > 0) {
            sb.append(" Missing arguments:").append(Arrays.toString(expression.getMissingUserDefinedArguments()));
        }
        if (expression.getMissingUserDefinedFunctions().length > 0) {
            sb.append(" Missing functions:").append(Arrays.toString(expression.getMissingUserDefinedFunctions()));
        }
        if (expression.getMissingUserDefinedUnits().length > 0) {
            sb.append(" Missing units:").append(Arrays.toString(expression.getMissingUserDefinedUnits()));
        }
        throw new IllegalArgumentException(sb.toString());
    }

    static JsonNode funcCeil(final JsonNode node, final String params) {
        return applyWithoutParam(node, params, jsonNode -> jsonNode.isNumber() || jsonNode.isTextual(),
                jsonNode -> IntNode.valueOf((int) Math.ceil(jsonNode.asDouble())));
    }

    static JsonNode funcFloor(final JsonNode node, final String params) {
        return applyWithoutParam(node, params, jsonNode -> jsonNode.isNumber() || jsonNode.isTextual(),
                jsonNode -> IntNode.valueOf((int) Math.floor(jsonNode.asDouble())));
    }

    static JsonNode funcMod(final JsonNode node, final String params) {
        return applyWithParamAsInt(node, params, jsonNode -> jsonNode.isNumber() || jsonNode.isTextual(),
                (jsonNode, objVar) -> {
                    final int divisor = (int) objVar;
                    final int result = jsonNode.asInt() % divisor;
                    return IntNode.valueOf(result < 0 ? result + divisor : result);
                }
        );
    }

    static JsonNode funcRound(final JsonNode node, final String params) {
        return applyWithParams(node, params, 1, 1,
                paramList -> {
                    final int precision = getNodeAsInt(node, paramList.get(0));
                    final double magnitude = Math.pow(10, precision);
                    return Pair.of(precision, magnitude);
                },
                jsonNode -> jsonNode.isNumber() || jsonNode.isTextual(),
                (jsonNode, objVar) -> {
                    final int precision = (int) ((Pair<?, ?>) objVar).getKey();
                    final double magnitude = (double) ((Pair<?, ?>) objVar).getValue();
                    final double result = Math.round(jsonNode.asDouble() * magnitude) / magnitude;
                    return precision > 0 ? DoubleNode.valueOf(result) : IntNode.valueOf((int) result);
                }
        );
    }
}

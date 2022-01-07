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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;
import org.mariuszgromada.math.mxparser.Argument;
import org.mariuszgromada.math.mxparser.Expression;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static com.octomix.josson.GetFuncParam.*;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.PatternMatcher.decomposeFunctionParameters;

class FuncArithmetic {
    static JsonNode funcAbs(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNodeByPath(node, path);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isNumber() || valueNode.isTextual()) {
                    array.add(DoubleNode.valueOf(Math.abs(valueNode.asDouble())));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!node.isNumber() && !node.isTextual()) {
            return null;
        }
        return DoubleNode.valueOf(Math.abs(node.asDouble()));
    }

    static JsonNode funcCalc(JsonNode node, String params) {
        List<String> paramList = decomposeFunctionParameters(params, 1, -1);
        String calc = paramList.remove(0);
        Map<String, String> args = getParamNamePath(paramList);
        if (calc.contains(CURRENT_NODE_PATH)) {
            calc = calc.replace(CURRENT_NODE_PATH, "_THIS_NODE_ ");
            args.put("_THIS_NODE_", CURRENT_NODE_PATH);
        }
        Expression expression = new Expression(calc);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                array.add(funcCalcElement(node, expression, args, i));
            }
            return array;
        }
        Double value = funcCalcElement(node, expression, args, -1);
        if (value == null) {
            return null;
        }
        return DoubleNode.valueOf(value);
    }

    private static Double funcCalcElement(JsonNode node, Expression expression, Map<String, String> args, int index) {
        expression.removeAllArguments();
        for (Map.Entry<String, String> arg : args.entrySet()) {
            String path = arg.getValue();
            if (path == null) {
                continue;
            }
            JsonNode tryNode = getNodeByPath(node, index, path);
            if (!nodeHasValue(tryNode)) {
                return null;
            }
            expression.addArguments(new Argument(arg.getKey(), tryNode.asDouble()));
        }
        if (!expression.checkSyntax()) {
            for (String missingArg : expression.getMissingUserDefinedArguments()) {
                JsonNode tryNode = getNodeByPath(node, index, missingArg);
                if (!nodeHasValue(tryNode)) {
                    return null;
                }
                expression.addArguments(new Argument(missingArg, tryNode.asDouble()));
            }
        }
        if (expression.checkSyntax()) {
            return expression.calculate();
        }
        StringBuilder sb = new StringBuilder("Calc syntax error.");
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

    static JsonNode funcCeil(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNodeByPath(node, path);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isNumber() || valueNode.isTextual()) {
                    array.add(IntNode.valueOf((int) Math.ceil(valueNode.asDouble())));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!node.isNumber() && !node.isTextual()) {
            return null;
        }
        return IntNode.valueOf((int) Math.ceil(node.asDouble()));
    }

    static JsonNode funcFloor(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNodeByPath(node, path);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isNumber() || valueNode.isTextual()) {
                    array.add(IntNode.valueOf((int) Math.floor(valueNode.asDouble())));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!node.isNumber() && !node.isTextual()) {
            return null;
        }
        return IntNode.valueOf((int) Math.floor(node.asDouble()));
    }

    static JsonNode funcMod(JsonNode node, String params) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        int divisor = Integer.parseInt(getNodeAsText(node, pathAndParams.getValue().get(0)));
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isNumber() || valueNode.isTextual()) {
                    int result = valueNode.asInt() % divisor;
                    array.add(IntNode.valueOf(result < 0 ? result + divisor : result));
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!node.isNumber() && !node.isTextual()) {
            return null;
        }
        int result = node.asInt() % divisor;
        return IntNode.valueOf(result < 0 ? result + divisor : result);
    }

    static JsonNode funcRound(JsonNode node, String params) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 0, 1);
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return null;
            }
        }
        int precision = pathAndParams.getValue().size() > 0 ?
                Integer.parseInt(getNodeAsText(node, pathAndParams.getValue().get(0))) : 0;
        double magnitude = Math.pow(10, precision);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isNumber() || valueNode.isTextual()) {
                    double result = Math.round(valueNode.asDouble() * magnitude) / magnitude;
                    if (precision > 0) {
                        array.add(DoubleNode.valueOf(result));
                    } else {
                        array.add(IntNode.valueOf((int) result));
                    }
                } else {
                    array.addNull();
                }
            }
            return array;
        }
        if (!node.isNumber() && !node.isTextual()) {
            return null;
        }
        double result = Math.round(node.asDouble() * magnitude) / magnitude;
        if (precision > 0) {
            return DoubleNode.valueOf(result);
        }
        return IntNode.valueOf((int) result);
    }
}

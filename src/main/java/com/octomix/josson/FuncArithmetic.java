package com.octomix.josson;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.mariuszgromada.math.mxparser.Argument;
import org.mariuszgromada.math.mxparser.Expression;

import java.util.Arrays;
import java.util.List;

import static com.octomix.josson.GetFuncParam.*;
import static com.octomix.josson.Josson.getNode;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.PatternMatcher.decomposeFunctionParameters;

class FuncArithmetic {
    static ValueNode funcAggregate(JsonNode node, String funcId, String params) {
        List<String> paramList = decomposeFunctionParameters(params, 0, 1);
        if (!node.isArray()) {
            return null;
        }
        String path = paramList.size() > 0 ? paramList.get(0) : null;
        double sum = 0;
        int count = 0;
        for (int i = 0; i < node.size(); i++) {
            JsonNode tryNode = getNode(node.get(i), path);
            if (tryNode != null && !tryNode.isNull() && tryNode.isValueNode()) {
                double value = tryNode.asDouble();
                sum += value;
                count++;
            }
        }
        if ("count".equals(funcId)) {
            return new IntNode(count);
        }
        if (count > 0) {
            switch (funcId) {
                case "sum":
                    return new DoubleNode(sum);
                case "avg":
                    return new DoubleNode(sum / count);
            }
        }
        return null;
    }

    static JsonNode funcAbs(JsonNode node, String params) {
        getParamNotAccept(params);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isNumber() || valueNode.isTextual()) {
                    array.add(DoubleNode.valueOf(Math.abs(valueNode.asDouble())));
                }
            }
            return array;
        } else if (!node.isNumber() && !node.isTextual()) {
            return null;
        }
        return DoubleNode.valueOf(Math.abs(node.asDouble()));
    }

    static JsonNode funcCalc(JsonNode node, String params) {
        List<String> paramList = decomposeFunctionParameters(params, 1, -1);
        Expression expression = new Expression(paramList.get(0));
        paramList.remove(0);
        List<ImmutablePair<String, String>> args = getParamNamePath(paramList);
        if (!node.isArray()) {
            Double value = funcCalcElement(node, expression, args, 0);
            if (value == null) {
                return null;
            }
            return DoubleNode.valueOf(value);
        }
        ArrayNode array = MAPPER.createArrayNode();
        for (int i  = 0; i < node.size(); i++) {
            Double value = funcCalcElement(node.get(i), expression, args, i);
            if (value != null) {
                array.add(value);
            }
        }
        return array;
    }

    static Double funcCalcElement(JsonNode node, Expression expression,
                                  List<ImmutablePair<String, String>> args, int index) {
        expression.removeAllArguments();
        for (ImmutablePair<String, String> arg : args) {
            if (arg.right == null) {
                continue;
            }
            double value;
            if (arg.right.charAt(0) == '\'') {
                value = Double.parseDouble(unquoteString(arg.right));
            } else if ("#".equals(arg.right)) {
                value = index + 1;
            } else if ("?".equals(arg.right)) {
                if (node.isNull() || !node.isValueNode()) {
                    return null;
                }
                value = node.asDouble();
            } else {
                JsonNode tryNode = getNode(node, arg.right);
                if (tryNode == null || tryNode.isNull() || !tryNode.isValueNode()) {
                    return null;
                }
                value = tryNode.asDouble();
            }
            expression.addArguments(new Argument(arg.left, value));
        }
        if (!expression.checkSyntax()) {
            for (String missingArg : expression.getMissingUserDefinedArguments()) {
                JsonNode tryNode = getNode(node, missingArg);
                if (tryNode == null || tryNode.isNull() || !tryNode.isValueNode()) {
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
        getParamNotAccept(params);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isNumber() || valueNode.isTextual()) {
                    array.add(DoubleNode.valueOf(Math.ceil(valueNode.asDouble())));
                }
            }
            return array;
        } else if (!node.isNumber() && !node.isTextual()) {
            return null;
        }
        return DoubleNode.valueOf(Math.ceil(node.asDouble()));
    }

    static JsonNode funcFloor(JsonNode node, String params) {
        getParamNotAccept(params);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isNumber() || valueNode.isTextual()) {
                    array.add(DoubleNode.valueOf(Math.floor(valueNode.asDouble())));
                }
            }
            return array;
        } else if (!node.isNumber() && !node.isTextual()) {
            return null;
        }
        return DoubleNode.valueOf(Math.floor(node.asDouble()));
    }

    static JsonNode funcMod(JsonNode node, String params) {
        List<String> paramList = decomposeFunctionParameters(params, 1, 1);
        int divisor = Integer.parseInt(paramList.get(0));
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isNumber() || valueNode.isTextual()) {
                    int result = valueNode.asInt() % divisor;
                    array.add(IntNode.valueOf(result < 0 ? result + divisor : result));
                }
            }
            return array;
        } else if (!node.isNumber() && !node.isTextual()) {
            return null;
        }
        int result = node.asInt() % divisor;
        return IntNode.valueOf(result < 0 ? result + divisor : result);
    }

    static JsonNode funcRound(JsonNode node, String params) {
        List<String> paramList = decomposeFunctionParameters(params, 0, 1);
        int precision = paramList.size() > 0 ? Integer.parseInt(paramList.get(0)) : 0;
        double magnitude = Math.pow(10, precision);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isNumber() || valueNode.isTextual()) {
                    array.add(DoubleNode.valueOf(Math.round(valueNode.asDouble() * magnitude) / magnitude));
                }
            }
            return array;
        } else if (!node.isNumber() && !node.isTextual()) {
            return null;
        }
        return DoubleNode.valueOf(Math.round(node.asDouble() * magnitude) / magnitude);
    }
}

package com.octomix.josson;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;
import org.mariuszgromada.math.mxparser.Argument;
import org.mariuszgromada.math.mxparser.Expression;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static com.octomix.josson.GetFuncParam.*;
import static com.octomix.josson.Josson.getNode;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.PatternMatcher.decomposeFunctionParameters;

class FuncArithmetic {
    static ValueNode funcAggregate(JsonNode node, String funcId, String params) {
        ArrayNode array = getParamArrayOrItself(params, node);
        if (array == null) {
            return null;
        }
        double sum = 0;
        int count = 0;
        for (int i = array.size() - 1; i >= 0; i--) {
            JsonNode tryNode = array.get(i);
            if (nodeHasValue(tryNode)) {
                sum += tryNode.asDouble();
                count++;
            }
        }
        if ("count".equals(funcId)) {
            return IntNode.valueOf(count);
        }
        if (count > 0) {
            switch (funcId) {
                case "sum":
                    return DoubleNode.valueOf(sum);
                case "avg":
                    return DoubleNode.valueOf(sum / count);
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
        if (calc.contains("?")) {
            calc = calc.replaceAll("\\?", "_THIS_NODE_ ");
            args.put("_THIS_NODE_", "?");
        }
        Expression expression = new Expression(calc);
        if (!node.isArray()) {
            Double value = funcCalcElement(node, expression, args, 0);
            if (value == null) {
                return null;
            }
            return DoubleNode.valueOf(value);
        }
        ArrayNode array = MAPPER.createArrayNode();
        for (int i  = 0; i < node.size(); i++) {
            Double value = funcCalcElement(node.get(i), expression, args, array.size());
            if (value != null) {
                array.add(value);
            }
        }
        return array;
    }

    private static Double funcCalcElement(JsonNode node, Expression expression, Map<String, String> args, int index) {
        expression.removeAllArguments();
        for (Map.Entry<String, String> arg : args.entrySet()) {
            String path = arg.getValue();
            if (path == null) {
                continue;
            }
            double value;
            if ("?".equals(path)) {
                value = node.asDouble();
            } else {
                JsonNode tryNode = path.charAt(0) == '#' ? getIndexNode(index, path) : getNode(node, path);
                if (!nodeHasValue(tryNode)) {
                    return null;
                }
                value = tryNode.asDouble();
            }
            expression.addArguments(new Argument(arg.getKey(), value));
        }
        if (!expression.checkSyntax()) {
            for (String missingArg : expression.getMissingUserDefinedArguments()) {
                JsonNode tryNode = getNode(node, missingArg);
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
        getParamNotAccept(params);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isNumber() || valueNode.isTextual()) {
                    array.add(IntNode.valueOf((int) Math.ceil(valueNode.asDouble())));
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
        getParamNotAccept(params);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode valueNode = node.get(i);
                if (valueNode.isNumber() || valueNode.isTextual()) {
                    array.add(IntNode.valueOf((int) Math.floor(valueNode.asDouble())));
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
        }
        if (!node.isNumber() && !node.isTextual()) {
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
                    double result = Math.round(valueNode.asDouble() * magnitude) / magnitude;
                    if (precision > 0) {
                        array.add(DoubleNode.valueOf(result));
                    } else {
                        array.add(IntNode.valueOf((int) result));
                    }
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

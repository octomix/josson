package com.octomix.josson;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.mariuszgromada.math.mxparser.Argument;
import org.mariuszgromada.math.mxparser.Expression;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;

import static com.octomix.josson.GetFuncParam.*;
import static com.octomix.josson.Josson.getNode;
import static com.octomix.josson.JossonCore.*;

class FuncArithmetic {
    static ValueNode funcAggregate(JsonNode node, String funcId, String params) {
        String path = null;
        Matcher m = DECOMPOSE_PARAMETERS.matcher(params);
        if (m.find()) {
            path = m.group(0);
            getParamNoMore(m);
        }
        if (!node.isArray()) {
            return null;
        }
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
        Matcher m = DECOMPOSE_PARAMETERS.matcher(params);
        getParamFindNextRequired(m);
        String expression = m.group(0).trim();
        if (expression.length() > 1 && expression.charAt(0) != '\'') {
            throw new UnsupportedOperationException("First argument must be string literal");
        }
        expression = unquoteString(expression);
        List<ImmutablePair<String, String>> args = getParamNamePath(params.substring(m.end() + 1));
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

    static Double funcCalcElement(JsonNode node, String expression, List<ImmutablePair<String, String>> args, int index) {
        List<Argument> arguments = new ArrayList<>();
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
                value = node.asDouble();
            } else {
                JsonNode tryNode = getNode(node, arg.right);
                if (tryNode == null || tryNode.isNull() || !tryNode.isValueNode()) {
                    return null;
                }
                value = tryNode.asDouble();
            }
            arguments.add(new Argument(arg.left, value));
        }
        Argument[] a = new Argument[arguments.size()];
        return new Expression(expression, arguments.toArray(a)).calculate();
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

    static JsonNode funcRound(JsonNode node, String params) {
        Matcher m = DECOMPOSE_PARAMETERS.matcher(params);
        int precision = 0;
        if (m.find()) {
            precision = StringUtils.isBlank(m.group(0)) ? 0 : Integer.parseInt(m.group(0));
            getParamNoMore(m);
        }
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

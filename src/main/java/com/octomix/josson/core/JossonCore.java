package com.octomix.josson.core;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;
import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.octomix.josson.core.FuncDispatcher.funcDispatcher;

public class JossonCore {

    static final Mapper MAPPER = new Mapper();
    static final Pattern IS_ARRAY_NODE_QUERY = Pattern.compile(
            "^([^\\[]*)\\[([^]]*)]\\s*(@|\\[\\s*@\\s*])?(.*)$", Pattern.DOTALL);
    static final Pattern IS_FUNCTION_PATTERN = Pattern.compile(
            "^([^\\[(]+)\\((.*)\\)\\s*$", Pattern.DOTALL);
    static final Pattern DECOMPOSE_PATH = Pattern.compile(
            "(?:[^(\\['.]+|'(?:'{2}|[^']+)*'|(?:(?=\\()(?:(?=(?>'.*?'|.)*?\\((?!.*?\\1)(.*\\)(?!.*\\2).*))(?=(?>'.*?'|.)*?\\)(?!.*?\\2)(.*)).)+?.*?(?=\\1)(?>'.*?'|[^(])*(?=\\2$))|(?:(?=\\[)(?:(?=(?>'.*?'|.)*?\\[(?!.*?\\3)(.*](?!.*\\4).*))(?=(?>'.*?'|.)*?](?!.*?\\4)(.*)).)+?.*?(?=\\3)(?>'.*?'|[^\\[])*(?=\\4$)))+(?=\\.|$)", Pattern.DOTALL);
    static final Pattern DECOMPOSE_PARAMETERS = Pattern.compile(
            "(?<=^|,)(?:[^(\\[',]+|'(?:'{2}|[^']+)*'|(?:(?=\\()(?:(?=(?>'.*?'|.)*?\\((?!.*?\\1)(.*\\)(?!.*\\2).*))(?=(?>'.*?'|.)*?\\)(?!.*?\\2)(.*)).)+?.*?(?=\\1)(?>'.*?'|[^(])*(?=\\2$))|(?:(?=\\[)(?:(?=(?>'.*?'|.)*?\\[(?!.*?\\3)(.*](?!.*\\4).*))(?=(?>'.*?'|.)*?](?!.*?\\4)(.*)).)+?.*?(?=\\3)(?>'.*?'|[^\\[])*(?=\\4$)))*(?=,|$)", Pattern.DOTALL);
    private static final Pattern DECOMPOSE_NESTED_RESULT_FUNCTIONS = Pattern.compile(
            "(([^(]+)\\((?:[^(\\[')]+|'(?:'{2}|[^']+)*'|(?:(?=\\()(?:(?=(?>'.*?'|.)*?\\((?!.*?\\3)(.*\\)(?!.*\\4).*))(?=(?>'.*?'|.)*?\\)(?!.*?\\4)(.*)).)+?.*?(?=\\3)(?>'.*?'|[^(])*(?=\\4$))|(?:(?=\\[)(?:(?=(?>'.*?'|.)*?\\[(?!.*?\\5)(.*](?!.*\\6).*))(?=(?>'.*?'|.)*?](?!.*?\\6)(.*)).)+?.*?(?=\\5)(?>'.*?'|[^\\[])*(?=\\6$)))*\\))\\s*(?:@|$)", Pattern.DOTALL);
    private static final Pattern DECOMPOSE_CONDITIONS = Pattern.compile(
            "\\s*([=!<>&]*)((?:[^=!<>&(\\[']+|'(?:'{2}|[^']+)*'|(?:(?=\\()(?:(?=(?>'.*?'|.)*?\\((?!.*?\\3)(.*\\)(?!.*\\4).*))(?=(?>'.*?'|.)*?\\)(?!.*?\\4)(.*)).)+?.*?(?=\\3)(?>'.*?'|[^(])*(?=\\4$))|(?:(?=\\[)(?:(?=(?>'.*?'|.)*?\\[(?!.*?\\5)(.*](?!.*\\6).*))(?=(?>'.*?'|.)*?](?!.*?\\6)(.*)).)+?.*?(?=\\5)(?>'.*?'|[^\\[])*(?=\\6$)))+)", Pattern.DOTALL);

    enum FilterArrayReturn {
        FIRST_MATCHED,
        ALL_MATCHED
    }

    public static ObjectNode createObjectNode() {
        return MAPPER.createObjectNode();
    }

    public static ArrayNode createArrayNode() {
        return MAPPER.createArrayNode();
    }

    public static JsonNode readJsonNode(Object object) {
        return MAPPER.valueToTree(object);
    }

    public static ObjectNode readObjectNode(Object object) {
        JsonNode node = MAPPER.valueToTree(object);
        if (node.isObject()) {
            return (ObjectNode) node;
        }
        throw new IllegalArgumentException("The provided object is not an object node");
    }

    public static ArrayNode readArrayNode(Object object) {
        JsonNode node = MAPPER.valueToTree(object);
        if (node.isArray()) {
            return (ArrayNode) node;
        }
        throw new IllegalArgumentException("The provided object is not an array node");
    }

    public static JsonNode readJsonNode(String json) throws JsonProcessingException {
        if (json == null) {
            return null;
        }
        return MAPPER.readTree(json);
    }

    public static <T> T readValue(String json, Class<T> valueType) throws JsonProcessingException {
        return MAPPER.readValue(json, valueType);
    }

    public static <T> T readValue(File file, Class<T> valueType) throws IOException {
        return MAPPER.readValue(file, valueType);
    }

    public static <T> T convertValue(JsonNode node) {
        return MAPPER.convertValue(node, new TypeReference<T>(){});
    }

    public static String toJsonString(Object object) {
        try {
            return MAPPER.writeValueAsString(object);
        } catch (JsonProcessingException e) {
            return MAPPER.valueToTree(object).toString();
        }
    }

    public static String toJsonPretty(Object object) {
        try {
            return MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(object);
        } catch (JsonProcessingException e) {
            return MAPPER.valueToTree(object).toPrettyString();
        }
    }

    public static JsonNode getNode(JsonNode node, String path) {
        if (StringUtils.isBlank(path)) {
            return node;
        }
        List<String> keys = new ArrayList<>();
        Matcher m = DECOMPOSE_PATH.matcher(path);
        while (m.find()) {
            if (!StringUtils.isBlank(m.group(0))) {
                keys.add(m.group(0));
            }
        }
        if (keys.isEmpty()) {
            return node;
        }
        String firstKey = keys.get(0).trim();
        if (firstKey.equalsIgnoreCase("null")) {
            node = NullNode.getInstance();
        } else if (firstKey.equalsIgnoreCase("true")) {
            node = BooleanNode.TRUE;
        } else if (firstKey.equalsIgnoreCase("false")) {
            node = BooleanNode.FALSE;
        } else if (firstKey.charAt(0) == '\'') {
            if (firstKey.charAt(firstKey.length() - 1) == '\'') {
                node = TextNode.valueOf(unquoteString(firstKey));
            } else {
                throw new IllegalArgumentException(path);
            }
        } else {
            try {
                node = new DoubleNode(Double.parseDouble(firstKey));
            } catch (NumberFormatException e) {
                return getNode(node, keys);
            }
        }
        keys.remove(0);
        return getNode(node, keys);
    }

    public static JsonNode getNode(JsonNode node, List<String> keys) {
        if (keys == null || keys.isEmpty()) {
            return node;
        }
        if (node == null || node.isNull()) {
            return null;
        }
        String key = keys.get(0).trim();
        Matcher m = IS_FUNCTION_PATTERN.matcher(key);
        if (m.find()) {
            node = funcDispatcher(node, m.group(1).trim(), m.group(2));
        } else if (node.isValueNode()) {
            return null;
        } else {
            m = IS_ARRAY_NODE_QUERY.matcher(key);
            if (m.find()) {
                String field = m.group(1).trim();
                FilterArrayReturn mode = m.group(3) == null
                        ? FilterArrayReturn.FIRST_MATCHED : FilterArrayReturn.ALL_MATCHED;
                node = filterArrayNode(field.isEmpty() ? node : node.get(field), m.group(2), mode);
                if (node != null && node.isArray()) {
                    keys.remove(0);
                    return forEachElement(node, keys, m.group(4), "@".equals(m.group(3)));
                }
            } else if (node.isArray()) {
                return forEachElement(node, keys, "", true);
            } else {
                node = node.get(key);
                if (node != null && node.isArray()) {
                    keys.remove(0);
                    return forEachElement(node, keys, "", true);
                }
            }
        }
        keys.remove(0);
        return getNode(node, keys);
    }

    static String unquoteString(String quotedString) {
        return quotedString.substring(1, quotedString.length() - 1)
                .replaceAll("''", "'");
    }

    /**
     * Find an element or filtered array in an array node
     *
     * @param arrayNode A json array node
     * @param statement Multiple conditions combined with "&" relational operator
     *                  Pattern: stringNode='string' & numberNode=0 & booleanNode=true & ... & valueNode=max
     *                  Supported operators: =, !=, >, >=, <, <=
     *                  Special operands:
     *                    =null, !=null, =max, =min, =maxNull, =minNull  -- max/min must be the last condition
     *                    =(value, value...), !=(value, value...)        -- like IN (...) and NOT IN (...)
     * @param mode      FIRST_MATCHED - return the 1st matched element
     *                  ALL_MATCHED - return all matched elements in array node
     * @return matched element node or matched elements in array node
     */
    private static JsonNode filterArrayNode(JsonNode arrayNode, String statement, FilterArrayReturn mode) {
        if (arrayNode == null || !arrayNode.isArray() || arrayNode.size() == 0) {
            return null;
        }
        statement = statement.trim();
        if (statement.isEmpty()) {
            if (FilterArrayReturn.FIRST_MATCHED == mode) {
                return arrayNode.get(0);
            }
            return arrayNode;
        }
        ArrayNode matchedNodes = null;
        if (FilterArrayReturn.FIRST_MATCHED != mode) {
            matchedNodes = MAPPER.createArrayNode();
        }
        try {
            if (FilterArrayReturn.FIRST_MATCHED == mode) {
                return arrayNode.get(Integer.parseInt(statement));
            }
            matchedNodes.add(arrayNode.get(Integer.parseInt(statement)));
            return matchedNodes;
        } catch (NumberFormatException e) {
            // continue
        }
        int maxMinIndex = -1;
        String maxMinString = null;
        Double maxMinDouble = null;
        for (int i = 0; i < arrayNode.size(); i++) {
            JsonNode valueNode = null;
            Matcher m = DECOMPOSE_CONDITIONS.matcher(statement);
            while (m.find()) {
                String expression = m.group(2).trim();
                if (expression.isEmpty()) {
                    throw new IllegalArgumentException(statement);
                }
                String operator = m.group(1);
                if (valueNode != null && operator.equals("&") && !valueNode.asBoolean()) {
                    valueNode = null;
                    break;
                }
                switch (operator) {
                    case "&":
                    case "":
                        valueNode = expression.equals("?") ?
                                arrayNode.get(i) :
                                expression.startsWith("@") ?
                                        getNode(arrayNode, expression.substring(1)) :
                                        getNode(arrayNode.get(i), expression);
                        continue;
                }
                if (expression.startsWith("(")) {
                    if (!(operator.equals("=") || operator.equals("!="))) {
                        throw new IllegalArgumentException(statement);
                    }
                    boolean matched = false;
                    if (valueNode == null || !valueNode.isValueNode()) {
                        break;
                    }
                    String text = valueNode.asText();
                    Matcher m2 = DECOMPOSE_PARAMETERS.matcher(expression.substring(1, expression.length()-1));
                    while (!matched && m2.find()) {
                        String valueToCheck = m2.group(0).trim();
                        try {
                            matched = valueToCheck.length() > 1 && valueToCheck.charAt(0) == '\''
                                    ? valueToCheck.substring(1, valueToCheck.length() - 1).equals(text)
                                    : Double.parseDouble(valueToCheck) == Double.parseDouble(text);
                        } catch (NumberFormatException e) {
                            // ignore
                        }
                    }
                    valueNode = BooleanNode.valueOf(operator.equals("=") == matched);
                    continue;
                }
                if ("max".equalsIgnoreCase(expression) || "maxNull".equalsIgnoreCase(expression)) {
                    if (!operator.equals("=")) {
                        throw new IllegalArgumentException(statement);
                    }
                    if (valueNode == null || valueNode.isNull()) {
                        if ("maxNull".equalsIgnoreCase(expression)) {
                            if (maxMinIndex < 0) {
                                maxMinIndex = i;
                            }
                            break;
                        }
                    } else {
                        if (valueNode.isNumber()) {
                            if (maxMinDouble == null || valueNode.asDouble() > maxMinDouble) {
                                maxMinIndex = i;
                                maxMinDouble = valueNode.asDouble();
                            }
                        } else if (valueNode.isValueNode() && !valueNode.isNull()) {
                            if (maxMinString == null || valueNode.asText().compareTo(maxMinString) > 0) {
                                maxMinIndex = i;
                                maxMinString = valueNode.asText();
                            }
                        }
                        valueNode = null;
                        break;
                    }
                } else if ("min".equalsIgnoreCase(expression) || "minNull".equalsIgnoreCase(expression)) {
                    if (!operator.equals("=")) {
                        throw new IllegalArgumentException(statement);
                    }
                    if (valueNode == null || valueNode.isNull()) {
                        if ("minNull".equalsIgnoreCase(expression)) {
                            if (maxMinIndex < 0) {
                                maxMinIndex = i;
                            }
                            break;
                        }
                    } else {
                        if (valueNode.isNumber()) {
                            if (maxMinDouble == null || valueNode.asDouble() < maxMinDouble) {
                                maxMinIndex = i;
                                maxMinDouble = valueNode.asDouble();
                            }
                        } else if (valueNode.isValueNode() && !valueNode.isNull()) {
                            if (maxMinString == null || valueNode.asText().compareTo(maxMinString) < 0) {
                                maxMinIndex = i;
                                maxMinString = valueNode.asText();
                            }
                        }
                        valueNode = null;
                        break;
                    }
                }
                JsonNode compareToNode = expression.equals("?") ?
                        arrayNode.get(i) :
                        expression.startsWith("@") ?
                                getNode(arrayNode, expression.substring(1)) :
                                getNode(arrayNode.get(i), expression);
                if (valueNode == null) {
                    valueNode = NullNode.getInstance();
                }
                if (compareToNode == null) {
                    compareToNode = NullNode.getInstance();
                }
                if (compareToNode.isTextual()) {
                    if (valueNode.isTextual()) {
                        int compareResult = valueNode.asText().compareTo(compareToNode.asText());
                        switch (operator) {
                            case "=":
                                valueNode = BooleanNode.valueOf(compareResult == 0);
                                break;
                            case "!=":
                                valueNode = BooleanNode.valueOf(compareResult != 0);
                                break;
                            case ">":
                                valueNode = BooleanNode.valueOf(compareResult > 0);
                                break;
                            case ">=":
                                valueNode = BooleanNode.valueOf(compareResult >= 0);
                                break;
                            case "<":
                                valueNode = BooleanNode.valueOf(compareResult < 0);
                                break;
                            case "<=":
                                valueNode = BooleanNode.valueOf(compareResult <= 0);
                                break;
                            default:
                                throw new IllegalArgumentException(statement);
                        }
                        continue;
                    }
                    JsonNode swap = valueNode;
                    valueNode = compareToNode;
                    compareToNode = swap;
                    switch (operator) {
                        case ">":
                            operator = "<";
                            break;
                        case ">=":
                            operator = "<=";
                            break;
                        case "<":
                            operator = ">";
                            break;
                        case "<=":
                            operator = ">=";
                            break;
                    }
                }
                if (!valueNode.isContainerNode() && compareToNode.isNumber()) {
                    try {
                        double value = valueNode.isNumber() ? valueNode.asDouble() : Double.parseDouble(valueNode.asText());
                        switch (operator) {
                            case "=":
                                valueNode = BooleanNode.valueOf(value == compareToNode.asDouble());
                                break;
                            case "!=":
                                valueNode = BooleanNode.valueOf(value != compareToNode.asDouble());
                                break;
                            case ">":
                                valueNode = BooleanNode.valueOf(value > compareToNode.asDouble());
                                break;
                            case ">=":
                                valueNode = BooleanNode.valueOf(value >= compareToNode.asDouble());
                                break;
                            case "<":
                                valueNode = BooleanNode.valueOf(value < compareToNode.asDouble());
                                break;
                            case "<=":
                                valueNode = BooleanNode.valueOf(value <= compareToNode.asDouble());
                                break;
                            default:
                                throw new IllegalArgumentException(statement);
                        }
                    } catch (NumberFormatException e) {
                        valueNode = BooleanNode.FALSE;
                    }
                } else if (!valueNode.isContainerNode() && compareToNode.isBoolean()) {
                    switch (operator) {
                        case "=":
                            valueNode = BooleanNode.valueOf(!valueNode.asBoolean() ^ compareToNode.asBoolean());
                            break;
                        case "!=":
                            valueNode = BooleanNode.valueOf(valueNode.asBoolean() ^ compareToNode.asBoolean());
                            break;
                        case ">":
                        case ">=":
                        case "<":
                        case "<=":
                            valueNode = BooleanNode.FALSE;
                            break;
                        default:
                            throw new IllegalArgumentException(statement);
                    }
                } else {
                    switch (operator) {
                        case "=":
                            valueNode = BooleanNode.valueOf(valueNode.isNull() && compareToNode.isNull());
                            break;
                        case "!=":
                            valueNode = BooleanNode.valueOf(valueNode.isNull() ^ compareToNode.isNull());
                            break;
                        case ">":
                        case ">=":
                        case "<":
                        case "<=":
                            valueNode = BooleanNode.FALSE;
                            break;
                        default:
                            throw new IllegalArgumentException(statement);
                    }
                }
            }
            if (valueNode != null && valueNode.asBoolean()) {
                if (FilterArrayReturn.FIRST_MATCHED == mode) {
                    return arrayNode.get(i);
                }
                matchedNodes.add(arrayNode.get(i));
            }
        }
        if (FilterArrayReturn.FIRST_MATCHED == mode) {
            return maxMinIndex >= 0 ? arrayNode.get(maxMinIndex) : null;
        }
        if (maxMinIndex >= 0) {
            matchedNodes.add(arrayNode.get(maxMinIndex));
        }
        return matchedNodes;
    }

    private static JsonNode forEachElement(JsonNode node, List<String> keys, String funcChain, boolean flattenArray) {
        List<String> functions = new ArrayList<>();
        Matcher m = DECOMPOSE_NESTED_RESULT_FUNCTIONS.matcher(funcChain);
        while (m.find()) {
            functions.add(m.group(1));
        }
        ArrayNode matchedNodes = MAPPER.createArrayNode();
        for (int i = 0; i < node.size(); i++) {
            JsonNode tryNode = getNode(node.get(i), new ArrayList<>(keys));
            if (tryNode != null) {
                if (tryNode.isArray() && flattenArray) {
                    matchedNodes.addAll((ArrayNode) tryNode);
                } else {
                    matchedNodes.add(tryNode);
                }
            }
        }
        return getNode(matchedNodes, functions);
    }
}

package com.octomix.josson;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;
import com.octomix.josson.core.JossonCore;
import com.octomix.josson.exception.NoValuePresentException;
import com.octomix.josson.exception.UnresolvedDataSourceException;
import org.apache.commons.lang3.StringUtils;

import java.util.*;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.octomix.josson.ResolverProgress.ShowResolvedValueMode;
import static org.apache.commons.text.StringEscapeUtils.unescapeXml;

public class Jossons {

    private static final Pattern SEPARATE_XML_TAGS = Pattern.compile(
        "((?:<[^>]*>)*)([^<]*)");
    private static final Pattern IS_DB_QUERY = Pattern.compile(
        "^([^\\[?]*)(\\[\\s*])?\\s*\\?\\s*(\\{.*})\\s*$", Pattern.DOTALL);
    private static final Pattern IS_JSON_QUERY = Pattern.compile(
        "^([^-<\\[]*)->(.*)", Pattern.DOTALL);
    private static final Pattern DECOMPOSE_IIF_ELSE = Pattern.compile(
        "(?<=^|:)((?:[^?:(']+|'(?:'{2}|[^']+)*'|(?:(?=\\()(?:(?=(?>'.*?'|.)*?\\((?!.*?\\2)(.*\\)(?!.*\\3).*))(?=(?>'.*?'|.)*?\\)(?!.*?\\3)(.*)).)+?.*?(?=\\2)(?>'.*?'|[^(])*(?=\\3$)))+)(?:\\?((?:[^:(']+|'(?:'{2}|[^']+)*'|(?:(?=\\()(?:(?=(?>'.*?'|.)*?\\((?!.*?\\5)(.*\\)(?!.*\\6).*))(?=(?>'.*?'|.)*?\\)(?!.*?\\6)(.*)).)+?.*?(?=\\5)(?>'.*?'|[^(])*(?=\\6$)))+)?\\s*)?(?=:|$)", Pattern.DOTALL);
    private static final Pattern DECOMPOSE_CONDITIONS = Pattern.compile(
        "\\s*([=!<>&]*)([^=!<>&(\\[']*(?:->)?\\s*(?:[^=!<>&(\\[']+|'(?:'{2}|[^']+)*'|(?:(?=\\()(?:(?=(?>'.*?'|.)*?\\((?!.*?\\3)(.*\\)(?!.*\\4).*))(?=(?>'.*?'|.)*?\\)(?!.*?\\4)(.*)).)+?.*?(?=\\3)(?>'.*?'|[^(])*(?=\\4$))|(?:(?=\\[)(?:(?=(?>'.*?'|.)*?\\[(?!.*?\\5)(.*](?!.*\\6).*))(?=(?>'.*?'|.)*?](?!.*?\\6)(.*)).)+?.*?(?=\\5)(?>'.*?'|[^\\[])*(?=\\6$)))+)", Pattern.DOTALL);

    private final Map<String, Josson> dataSources = new HashMap<>();
    private ShowResolvedValueMode showResolvedValueMode = ShowResolvedValueMode.VALUE_NODE_ONLY;

    public static Jossons create(JsonNode dataSources) {
        if (dataSources != null && dataSources.getNodeType() != JsonNodeType.OBJECT) {
            throw new IllegalArgumentException("Argument is not an object node");
        }
        Jossons jossons = new Jossons();
        if (dataSources != null) {
            dataSources.fields().forEachRemaining(entry ->
                jossons.dataSources.put(entry.getKey(), Josson.create(entry.getValue())));
        }
        return jossons;
    }

    public static Jossons fromJsonString(String json) throws JsonProcessingException {
        return create(JossonCore.readJsonNode(json));
    }

    public static Jossons fromMap(Map<String, String> textParams) {
        Jossons jossons = new Jossons();
        if (textParams != null) {
            textParams.forEach((key, value) ->
                jossons.dataSources.put(key, Josson.fromText(value)));
        }
        return jossons;
    }

    private static String unquoteString(String quotedString) {
        return quotedString.substring(1, quotedString.length() - 1)
            .replaceAll("''", "'");
    }

    public Jossons putDataSource(String key, Josson value) {
        dataSources.put(key, value);
        return this;
    }

    public Map<String, Josson> getDataSources() {
        return dataSources;
    }

    public ShowResolvedValueMode getProgressShowResolvedValueMode() {
        return showResolvedValueMode;
    }

    public void setProgressShowResolvedValueMode(ShowResolvedValueMode mode) {
        showResolvedValueMode = mode;
    }

    private String showProgressResolvedValue(JsonNode node) {
        if (showResolvedValueMode == ShowResolvedValueMode.VALUE_NODE_ONLY) {
            if (node.isArray()) {
                return "Array with " + node.size() + " elements";
            }
            if (node.isObject()) {
                return "Object with " + node.size() + " elements";
            }
        }
        if (node.isTextual()) {
            return '"' + node.asText() + '"';
        }
        return node.toString();
    }

    public String fillInXmlPlaceholder(String content) throws NoValuePresentException {
        return fillInPlaceholderLoop(content, true);
    }

    public String fillInPlaceholder(String content) throws NoValuePresentException {
        return fillInPlaceholderLoop(content, false);
    }

    private String fillInPlaceholderLoop(String content, boolean isXml) throws NoValuePresentException {
        Set<String> unresolvedDataSources = new HashSet<>();
        Set<String> unresolvedPlaceholders = new HashSet<>();
        StringBuilder sb = new StringBuilder();
        int last = content.length() - 1;
        int offset = 0;
        int placeholderAt = -1;
        for (int i = 0; i < last; i++) {
            if (content.charAt(i) == '{') {
                if (content.charAt(i + 1) == '{') {
                    i++;
                    while (content.charAt(i + 1) == '{' && i < last) {
                        i++;
                    }
                    placeholderAt = i - 1;
                    sb.append(content, offset, placeholderAt);
                    offset = placeholderAt;
                }
            } else if (placeholderAt >= 0 && content.charAt(i) == '}' && content.charAt(i + 1) == '}') {
                String query = content.substring(placeholderAt + 2, i);
                if (isXml) {
                    Matcher m = SEPARATE_XML_TAGS.matcher(query);
                    StringBuilder rebuild = new StringBuilder();
                    while (m.find()) {
                        sb.append(m.group(1));
                        rebuild.append(m.group(2));
                    }
                    query = unescapeXml(rebuild.toString());
                }
                try {
                    query = query.trim();
                    JsonNode node = evaluateQuery(query);
                    if (node != null && node.isValueNode()) {
                        sb.append(node.asText());
                    } else if (node != null && node.isArray()) {
                        sb.append(node.toString());
                    } else {
                        unresolvedPlaceholders.add(query);
                        putDataSource(query, null);
                        sb.append("**").append(query).append("**");
                    }
                } catch (UnresolvedDataSourceException e) {
                    unresolvedDataSources.add(e.getDataSourceName());
                    sb.append("{{").append(query).append("}}");
                }
                placeholderAt = -1;
                offset = ++i + 1;
            }
        }
        if (sb.length() == 0) {
            return content;
        }
        sb.append(content, offset, content.length());
        content = sb.toString();
        if (!unresolvedDataSources.isEmpty() || !unresolvedPlaceholders.isEmpty()) {
            throw new NoValuePresentException(unresolvedDataSources, unresolvedPlaceholders, content);
        }
        return fillInPlaceholderLoop(content, isXml);
    }

    public String fillInXmlPlaceholderWithResolver(String content, Function<String, String> dictionaryFinder,
                                                   BiFunction<String, String, Josson> dataFinder,
                                                   ResolverProgress progress)
        throws NoValuePresentException {
        try {
            return fillInPlaceholderWithResolver(content, dictionaryFinder, dataFinder, true, progress);
        } finally {
            progress.markCompleted();
        }
    }

    public String fillInPlaceholderWithResolver(String content, Function<String, String> dictionaryFinder,
                                                BiFunction<String, String, Josson> dataFinder,
                                                ResolverProgress progress)
        throws NoValuePresentException {
        try {
            return fillInPlaceholderWithResolver(content, dictionaryFinder, dataFinder, false, progress);
        } finally {
            progress.markCompleted();
        }
    }

    private String fillInPlaceholderWithResolver(String content, Function<String, String> dictionaryFinder,
                                                 BiFunction<String, String, Josson> dataFinder, boolean isXml,
                                                 ResolverProgress progress)
        throws NoValuePresentException {
        Set<String> unresolvedPlaceholders = new HashSet<>();
        Set<String> unresolvedDataSourceNames = new HashSet<>();
        while (true) {
            progress.nextRound();
            try {
                if (!unresolvedDataSourceNames.isEmpty()) {
                    throw new NoValuePresentException(new HashSet<>(unresolvedDataSourceNames), null);
                }
                content = fillInPlaceholderLoop(content, isXml);
                break;
            } catch (NoValuePresentException e) {
                if (!e.getDataSourceNames().isEmpty()) {
                    progress.addStep("Unresolved " + e.getDataSourceNames());
                }
                if (e.getPlaceholders() != null && !e.getPlaceholders().isEmpty()) {
                    progress.addStep("Unresolved placeholders " + e.getPlaceholders());
                }
                if (e.getPlaceholders() == null) {
                    unresolvedDataSourceNames.clear();
                } else {
                    unresolvedPlaceholders.addAll(e.getPlaceholders());
                    content = e.getContent();
                }
                Map<String, String> namedQueries = new HashMap<>();
                e.getDataSourceNames().forEach(name -> {
                    String findQuery = dictionaryFinder.apply(name);
                    if (findQuery == null) {
                        putDataSource(name, null);
                        return;
                    }
                    try {
                        findQuery = fillInPlaceholderLoop(findQuery, false);
                        Matcher m = IS_DB_QUERY.matcher(findQuery);
                        if (m.find()) {
                            String collectionName = m.group(1).trim();
                            if (collectionName.isEmpty()) {
                                collectionName = name;
                            }
                            if (m.group(2) != null) {
                                collectionName += "[]";
                            }
                            progress.addStep("Resolving " + name + " from DB query " + findQuery);
                            putDataSource(name, dataFinder.apply(collectionName, m.group(3)));
                        } else {
                            namedQueries.put(name, findQuery);
                            unresolvedDataSourceNames.remove(name);
                        }
                    } catch (NoValuePresentException ex) {
                        if (ex.getPlaceholders().isEmpty()) {
                            ex.getDataSourceNames().stream()
                                .filter(s -> !namedQueries.containsKey(s))
                                .forEach(unresolvedDataSourceNames::add);
                        } else {
                            unresolvedPlaceholders.addAll(ex.getPlaceholders());
                            putDataSource(name, null);
                        }
                    }
                });
                if (!namedQueries.isEmpty()) {
                    progress.addStep("Resolving " + namedQueries);
                    namedQueries.forEach((name, findQuery) -> {
                        try {
                            JsonNode node = evaluateQuery(findQuery);
                            if (node == null) {
                                unresolvedPlaceholders.add(name);
                                putDataSource(name, null);
                            } else {
                                putDataSource(name, Josson.create(node));
                                unresolvedDataSourceNames.remove(name);
                                progress.addStep("Resolved " + name + " = " + showProgressResolvedValue(node));
                            }
                        } catch (UnresolvedDataSourceException ex) {
                            unresolvedDataSourceNames.add(ex.getDataSourceName());
                        }
                    });
                }
            }
        }
        if (!unresolvedPlaceholders.isEmpty()) {
            progress.addStep("Unresolved placeholders " + unresolvedPlaceholders);
            throw new NoValuePresentException(null, unresolvedPlaceholders, content);
        }
        return content;
    }

    public JsonNode evaluateQueryWithResolver(String query, Function<String, String> dictionaryFinder,
                                              BiFunction<String, String, Josson> dataFinder,
                                              ResolverProgress progress) {
        JsonNode node = evaluateQueryWithResolverLoop(query, dictionaryFinder, dataFinder, progress);
        progress.markCompleted();
        return node;
    }

    private JsonNode evaluateQueryWithResolverLoop(String query, Function<String, String> dictionaryFinder,
                                                   BiFunction<String, String, Josson> dataFinder,
                                                   ResolverProgress progress) {
        JsonNode result = null;
        while (true) {
            progress.nextRound();
            try {
                result = evaluateQuery(query);
                break;
            } catch (UnresolvedDataSourceException e) {
                String name = e.getDataSourceName();
                progress.addStep("Unresolved " + name);
                String findQuery = dictionaryFinder.apply(name);
                if (findQuery == null) {
                    putDataSource(name, null);
                    break;
                }
                try {
                    findQuery = fillInPlaceholderWithResolver(
                        findQuery, dictionaryFinder, dataFinder, false, progress);
                    Matcher m = IS_DB_QUERY.matcher(findQuery);
                    if (m.find()) {
                        String collectionName = m.group(1).trim();
                        if (collectionName.isEmpty()) {
                            collectionName = name;
                        }
                        if (m.group(2) != null) {
                            collectionName += "[]";
                        }
                        progress.addStep("Resolving " + name + " from DB query " + findQuery);
                        putDataSource(name, dataFinder.apply(collectionName, m.group(3)));
                    } else {
                        progress.addStep("Resolving " + name + " from " + findQuery);
                        JsonNode node = evaluateQueryWithResolverLoop(findQuery, dictionaryFinder, dataFinder, progress);
                        if (node == null) {
                            putDataSource(name, null);
                            break;
                        }
                        putDataSource(name, Josson.create(node));
                        progress.addStep("Resolved " + name + " = " + showProgressResolvedValue(node));
                    }
                } catch (NoValuePresentException ex) {
                    putDataSource(name, null);
                    break;
                }
            }
        }
        return result;
    }

    public JsonNode evaluateQuery(String query) throws UnresolvedDataSourceException {
        JsonNode node = null;
        String ifTrueValue = null;
        Matcher m = DECOMPOSE_IIF_ELSE.matcher(query);
        while (m.find() && node == null) {
            String statement = m.group(1).trim();
            if (statement.isEmpty()) {
                return null;
            }
            ifTrueValue = m.group(4);
            if (StringUtils.isBlank(ifTrueValue)) {
                if (statement.charAt(0) == '\'') {
                    return TextNode.valueOf(unquoteString(statement));
                }
                try {
                    return new DoubleNode(Double.parseDouble(statement));
                } catch (NumberFormatException e) {
                    // continue
                }
            }
            node = evaluateStatement(statement);
            if (node != null) {
                if (StringUtils.isBlank(ifTrueValue)) {
                    if (node.isTextual() && node.textValue().isEmpty()) {
                        node = null;
                    }
                } else if (node.asBoolean()) {
                    node = evaluateStatement(ifTrueValue);
                } else {
                    node = null;
                }
            }
        }
        if (node == null && StringUtils.isNotBlank(ifTrueValue)) {
            return TextNode.valueOf("");
        }
        return node;
    }

    public JsonNode evaluateStatement(String statement) throws UnresolvedDataSourceException {
        statement = statement.trim();
        if (StringUtils.isEmpty(statement)) {
            return null;
        }
        if (statement.charAt(0) == '\'') {
            return TextNode.valueOf(unquoteString(statement));
        }
        JsonNode node = null;
        Matcher m = DECOMPOSE_CONDITIONS.matcher(statement);
        while (m.find()) {
            String expression = m.group(2).trim();
            if (expression.isEmpty()) {
                return null;
            }
            String operator = m.group(1);
            switch (operator) {
                case "&":
                    if (node != null && !node.asBoolean()) {
                        return BooleanNode.FALSE;
                    }
                    // fallthrough
                case "":
                    node = evaluateExpression(expression);
                    continue;
            }
            if (node == null || node.isContainerNode()) {
                return null;
            }
            JsonNode compareToNode = evaluateExpression(expression);
            if (compareToNode == null) {
                return null;
            }
            if (compareToNode.isTextual()) {
                if (node.isTextual()) {
                    int compareResult = node.asText().compareTo(compareToNode.asText());
                    switch (operator) {
                        case "=":
                            node = BooleanNode.valueOf(compareResult == 0);
                            break;
                        case "!=":
                            node = BooleanNode.valueOf(compareResult != 0);
                            break;
                        case ">":
                            node = BooleanNode.valueOf(compareResult > 0);
                            break;
                        case ">=":
                            node = BooleanNode.valueOf(compareResult >= 0);
                            break;
                        case "<":
                            node = BooleanNode.valueOf(compareResult < 0);
                            break;
                        case "<=":
                            node = BooleanNode.valueOf(compareResult <= 0);
                            break;
                        default:
                            return null;
                    }
                    continue;
                }
                JsonNode swap = node;
                node = compareToNode;
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
            if (compareToNode.isDouble()) {
                try {
                    double value = node.isDouble() ? node.asDouble() : Double.parseDouble(node.asText());
                    switch (operator) {
                        case "=":
                            node = BooleanNode.valueOf(value == compareToNode.asDouble());
                            break;
                        case "!=":
                            node = BooleanNode.valueOf(value != compareToNode.asDouble());
                            break;
                        case ">":
                            node = BooleanNode.valueOf(value > compareToNode.asDouble());
                            break;
                        case ">=":
                            node = BooleanNode.valueOf(value >= compareToNode.asDouble());
                            break;
                        case "<":
                            node = BooleanNode.valueOf(value < compareToNode.asDouble());
                            break;
                        case "<=":
                            node = BooleanNode.valueOf(value <= compareToNode.asDouble());
                            break;
                        default:
                            return null;
                    }
                } catch (NumberFormatException e) {
                    node = BooleanNode.FALSE;
                }
            } else if (compareToNode.isBoolean()) {
                switch (operator) {
                    case "=":
                        node = BooleanNode.valueOf(!node.asBoolean() ^ compareToNode.asBoolean());
                        break;
                    case "!=":
                        node = BooleanNode.valueOf(node.asBoolean() ^ compareToNode.asBoolean());
                        break;
                    default:
                        return null;
                }
            } else if (compareToNode.isNull()) {
                switch (operator) {
                    case "=":
                        node = BooleanNode.valueOf(node.isNull());
                        break;
                    case "!=":
                        node = BooleanNode.valueOf(!node.isNull());
                        break;
                    default:
                        return null;
                }
            } else {
                return null;
            }
        }
        return node;
    }

    private JsonNode evaluateExpression(String expression) throws UnresolvedDataSourceException {
        if (expression.equalsIgnoreCase("true")) {
            return BooleanNode.TRUE;
        }
        if (expression.equalsIgnoreCase("false")) {
            return BooleanNode.FALSE;
        }
        if (expression.equalsIgnoreCase("null")) {
            return NullNode.instance;
        }
        if (expression.charAt(0) == '\'') {
            if (expression.charAt(expression.length() - 1) == '\'') {
                return TextNode.valueOf(unquoteString(expression));
            }
            return null;
        }
        try {
            return new DoubleNode(Double.parseDouble(expression));
        } catch (NumberFormatException e) {
            // continue
        }
        if (dataSources.containsKey(expression)) {
            Josson josson = dataSources.get(expression);
            if (josson == null) {
                return null;
            }
            return josson.getJsonNode();
        }
        Matcher m = IS_JSON_QUERY.matcher(expression);
        if (!m.find()) {
            throw new UnresolvedDataSourceException(expression);
        }
        // Search document from cache
        String key = m.group(1).trim();
        if (!dataSources.containsKey(key)) {
            throw new UnresolvedDataSourceException(key);
        }
        Josson josson = dataSources.get(key);
        if (josson == null) {
            return null;
        }
        JsonNode node = josson.getNode(m.group(2));
        dataSources.put(expression, node == null ? null : Josson.create(node));
        return node;
    }

    public String evaluateExpressionForText(String expression) throws UnresolvedDataSourceException {
        JsonNode node = evaluateExpression(expression.trim());
        return node == null ? null : node.textValue();
    }

    public Number evaluateExpressionForNumber(String expression) throws UnresolvedDataSourceException {
        JsonNode node = evaluateExpression(expression.trim());
        return node == null ? null : node.numberValue();
    }

    public Boolean evaluateExpressionForBoolean(String expression) throws UnresolvedDataSourceException {
        JsonNode node = evaluateExpression(expression.trim());
        return node == null ? null : node.booleanValue();
    }
}

package com.octomix.josson;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;
import com.octomix.josson.exception.NoValuePresentException;
import com.octomix.josson.exception.UnresolvedDatasetException;
import org.apache.commons.lang3.StringUtils;

import java.util.*;
import java.util.function.BiFunction;
import java.util.function.Function;

import static com.octomix.josson.JossonCore.*;

import static com.octomix.josson.PatternMatcher.*;
import static org.apache.commons.text.StringEscapeUtils.unescapeXml;

public class Jossons {

    private final Map<String, Josson> datasets = new HashMap<>();
    private ShowResolvedValueMode showResolvedValueMode = ShowResolvedValueMode.VALUE_NODE_ONLY;

    public static Jossons create(JsonNode datasets) {
        if (datasets != null && datasets.getNodeType() != JsonNodeType.OBJECT) {
            throw new IllegalArgumentException("Argument is not an object node");
        }
        Jossons jossons = new Jossons();
        if (datasets != null) {
            datasets.fields().forEachRemaining(entry ->
                jossons.datasets.put(entry.getKey(), Josson.create(entry.getValue())));
        }
        return jossons;
    }

    public static Jossons fromJsonString(String json) throws JsonProcessingException {
        return create(Josson.readJsonNode(json));
    }

    public static Jossons fromMap(Map<String, String> textParams) {
        Jossons jossons = new Jossons();
        if (textParams != null) {
            textParams.forEach((key, value) ->
                jossons.datasets.put(key, Josson.fromText(value)));
        }
        return jossons;
    }

    public Jossons putDataset(String key, Josson value) {
        datasets.put(key, value);
        return this;
    }

    public Map<String, Josson> getDatasets() {
        return datasets;
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

    private boolean buildDataset(String name, String findQuery, Function<String, String> dictionaryFinder,
                                 BiFunction<String, String, Josson> dataFinder, ResolverProgress progress) {
        String[] tokens = matchDbQuery(findQuery);
        if (tokens != null) {
            String collectionName = (tokens[0].isEmpty() ? name : tokens[0]) + tokens[1];
            Josson dataset = dataFinder.apply(collectionName, tokens[2]);
            progress.addStep((dataset == null ? "Unresolved " : "Resolved ") + name + " from DB query " + findQuery);
            putDataset(name, dataset);
            return true;
        }
        List<String[]> conditions = decomposeConditions(findQuery);
        if (conditions.size() < 2) {
            return false;
        }
        String[] leftQuery = matchJoinOperation(conditions.get(0)[1]);
        if (leftQuery == null) {
            return false;
        }
        if (conditions.size() > 2) {
            throw new IllegalArgumentException("too many arguments: " + findQuery);
        }
        JoinOperator operator;
        switch (conditions.get(1)[0]) {
            case ">=<":
                operator = JoinOperator.INNER_JOIN_ONE;
                break;
            case "<=<":
                operator = JoinOperator.LEFT_JOIN_ONE;
                break;
            case ">=>":
                operator = JoinOperator.RIGHT_JOIN_ONE;
                break;
            case "<=<<":
                operator = JoinOperator.LEFT_JOIN_MANY;
                break;
            case ">>=>":
                operator = JoinOperator.RIGHT_JOIN_MANY;
                break;
            default:
                return false;
        }
        try {
            String[] rightQuery = matchJoinOperation(conditions.get(1)[1]);
            String[] leftKeys = leftQuery[1].split(",");
            String[] rightKeys = rightQuery == null ? null : rightQuery[1].split(",");
            if (anyIsBlank(leftKeys) || anyIsBlank(rightKeys)) {
                throw new IllegalArgumentException("missing join key");
            }
            if (leftKeys.length != rightKeys.length) {
                throw new IllegalArgumentException("mismatch key count");
            }
            JsonNode leftNode = evaluateQueryWithResolverLoop(leftQuery[0], dictionaryFinder, dataFinder, progress);
            if (leftNode == null) {
                throw new IllegalArgumentException("unresolved left side");
            }
            if (!leftNode.isContainerNode()) {
                throw new IllegalArgumentException("left side is not a container node");
            }
            JsonNode rightNode = evaluateQueryWithResolverLoop(rightQuery[0], dictionaryFinder, dataFinder, progress);
            if (rightNode == null) {
                throw new IllegalArgumentException("unresolved right side");
            }
            if (!rightNode.isContainerNode()) {
                throw new IllegalArgumentException("right side is not a container node");
            }
            String leftArrayName = null;
            String rightArrayName = null;
            int pos = leftKeys[0].indexOf(':');
            if (pos >= 0) {
                leftArrayName = leftKeys[0].substring(0, pos);
                leftKeys[0] = leftKeys[0].substring(pos + 1);
            }
            pos = rightKeys[0].indexOf(':');
            if (pos >= 0) {
                rightArrayName = rightKeys[0].substring(0, pos);
                rightKeys[0] = rightKeys[0].substring(pos + 1);
            }
            if ((operator == JoinOperator.LEFT_JOIN_MANY && rightArrayName == null)
                    || (operator == JoinOperator.RIGHT_JOIN_MANY && leftArrayName == null)) {
                throw new IllegalArgumentException("missing array name for join-many operation");
            }
            JsonNode joinedNode = joinNodes(
                    leftNode, leftKeys, leftArrayName, operator, rightNode, rightKeys, rightArrayName);
            if (joinedNode == null) {
                throw new IllegalArgumentException("no data matched");
            }
            putDataset(name, Josson.create(joinedNode));
            progress.addStep("Resolved " + name + " by join operation " + findQuery);
        } catch (IllegalArgumentException e) {
            putDataset(name, null);
            progress.addStep("Unresolved " + name + " - " + e.getMessage() + " " + findQuery);
        }
        return true;
    }

    public String fillInXmlPlaceholder(String content) throws NoValuePresentException {
        return fillInPlaceholderLoop(content, true);
    }

    public String fillInPlaceholder(String content) throws NoValuePresentException {
        return fillInPlaceholderLoop(content, false);
    }

    private String fillInPlaceholderLoop(String content, boolean isXml) throws NoValuePresentException {
        Set<String> unresolvedDatasets = new HashSet<>();
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
                    StringBuilder rebuild = new StringBuilder();
                    for (String token : separateXmlTags(query)) {
                        if (token.charAt(0) == '<') {
                            sb.append(token);
                        } else {
                            rebuild.append(token);
                        }
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
                        putDataset(query, null);
                        sb.append("**").append(query).append("**");
                    }
                } catch (UnresolvedDatasetException e) {
                    unresolvedDatasets.add(e.getDatasetName());
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
        if (!unresolvedDatasets.isEmpty() || !unresolvedPlaceholders.isEmpty()) {
            throw new NoValuePresentException(unresolvedDatasets, unresolvedPlaceholders, content);
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
        Set<String> unresolvedDatasetNames = new HashSet<>();
        for (;;progress.nextRound()) {
            try {
                if (!unresolvedDatasetNames.isEmpty()) {
                    throw new NoValuePresentException(new HashSet<>(unresolvedDatasetNames), null);
                }
                content = fillInPlaceholderLoop(content, isXml);
                break;
            } catch (NoValuePresentException e) {
                if (!e.getDatasetNames().isEmpty()) {
                    progress.addStep("Unresolved " + e.getDatasetNames());
                }
                if (e.getPlaceholders() != null && !e.getPlaceholders().isEmpty()) {
                    progress.addStep("Unresolved placeholders " + e.getPlaceholders());
                }
                if (e.getPlaceholders() == null) {
                    unresolvedDatasetNames.clear();
                } else {
                    unresolvedPlaceholders.addAll(e.getPlaceholders());
                    content = e.getContent();
                }
                Map<String, String> namedQueries = new HashMap<>();
                e.getDatasetNames().forEach(name -> {
                    String findQuery = dictionaryFinder.apply(name);
                    if (findQuery == null) {
                        putDataset(name, null);
                        return;
                    }
                    try {
                        findQuery = fillInPlaceholderLoop(findQuery, false);
                        if (!buildDataset(name, findQuery, dictionaryFinder, dataFinder, progress)) {
                            namedQueries.put(name, findQuery);
                            unresolvedDatasetNames.remove(name);
                        }
                    } catch (NoValuePresentException ex) {
                        if (ex.getPlaceholders().isEmpty()) {
                            ex.getDatasetNames().stream()
                                .filter(s -> !namedQueries.containsKey(s))
                                .forEach(unresolvedDatasetNames::add);
                        } else {
                            unresolvedPlaceholders.addAll(ex.getPlaceholders());
                            putDataset(name, null);
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
                                putDataset(name, null);
                            } else {
                                putDataset(name, Josson.create(node));
                                unresolvedDatasetNames.remove(name);
                                progress.addStep("Resolved " + name + " = " + showProgressResolvedValue(node));
                            }
                        } catch (UnresolvedDatasetException ex) {
                            unresolvedDatasetNames.add(ex.getDatasetName());
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
        for (;;progress.nextRound()) {
            try {
                result = evaluateQuery(query);
                break;
            } catch (UnresolvedDatasetException e) {
                String name = e.getDatasetName();
                progress.addStep("Unresolved " + name);
                String findQuery = dictionaryFinder.apply(name);
                if (findQuery == null) {
                    putDataset(name, null);
                    break;
                }
                try {
                    findQuery = fillInPlaceholderWithResolver(
                        findQuery, dictionaryFinder, dataFinder, false, progress);
                    if (!buildDataset(name, findQuery, dictionaryFinder, dataFinder, progress)) {
                        progress.addStep("Resolving " + name + " from " + findQuery);
                        JsonNode node = evaluateQueryWithResolverLoop(findQuery, dictionaryFinder, dataFinder, progress);
                        if (node == null) {
                            putDataset(name, null);
                            break;
                        }
                        putDataset(name, Josson.create(node));
                        progress.addStep("Resolved " + name + " = " + showProgressResolvedValue(node));
                    }
                } catch (NoValuePresentException ex) {
                    putDataset(name, null);
                    break;
                }
            }
        }
        return result;
    }

    public JsonNode evaluateQuery(String query) throws UnresolvedDatasetException {
        String ifTrueValue = null;
        List<String[]> steps = decomposeTernarySteps(query);
        for (String[] step : steps) {
            JsonNode node = evaluateStatement(step[0]);
            if (step[1] == null) {
                return node;
            }
            ifTrueValue = step[1];
            if (node != null) {
                if (ifTrueValue.isEmpty()) {
                    if (!(node.isTextual() && node.textValue().isEmpty())) {
                        return node;
                    }
                } else if (node.asBoolean()) {
                    node = evaluateStatement(ifTrueValue);
                    if (node != null) {
                        return node;
                    }
                }
            }
        }
        return StringUtils.isEmpty(ifTrueValue) ? null : TextNode.valueOf("");
    }

    public JsonNode evaluateStatement(String statement) throws UnresolvedDatasetException {
        try {
            return toValueNode(statement);
        } catch (NumberFormatException e) {
            // continue
        }
        LogicalOpStack opStack = new LogicalOpStack(datasets);
        List<String[]> conditions = decomposeConditions(statement);
        for (String[] condition : conditions) {
            try {
                opStack.evaluate(condition[0], condition[1]);
            } catch (IllegalArgumentException e) {
                if (e.getMessage() == null) {
                    throw new IllegalArgumentException(statement);
                }
                throw new IllegalArgumentException("\"" + e.getMessage() + "\" in " + statement);
            }
        }
        return opStack.finalResult();
    }

    public String evaluateExpressionForText(String expression) throws UnresolvedDatasetException {
        JsonNode node = evaluateExpression(expression.trim(), datasets);
        return node == null ? null : node.textValue();
    }

    public Number evaluateExpressionForNumber(String expression) throws UnresolvedDatasetException {
        JsonNode node = evaluateExpression(expression.trim(), datasets);
        return node == null ? null : node.numberValue();
    }

    public Boolean evaluateExpressionForBoolean(String expression) throws UnresolvedDatasetException {
        JsonNode node = evaluateExpression(expression.trim(), datasets);
        return node == null ? null : node.booleanValue();
    }
}

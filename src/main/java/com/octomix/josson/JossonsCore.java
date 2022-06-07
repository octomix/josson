package com.octomix.josson;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.octomix.josson.commons.StringUtils;
import com.octomix.josson.exception.NoValuePresentException;
import com.octomix.josson.exception.UnresolvedDatasetException;

import java.util.*;
import java.util.function.BiFunction;
import java.util.function.Function;

import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.PatternMatcher.*;
import static com.octomix.josson.commons.StringEscapeUtils.unescapeXml;

/**
 * Core functions for Jossons.
 */
class JossonsCore {

    private static final char PLACEHOLDER_OPEN = '{';

    private static final char PLACEHOLDER_CLOSE = '}';

    private static final char ANTI_INJECTION_SYMBOL = '\u001B';

    private static final char ANTI_INJECTION_OPEN = '(';

    private static final char ANTI_INJECTION_CLOSE = ')';

    private static final String UNRESOLVABLE_PLACEHOLDER_MARK = "**";

    private static final String DICTIONARY_FUNCTION_PARAMS = "$params";

    protected final Map<String, Josson> datasets = new HashMap<>();

    protected JossonsCore() {
    }

    private boolean buildDataset(final String name, final String query, final Function<String, String> dictionaryFinder,
                                 final BiFunction<String, String, Josson> dataFinder, final ResolverProgress progress) {
        Josson dataset = null;
        final String[] tokens = matchDbQuery(query);
        if (tokens != null) {
            progress.addResolvingFrom(name, query);
            final String collectionName = (tokens[0].isEmpty() ? name : tokens[0]) + tokens[1];
            dataset = dataFinder.apply(collectionName, tokens[2]);
        } else {
            final JoinDatasets joinDatasets = matchJoinDatasetOperation(query);
            if (joinDatasets == null) {
                return false;
            }
            progress.addResolvingFrom(name, query);
            try {
                dataset = joinDatasets(joinDatasets, dictionaryFinder, dataFinder, progress);
            } catch (IllegalArgumentException e) {
                progress.addStep("Join operation failed - " + e.getMessage());
            }
        }
        progress.addResolvedDataset(name, dataset);
        datasets.put(name, dataset);
        return true;
    }

    private Josson joinDatasets(final JoinDatasets datasets, final Function<String, String> dictionaryFinder,
                                final BiFunction<String, String, Josson> dataFinder, final ResolverProgress progress) {
        final JsonNode leftNode = evaluateQueryWithResolverLoop(
                datasets.getLeftDataset().getQuery(), dictionaryFinder, dataFinder, progress);
        if (leftNode == null) {
            throw new IllegalArgumentException("unresolvable left side");
        }
        if (!leftNode.isContainerNode()) {
            throw new IllegalArgumentException("left side is not a container node");
        }
        final JsonNode rightNode = evaluateQueryWithResolverLoop(
                datasets.getRightDataset().getQuery(), dictionaryFinder, dataFinder, progress);
        if (rightNode == null) {
            throw new IllegalArgumentException("unresolvable right side");
        }
        if (!rightNode.isContainerNode()) {
            throw new IllegalArgumentException("right side is not a container node");
        }
        final JsonNode joinedNode = datasets.joinNodes(leftNode, rightNode);
        if (joinedNode == null) {
            throw new IllegalArgumentException("invalid data");
        }
        return Josson.create(joinedNode);
    }

    private void evaluateQueryWithResolver(final String name, final String query, final ResolverProgress progress,
                                           final Set<String> unresolvablePlaceholders,
                                           final Set<String> unresolvedDatasetNames) {
        try {
            final JsonNode node = new OperationStackForDatasets(datasets).evaluateQuery(query);
            if (node == null) {
                unresolvablePlaceholders.add(name);
                datasets.put(name, null);
            } else {
                datasets.put(name, Josson.create(node));
                unresolvedDatasetNames.remove(name);
                progress.addResolvedNode(name, node);
            }
        } catch (UnresolvedDatasetException e) {
            unresolvedDatasetNames.add(e.getDatasetName());
        }
    }

    private String dictionaryFinderApply(final String name,
                                         final Function<String, String> dictionaryFinder,
                                         final BiFunction<String, String, Josson> dataFinder,
                                         final ResolverProgress progress) {
        String query = dictionaryFinder.apply(name);
        if (query == null) {
            final String[] funcAndArgs = matchFunctionAndArgument(name);
            if (funcAndArgs == null || (query = dictionaryFinder.apply(funcAndArgs[0] + "()")) == null) {
                datasets.put(name, null);
                return null;
            }
            progress.addResolvingFrom(name, query);
            final ArrayNode params = MAPPER.createArrayNode();
            for (String param : decomposeFunctionParameters(funcAndArgs[1], 0, -1)) {
                params.add(evaluateQueryWithResolverLoop(param, dictionaryFinder, dataFinder, progress));
            }
            removeDictionaryFunctionParams();
            datasets.put(DICTIONARY_FUNCTION_PARAMS, Josson.create(params));
            for (int i = 0; i < params.size(); i++) {
                datasets.put("$" + i, Josson.create(params.get(i)));
            }
        }
        return query;
    }

    private Map<String, Josson> backupDictionaryFunctionParams() {
        final Josson params = datasets.get(DICTIONARY_FUNCTION_PARAMS);
        if (params == null) {
            return null;
        }
        final Map<String, Josson> backup = new HashMap<>();
        backup.put(DICTIONARY_FUNCTION_PARAMS, params);
        for (int i = params.getNode().size() - 1; i >= 0; i--) {
            final String key = "$" + i;
            backup.put(key, datasets.get(key));
        }
        return backup;
    }

    private void restoreDictionaryFunctionParams(Map<String, Josson> backup) {
        removeDictionaryFunctionParams();
        if (backup != null) {
            datasets.putAll(backup);
        }
    }

    private void removeDictionaryFunctionParams() {
        final Josson params = datasets.remove(DICTIONARY_FUNCTION_PARAMS);
        if (params != null) {
            for (int i = params.getNode().size() - 1; i >= 0; i--) {
                datasets.remove("$" + i);
            }
        }
    }

    protected JsonNode evaluateQueryWithResolverLoop(final String query, final Function<String, String> dictionaryFinder,
                                                     final BiFunction<String, String, Josson> dataFinder,
                                                     final ResolverProgress progress) {
        final Map<String, Josson> backupParams = backupDictionaryFunctionParams();
        for (; ; progress.nextRound()) {
            try {
                return new OperationStackForDatasets(datasets).evaluateQuery(query);
            } catch (UnresolvedDatasetException e) {
                final String name = e.getDatasetName();
                JsonNode node = null;
                String findQuery = dictionaryFinderApply(name, dictionaryFinder, dataFinder, progress);
                if (findQuery != null) {
                    try {
                        findQuery = fillInPlaceholderWithResolver(
                                findQuery, dictionaryFinder, dataFinder, false, progress);
                        if (buildDataset(name, findQuery, dictionaryFinder, dataFinder, progress)) {
                            continue;
                        }
                        progress.addResolvingFrom(name, findQuery);
                        node = evaluateQueryWithResolverLoop(findQuery, dictionaryFinder, dataFinder, progress);
                    } catch (NoValuePresentException ex) {
                        // ignore
                    } finally {
                        restoreDictionaryFunctionParams(backupParams);
                    }
                }
                datasets.put(name, node == null ? null : Josson.create(node));
                progress.addResolvedNode(name, node);
            }
        }
    }

    protected String fillInPlaceholderWithResolver(String template, final Function<String, String> dictionaryFinder,
                                                   final BiFunction<String, String, Josson> dataFinder, final boolean isXml,
                                                   final ResolverProgress progress) throws NoValuePresentException {
        final Set<String> unresolvablePlaceholders = new HashSet<>();
        final Set<String> unresolvedDatasetNames = new HashSet<>();
        final List<String> resolveHistory = new ArrayList<>();
        final Map<String, Josson> backupParams = backupDictionaryFunctionParams();
        boolean isAntiInject = false;
        for (; ; progress.nextRound()) {
            try {
                if (!unresolvedDatasetNames.isEmpty()) {
                    throw new NoValuePresentException(new HashSet<>(unresolvedDatasetNames), null);
                }
                template = fillInPlaceholderLoop(template, isXml, isAntiInject);
                break;
            } catch (NoValuePresentException e) {
                isAntiInject = e.isAntiInject();
                if (e.getPlaceholders() == null) {
                    unresolvedDatasetNames.clear();
                } else {
                    unresolvablePlaceholders.addAll(e.getPlaceholders());
                    template = e.getContent();
                }
                final Map<String, String> namedQueries = new HashMap<>();
                e.getDatasetNames().forEach(name -> {
                    if (inInfiniteLoop(name, resolveHistory)) {
                        unresolvablePlaceholders.add(name);
                        datasets.put(name, null);
                        return;
                    }
                    String findQuery = dictionaryFinderApply(name, dictionaryFinder, dataFinder, progress);
                    if (findQuery != null) {
                        try {
                            findQuery = fillInPlaceholderLoop(findQuery, false, e.isAntiInject());
                            if (!buildDataset(name, findQuery, dictionaryFinder, dataFinder, progress)) {
                                unresolvedDatasetNames.remove(name);
                                if (datasets.containsKey(DICTIONARY_FUNCTION_PARAMS)) {
                                    evaluateQueryWithResolver(name, findQuery, progress,
                                            unresolvablePlaceholders, unresolvedDatasetNames);
                                } else {
                                    namedQueries.put(name, findQuery);
                                }
                            }
                        } catch (NoValuePresentException ex) {
                            if (ex.getPlaceholders().isEmpty()) {
                                ex.getDatasetNames().stream()
                                        .filter(s -> !namedQueries.containsKey(s))
                                        .forEach(unresolvedDatasetNames::add);
                            } else {
                                unresolvablePlaceholders.addAll(ex.getPlaceholders());
                                unresolvablePlaceholders.add(name);
                                datasets.put(name, null);
                            }
                        }
                        restoreDictionaryFunctionParams(backupParams);
                    }
                });
                if (!namedQueries.isEmpty()) {
                    progress.addStep("Resolving " + namedQueries);
                    namedQueries.forEach((name, findQuery) ->
                        evaluateQueryWithResolver(name, findQuery, progress, unresolvablePlaceholders, unresolvedDatasetNames));
                }
            }
        }
        if (!unresolvablePlaceholders.isEmpty()) {
            progress.addUnresolvableStep("placeholders " + unresolvablePlaceholders);
            throw new NoValuePresentException(null, unresolvablePlaceholders, template);
        }
        return template;
    }

    protected String fillInPlaceholderLoop(final String template, final boolean isXml,
                                           final boolean inIsAntiInject) throws NoValuePresentException {
        final Set<String> unresolvedDatasets = new HashSet<>();
        final Set<String> unresolvedPlaceholders = new HashSet<>();
        final StringBuilder sb = new StringBuilder();
        final int last = template.length() - 1;
        int offset = 0;
        int placeholderAt = -1;
        boolean textAdded = false;
        boolean outIsAntiInject = false;
        for (int i = 0; i < last; i++) {
            if (template.charAt(i) == PLACEHOLDER_OPEN) {
                if (template.charAt(i + 1) == PLACEHOLDER_OPEN) {
                    i++;
                    while (template.charAt(i + 1) == PLACEHOLDER_OPEN && i < last) {
                        i++;
                    }
                    placeholderAt = i - 1;
                    sb.append(template, offset, placeholderAt);
                    offset = placeholderAt;
                }
            } else if (placeholderAt >= 0
                    && template.charAt(i) == PLACEHOLDER_CLOSE
                    && template.charAt(i + 1) == PLACEHOLDER_CLOSE) {
                String query = template.substring(placeholderAt + 2, i);
                if (isXml) {
                    final StringBuilder rebuild = new StringBuilder();
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
                    query = StringUtils.strip(query);
                    final JsonNode node = new OperationStackForDatasets(datasets, inIsAntiInject).evaluateQuery(query);
                    if (node == null) {
                        unresolvedPlaceholders.add(query);
                        datasets.put(query, null);
                        sb.append(UNRESOLVABLE_PLACEHOLDER_MARK).append(query).append(UNRESOLVABLE_PLACEHOLDER_MARK);
                    } else if (node.isValueNode()) {
                        outIsAntiInject = antiInjectionEncode(sb, node.asText()) || outIsAntiInject;
                        // Remember even if it is an empty string
                        textAdded = true;
                    } else {
                        outIsAntiInject = antiInjectionEncode(sb, node.toString()) || outIsAntiInject;
                    }
                } catch (UnresolvedDatasetException e) {
                    unresolvedDatasets.add(e.getDatasetName());
                    sb.append(PLACEHOLDER_OPEN).append(PLACEHOLDER_OPEN).append(query)
                            .append(PLACEHOLDER_CLOSE).append(PLACEHOLDER_CLOSE);
                }
                placeholderAt = -1;
                offset = ++i + 1;
            }
        }
        if (sb.length() == 0 && !textAdded) {
            return inIsAntiInject ? antiInjectionDecode(template) : template;
        }
        if (placeholderAt >= 0) {
            unresolvedPlaceholders.add("Lack of closing tag: "
                    + StringUtils.abbreviate(template.substring(placeholderAt), 0, 20));
            sb.append(UNRESOLVABLE_PLACEHOLDER_MARK).append(template, placeholderAt + 2, template.length());
        } else {
            sb.append(template, offset, template.length());
        }
        String filled = sb.toString();
        if (!unresolvedDatasets.isEmpty() || !unresolvedPlaceholders.isEmpty()) {
            throw new NoValuePresentException(unresolvedDatasets, unresolvedPlaceholders, filled).isAntiInject(outIsAntiInject);
        }
        return fillInPlaceholderLoop(filled, isXml, outIsAntiInject);
    }

    private static boolean inInfiniteLoop(final String name, final List<String> resolveHistory) {
        resolveHistory.add(name);
        final int half = resolveHistory.size() / 2;
        int i = resolveHistory.size() - 2;
        for (int j = i; j >= half; j--) {
            if (resolveHistory.get(j).equals(name)) {
                for (int k = j - 1; i >= j; i--, k--) {
                    if (!resolveHistory.get(k).equals(resolveHistory.get(i))) {
                        break;
                    }
                }
                if (i < j) {
                    return true;
                }
                break;
            }
        }
        return false;
    }

    private static boolean antiInjectionEncode(final StringBuilder sb, final String s) {
        final int len = s.length();
        int offset = 0;
        for (int i = 0; i < len; i++) {
            switch (s.charAt(i)) {
                case PLACEHOLDER_OPEN:
                    sb.append(s, offset, i).append(ANTI_INJECTION_SYMBOL).append(ANTI_INJECTION_OPEN);
                    offset = i + 1;
                    break;
                case PLACEHOLDER_CLOSE:
                    sb.append(s, offset, i).append(ANTI_INJECTION_SYMBOL).append(ANTI_INJECTION_CLOSE);
                    offset = i + 1;
                    break;
            }
        }
        if (offset == 0) {
            sb.append(s);
            return false;
        }
        sb.append(s, offset, len);
        return true;
    }

    static String antiInjectionDecode(final String s) {
        final StringBuilder sb = new StringBuilder();
        final int len = s.length();
        int offset = 0;
        for (int i = 0, last = len - 1; i < last; i++) {
            if (s.charAt(i) == ANTI_INJECTION_SYMBOL) {
                switch (s.charAt(i + 1)) {
                    case ANTI_INJECTION_OPEN:
                        sb.append(s, offset, i).append(PLACEHOLDER_OPEN);
                        offset = ++i + 1;
                        break;
                    case ANTI_INJECTION_CLOSE:
                        sb.append(s, offset, i).append(PLACEHOLDER_CLOSE);
                        offset = ++i + 1;
                        break;
                }
            }
        }
        return offset == 0 ? s : sb.append(s, offset, len).toString();
    }
}

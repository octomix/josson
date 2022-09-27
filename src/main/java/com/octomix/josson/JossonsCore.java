package com.octomix.josson;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.NullNode;
import com.octomix.josson.commons.StringUtils;
import com.octomix.josson.exception.NoValuePresentException;
import com.octomix.josson.exception.UnresolvedDatasetException;

import java.util.*;
import java.util.function.BiFunction;
import java.util.function.Function;

import static com.octomix.josson.FuncExecutor.UNLIMITED_WITH_PATH;
import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.PatternMatcher.*;
import static com.octomix.josson.commons.StringUtils.EMPTY;

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

    private static final String DICTIONARY_FUNCTION_PARAM_PREFIX = "$";

    private static final String DICTIONARY_FUNCTION_PARAMS = DICTIONARY_FUNCTION_PARAM_PREFIX + "params";

    /**
     * Collection of datasets for query resolution.
     */
    protected final Map<String, Josson> datasets = new HashMap<>();

    protected boolean needUnescapeQuery = true;

    protected JossonsCore() {
    }

    private boolean evaluateDbQuery(final String name, final String query,
                                    final BiFunction<String, String, Josson> dataFinder, final ResolverProgress progress) {
        final String[] tokens = matchDbQuery(query);
        if (tokens == null) {
            return false;
        }
        progress.addResolvingStep(name, query);
        final Josson dataset;
        if (dataFinder == null) {
            dataset = null;
        } else {
            final String collectionName = (tokens[0].isEmpty() ? name : tokens[0]) + tokens[1];
            dataset = dataFinder.apply(collectionName, tokens[2]);
        }
        datasets.put(name, dataset);
        progress.addResolvedDataset(name, dataset);
        return true;
    }

    private boolean evaluateQuery(final String name, final String query, final ResolverProgress progress,
                                  final Set<String> unresolvedDatasetNames) {
        progress.addResolvingStep(name, query);
        JsonNode node;
        try {
            node = new OperationStackForDatasets(datasets).evaluateQuery(query);
        } catch (IllegalArgumentException e) {
            node = null;
            progress.addMessageStep(e.getMessage());
        } catch (UnresolvedDatasetException e) {
            unresolvedDatasetNames.add(e.getDatasetName());
            return false;
        }
        datasets.put(name, node == null ? null : Josson.create(node));
        progress.addResolvedNode(name, node);
        unresolvedDatasetNames.remove(name);
        return true;
    }

    private String dictionaryFinderApply(final String name, final Function<String, String> dictionaryFinder,
                                         final BiFunction<String, String, Josson> dataFinder,
                                         final ResolverProgress progress) {
        String query = dictionaryFinder.apply(name);
        if (query == null) {
            final String[] funcAndArgs = matchFunctionAndArgument(name, false);
            if (funcAndArgs == null || (query = dictionaryFinder.apply(funcAndArgs[0] + "()")) == null) {
                if (isCacheDataset(name)) {
                    datasets.put(name, null);
                }
                return null;
            }
            final ArrayNode params = MAPPER.createArrayNode();
            for (String param : decomposeFunctionParameters(funcAndArgs[1], 0, UNLIMITED_WITH_PATH)) {
                params.add(evaluateQueryWithResolverLoop(param, dictionaryFinder, dataFinder, progress));
            }
            removeDictionaryFunctionParams();
            datasets.put(DICTIONARY_FUNCTION_PARAMS, Josson.create(params));
            for (int i = 0; i < params.size(); i++) {
                datasets.put(DICTIONARY_FUNCTION_PARAM_PREFIX + i, Josson.create(params.get(i)));
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
            final String key = DICTIONARY_FUNCTION_PARAM_PREFIX + i;
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
                datasets.remove(DICTIONARY_FUNCTION_PARAM_PREFIX + i);
            }
        }
    }

    protected JsonNode evaluateQueryWithResolverLoop(final String query, final Function<String, String> dictionaryFinder,
                                                     final BiFunction<String, String, Josson> dataFinder,
                                                     final ResolverProgress progress) {
        final Map<String, Josson> backupParams = backupDictionaryFunctionParams();
        for (; ; restoreDictionaryFunctionParams(backupParams), progress.nextRound()) {
            try {
                return new OperationStackForDatasets(datasets).evaluateQuery(query);
            } catch (IllegalArgumentException e) {
                progress.addMessageStep(e.getMessage());
                return null;
            } catch (UnresolvedDatasetException e) {
                removeDictionaryFunctionParams();
                final String name = e.getDatasetName();
                JsonNode node = null;
                String findQuery = dictionaryFinderApply(name, dictionaryFinder, dataFinder, progress);
                if (findQuery != null) {
                    progress.addResolvingStep(name, findQuery);
                    try {
                        findQuery = fillInPlaceholderWithResolver(
                                findQuery, dictionaryFinder, dataFinder, MarkupLanguage.NONE, progress);
                    } catch (NoValuePresentException ex) {
                        ex.getPlaceholders().forEach(placeholder ->
                                datasets.put(placeholder, Josson.create(NullNode.getInstance())));
                        try {
                            findQuery = fillInPlaceholderWithResolver(
                                    findQuery, dictionaryFinder, dataFinder, MarkupLanguage.NONE, progress);
                        } catch (NoValuePresentException exc) {
                            findQuery = null;
                        }
                        ex.getPlaceholders().forEach(datasets::remove);
                    }
                    if (findQuery != null) {
                        if (evaluateDbQuery(name, findQuery, dataFinder, progress)) {
                            continue;
                        }
                        node = evaluateQueryWithResolverLoop(findQuery, dictionaryFinder, dataFinder, progress);
                    }
                }
                datasets.put(name, node == null ? null : Josson.create(node));
                progress.addResolvedNode(name, node);
            }
        }
    }

    protected String fillInPlaceholderWithResolver(String template, final Function<String, String> dictionaryFinder,
                                                   final BiFunction<String, String, Josson> dataFinder,
                                                   final MarkupLanguage escaping, final ResolverProgress progress)
            throws NoValuePresentException {
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
                template = fillInPlaceholderLoop(template, escaping, isAntiInject);
                break;
            } catch (NoValuePresentException e) {
                isAntiInject = e.isAntiInject();
                if (e.getPlaceholders() == null) {
                    unresolvedDatasetNames.clear();
                } else {
                    unresolvablePlaceholders.addAll(e.getPlaceholders());
                    template = e.getContent();
                }
                final Map<String, String> batchEvaluate = new HashMap<>();
                int lastHistory = resolveHistory.size();
                e.getDatasetNames().forEach(name -> {
                    if (inInfiniteLoop(name, resolveHistory)) {
                        unresolvablePlaceholders.add(name);
                        datasets.put(name, null);
                        return;
                    }
                    final String query = dictionaryFinderApply(name, dictionaryFinder, dataFinder, progress);
                    if (query != null) {
                        fillInAndResolveQuery(name, query, dataFinder, progress,
                                batchEvaluate, unresolvedDatasetNames, e.isAntiInject());
                        restoreDictionaryFunctionParams(backupParams);
                    }
                });
                batchEvaluate.forEach((name, query) -> evaluateQuery(name, query, progress, unresolvedDatasetNames));
                if (unresolvedDatasetNames.isEmpty()) {
                    while (--lastHistory >= 0) {
                        final String retry = resolveHistory.get(lastHistory);
                        if (datasets.containsKey(retry)) {
                            break;
                        }
                        final String query = dictionaryFinderApply(retry, dictionaryFinder, dataFinder, progress);
                        if (query == null) {
                            break;
                        }
                        if (!fillInAndResolveQuery(retry, query, dataFinder, progress,
                                null, unresolvedDatasetNames, e.isAntiInject())) {
                            lastHistory = 0;
                        }
                        restoreDictionaryFunctionParams(backupParams);
                    }
                }
            }
        }
        if (!unresolvablePlaceholders.isEmpty()) {
            progress.addUnresolvableStep("placeholders " + unresolvablePlaceholders);
            throw new NoValuePresentException(null, unresolvablePlaceholders, template);
        }
        return template;
    }

    private boolean fillInAndResolveQuery(final String name, final String query,
                                          final BiFunction<String, String, Josson> dataFinder,
                                          final ResolverProgress progress, final Map<String, String> batchEvaluate,
                                          final Set<String> unresolvedDatasetNames, final boolean isAntiInject) {
        try {
            unresolvedDatasetNames.remove(name);
            final String filled = fillInPlaceholderLoop(query, MarkupLanguage.NONE, isAntiInject);
            if (evaluateDbQuery(name, filled, dataFinder, progress)) {
                return true;
            }
            if (batchEvaluate == null || datasets.containsKey(DICTIONARY_FUNCTION_PARAMS)) {
                return evaluateQuery(name, filled, progress, unresolvedDatasetNames);
            }
            batchEvaluate.put(name, filled);
        } catch (NoValuePresentException ex) {
            if (ex.getPlaceholders().isEmpty()) {
                ex.getDatasetNames().stream()
                        .filter(JossonsCore::isCacheDataset)
                        .forEach(unresolvedDatasetNames::add);
            } else {
                datasets.put(name, null);
            }
        }
        return false;
    }

    protected String fillInPlaceholderLoop(final String template, final MarkupLanguage escaping,
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
                if (escaping.isEscapingApplicable() && needUnescapeQuery) {
                    final StringBuilder rebuild = new StringBuilder();
                    for (String token : escaping.separateTagAndText(query)) {
                        if (token.charAt(0) == escaping.tagOpen) {
                            sb.append(token);
                        } else {
                            rebuild.append(token);
                        }
                    }
                    query = escaping.unescape(rebuild.toString());
                }
                query = StringUtils.strip(query);
                final boolean skipEscape;
                if (!query.isEmpty()
                        && query.charAt(0) == escaping.tagOpen && query.charAt(query.length() - 1) == escaping.tagClose) {
                    query = StringUtils.strip(query.substring(1, query.length() - 1));
                    skipEscape = true;
                } else {
                    skipEscape = false;
                }
                try {
                    final JsonNode node = new OperationStackForDatasets(datasets, inIsAntiInject).evaluateQuery(query);
                    if (node == null) {
                        unresolvedPlaceholders.add(query);
                        if (isCacheDataset(query)) {
                            datasets.put(query, null);
                        }
                        sb.append(UNRESOLVABLE_PLACEHOLDER_MARK)
                                .append(needUnescapeQuery ? escaping.escape(query) : query)
                                .append(UNRESOLVABLE_PLACEHOLDER_MARK);
                    } else {
                        final String text;
                        if (node.isValueNode()) {
                            text = node.asText();
                            // Remember even if it is an empty string
                            textAdded = true;
                        } else {
                            text = node.toString();
                        }
                        outIsAntiInject = antiInjectionEncode(sb, skipEscape ? text : escaping.escape(text)) || outIsAntiInject;
                    }
                } catch (UnresolvedDatasetException e) {
                    unresolvedDatasets.add(e.getDatasetName());
                    sb.append(PLACEHOLDER_OPEN).append(PLACEHOLDER_OPEN)
                            .append(skipEscape ? escaping.tagOpen : EMPTY)
                            .append(needUnescapeQuery ? escaping.escape(query) : query)
                            .append(skipEscape ? escaping.tagClose : EMPTY)
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
        return fillInPlaceholderLoop(filled, escaping, outIsAntiInject);
    }

    private static boolean inInfiniteLoop(final String name, final List<String> resolveHistory) {
        resolveHistory.add(name);
        final int half = resolveHistory.size() / 2;
        int i = resolveHistory.size() - 2;
        for (int j = i; j >= half; j--) {
            if (resolveHistory.get(j).equals(name)) {
                for (int k = j - 1; i >= j; i--, k--) {
                    if (!resolveHistory.get(k).equals(resolveHistory.get(i))) {
                        return false;
                    }
                }
                return true;
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

    static boolean isCacheDataset(final String name) {
        // Don't cache result for variable name starts with $
        return name.charAt(0) != '$';
    }
}

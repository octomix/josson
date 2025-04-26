/*
 * Copyright 2020-2025 Choi Wai Man Raymond
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
import com.octomix.josson.commons.StringUtils;
import com.octomix.josson.exception.SyntaxErrorException;

import java.util.*;
import java.util.stream.Collectors;

import static com.octomix.josson.FunctionExecutor.*;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Mapper.*;
import static com.octomix.josson.Utils.*;
import static com.octomix.josson.commons.StringUtils.EMPTY;

/**
 * Structural functions.
 */
final class FuncStructural {

    private FuncStructural() {
    }

    static PathTrace funcAssort(final PathTrace path, final String params) {
        final List<String> paramList = new SyntaxDecomposer(params).deFunctionParameters(0, UNLIMITED_WITH_PATH);
        if (!path.isContainer()) {
            return null;
        }
        final boolean notAssorted;
        if (paramList.isEmpty()) {
            notAssorted = true;
        } else if (VAR_ARGS.equals(paramList.get(paramList.size() - 1))) {
            notAssorted = true;
            paramList.remove(paramList.size() - 1);
        } else {
            notAssorted = false;
        }
        final ArrayNode array = MAPPER.createArrayNode();
        if (path.isObject()) {
            paramList.forEach(each -> array.add(MAPPER.createObjectNode()));
            path.node().properties().forEach(field -> {
                final ObjectNode entry = MAPPER.createObjectNode().set(field.getKey(), field.getValue());
                for (int i = 0; i < paramList.size(); i++) {
                    if (!nodeIsNull(getNodeByExpression(path.push(entry), paramList.get(i)))) {
                        ((ObjectNode) array.get(i)).setAll(entry);
                        return;
                    }
                }
                if (notAssorted) {
                    array.add(entry);
                }
            });
        } else {
            paramList.forEach(each -> array.add(MAPPER.createArrayNode()));
            path.node().elements().forEachRemaining(elem -> {
                for (int i = 0; i < paramList.size(); i++) {
                    if (!nodeIsNull(getNodeByExpression(path.push(elem), paramList.get(i)))) {
                        ((ArrayNode) array.get(i)).add(elem);
                        return;
                    }
                }
                if (notAssorted) {
                    array.add(MAPPER.createArrayNode().add(elem));
                }
            });
        }
        return path.push(array);
    }

    static PathTrace funcCollect(final PathTrace path, final String params) {
        final List<String> paramList = new SyntaxDecomposer(params).deFunctionParameters(1, UNLIMITED_AND_NO_PATH);
        final ArrayNode array = MAPPER.createArrayNode();
        paramList.forEach(param -> addArrayElement(array, getNodeByExpression(path, param)));
        return path.push(array);
    }

    static PathTrace funcCumulateCollect(final PathTrace path, final String params) {
        final Pair<PathTrace, List<String>> pathAndParams = getParamPathAndStrings(path, params, 2, 2);
        final ArrayNode array = MAPPER.createArrayNode();
        funcCumulateCollect(array, pathAndParams.getKey(), null, pathAndParams.getValue().get(0), pathAndParams.getValue().get(1));
        return path.push(array);
    }

    private static void funcCumulateCollect(final ArrayNode array, final PathTrace path, final Integer index,
                                            final String expression, final String next) {
        final PathTrace result = index == null ? path : getPathByExpression(path, index, next);
        if (result == null || (index != null && Operator.EQ.compare(path.node(), result.node()))) {
            return;
        }
        if (result.isArray()) {
            for (int i = 0; i < result.containerSize(); i++) {
                addArrayElement(array, getNodeByExpression(result, i, expression));
                funcCumulateCollect(array, result, i, expression, next);
            }
            return;
        }
        addArrayElement(array, getNodeByExpression(result, expression));
        if (result.isObject()) {
            funcCumulateCollect(array, result, NON_ARRAY_INDEX, expression, next);
        }
    }

    static PathTrace funcDepthLimit(final PathTrace path, final String params) {
        final Pair<PathTrace, List<String>> pathAndParams = getParamPathAndStrings(path, params, 1, 1);
        final PathTrace dataPath = pathAndParams.getKey();
        if (dataPath == null) {
            return null;
        }
        final int depth = getNodeAsInt(path, pathAndParams.getValue().get(0));
        if (depth < 1) {
            return null;
        }
        if (dataPath.isObject()) {
            return path.push(deepCopy((ObjectNode) dataPath.node(), depth));
        }
        if (dataPath.isArray()) {
            return path.push(deepCopy((ArrayNode) dataPath.node(), depth));
        }
        return dataPath;
    }

    static PathTrace funcEntries(final PathTrace path, final String params) {
        final PathTrace paramPath = getParamPath(path, params);
        if (paramPath == null || !paramPath.isContainer()) {
            return null;
        }
        final ArrayNode array = MAPPER.createArrayNode();
        if (paramPath.isArray()) {
            paramPath.node().forEach(elem -> elem.properties()
                .forEach(field ->
                    array.add(Josson.createObjectNode().put(ENTRY_KEY_NAME, field.getKey()).set(ENTRY_VALUE_NAME, field.getValue()))));
        } else {
            paramPath.node().properties().forEach(field ->
                array.add(Josson.createObjectNode().put(ENTRY_KEY_NAME, field.getKey()).set(ENTRY_VALUE_NAME, field.getValue())));
        }
        return path.push(array);
    }

    static PathTrace funcField(final PathTrace path, final String params) {
        final List<String[]> nameAndPaths = getParamNamePath(new SyntaxDecomposer(params).deFunctionParameters(1, UNLIMITED_WITH_PATH));
        if (path.isObject()) {
            return path.push(funcMap(cloneObject((ObjectNode) path.node()), path, nameAndPaths, NON_ARRAY_INDEX));
        }
        if (path.isArray()) {
            final ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < path.containerSize(); i++) {
                final JsonNode elem = path.get(i);
                array.add(elem.isObject() ? funcMap(cloneObject((ObjectNode) elem), path, nameAndPaths, i) : null);
            }
            return path.push(array);
        }
        return null;
    }

    static PathTrace funcFlatten(final PathTrace path, final String params) {
        final Pair<PathTrace, List<String>> pathAndParams = getParamPathAndStrings(path, params, 0, 2);
        final PathTrace dataPath = pathAndParams.getKey();
        if (dataPath != null && dataPath.isContainer()) {
            final List<String> paramList = pathAndParams.getValue();
            final JsonNode param = paramList.isEmpty() ? null : getNodeByExpression(path, paramList.get(0));
            final JsonNode indexFormat = paramList.size() > 1 ? getNodeByExpression(path, paramList.get(1)) : null;
            if (param != null && (param.isTextual() || param.isNull() || !nodeIsNull(indexFormat))) {
                final ObjectNode object = MAPPER.createObjectNode();
                funcFlatten(object, null, dataPath.node(), param.isNull() ? null : param.asText(),
                        nodeIsNull(indexFormat) ? null : indexFormat.asText());
                return path.push(object);
            }
            if (dataPath.isArray()) {
                final ArrayNode array = MAPPER.createArrayNode();
                funcFlatten(array, dataPath.node(), param == null ? 0 : param.asInt());
                return path.push(array);
            }
        }
        return dataPath;
    }

    private static void funcFlatten(final ArrayNode array, final JsonNode node, final int levels) {
        node.forEach(elem -> {
            if (!elem.isArray()) {
                array.add(elem);
            } else if (levels == 1) {
                array.addAll((ArrayNode) elem);
            } else {
                funcFlatten(array, elem, levels - 1);
            }
        });
    }

    private static void funcFlatten(final ObjectNode object, final String name, final JsonNode node,
                                    final String separator, final String indexFormat) {
        if (node.isValueNode()) {
            object.set(name, node);
        } else if (node.isObject()) {
            final String prefix = name == null || separator == null ? EMPTY : name + separator;
            node.properties().forEach(elem ->
                    funcFlatten(object, prefix + elem.getKey(), elem.getValue(), separator, indexFormat));
        } else if (indexFormat == null) {
            final String prefix = name == null ? EMPTY : separator == null ? name : name + separator;
            for (int i = 0; i < node.size(); i++) {
                funcFlatten(object, prefix + i, node.get(i), separator, null);
            }
        } else {
            final String prefix = name == null ? EMPTY : name;
            for (int i = 0; i < node.size(); i++) {
                funcFlatten(object, prefix + String.format(indexFormat, i), node.get(i), separator, indexFormat);
            }
        }
    }

    static PathTrace funcGroup(final PathTrace path, final String params) {
        final Pair<PathTrace, List<String>> pathAndParams = getParamPathAndStrings(path, params, 1, 2);
        final PathTrace dataPath = pathAndParams.getKey();
        if (dataPath == null || !dataPath.isArray()) {
            return null;
        }
        final List<String> paramList = pathAndParams.getValue();
        final String[] nameAndPath = new SyntaxDecomposer(paramList.get(0)).deNameAndPath((ifFuncName) -> ENTRY_KEY_NAME);
        final String[] grouping = paramList.size() > 1
                ? new SyntaxDecomposer(paramList.get(1)).deNameAndPath((ifFuncName) -> GROUP_VALUE_NAME)
                : new String[]{GROUP_VALUE_NAME, null, null};
        final ArrayNode array = MAPPER.createArrayNode();
        for (int i = 0; i < dataPath.containerSize(); i++) {
            final String[] evalNameAndPath = evaluateNameAndPath(nameAndPath, dataPath, i);
            final JsonNode key = getNodeByExpression(
                dataPath, i, evalNameAndPath[evalNameAndPath[1] == null ? 0 : 1], evalNameAndPath[2] != null);
            if (key != null) {
                ArrayNode values = null;
                final String[] evalGrouping = evaluateNameAndPath(grouping, dataPath, i);
                for (JsonNode elem : array) {
                    if (Operator.EQ.compare(elem.get(evalNameAndPath[0]), key)) {
                        values = (ArrayNode) elem.get(evalGrouping[0]);
                        break;
                    }
                }
                if (values == null) {
                    values = MAPPER.createArrayNode();
                    final ObjectNode entry = Josson.createObjectNode();
                    entry.set(evalNameAndPath[0], key);
                    entry.set(evalGrouping[0], values);
                    array.add(entry);
                }
                values.add(evalGrouping[1] == null ? dataPath.get(i)
                        : getNodeByExpression(dataPath, i, evalGrouping[1], evalGrouping[2] != null));
            }
        }
        return path.push(array);
    }

    static PathTrace funcKeys(final PathTrace path, final String params) {
        final Pair<PathTrace, List<String>> pathAndParams = getParamPathAndStrings(path, params, 0, 1);
        final PathTrace dataPath = pathAndParams.getKey();
        if (dataPath == null || !dataPath.isObject()) {
            return null;
        }
        final ArrayNode array = MAPPER.createArrayNode();
        funcKeys(array, dataPath.node(),
                pathAndParams.getValue().isEmpty() ? 1 : getNodeAsInt(dataPath, pathAndParams.getValue().get(0)));
        return path.push(array);
    }

    private static void funcKeys(final ArrayNode array, final JsonNode node, final int levels) {
        node.properties().forEach(field -> {
            array.add(field.getKey());
            if (levels != 1 && field.getValue().isObject()) {
                funcKeys(array, field.getValue(), levels - 1);
            }
        });
    }

    static PathTrace funcMap(final PathTrace path, final String params) {
        final List<String[]> nameAndPaths = getParamNamePath(new SyntaxDecomposer(params).deFunctionParameters(1, UNLIMITED_WITH_PATH));
        if (!path.isArray()) {
            return path.push(funcMap(MAPPER.createObjectNode(), path, nameAndPaths, NON_ARRAY_INDEX));
        }
        final ArrayNode array = MAPPER.createArrayNode();
        for (int i = 0; i < path.containerSize(); i++) {
            array.add(funcMap(MAPPER.createObjectNode(), path, nameAndPaths, i));
        }
        return path.push(array);
    }

    private static ObjectNode funcMap(final ObjectNode base, final PathTrace node, final List<String[]> nameAndPaths, final int index) {
        for (String[] nameAndPath : nameAndPaths) {
            final String[] evalNameAndPath = evaluateNameAndPath(nameAndPath, node, index);
            if (evalNameAndPath[1] == null) {
                base.remove(evalNameAndPath[0]);
            } else {
                final JsonNode result = getNodeByExpression(node, index, evalNameAndPath[1], evalNameAndPath[2] != null);
                if (result == null) {
                    base.remove(evalNameAndPath[0]);
                } else if (evalNameAndPath[0].equals(WILDCARD_COLLECT_ALL)) {
                    if (result.isObject()) {
                        result.properties().forEach(field -> base.set(field.getKey(), field.getValue()));
                    }
                } else {
                    base.set(evalNameAndPath[0], result);
                }
            }
        }
        return base;
    }

    static PathTrace funcMergeArrays(final PathTrace path, final String params) {
        final List<String> paramList = new SyntaxDecomposer(params).deFunctionParameters(0, UNLIMITED_WITH_PATH);
        final ArrayNode array = MAPPER.createArrayNode();
        if (paramList.isEmpty()) {
            if (!path.isArray()) {
                return null;
            }
            final int size = path.containerSize();
            for (int i = 0; i < size; i++) {
                mergeArrays(array, path.get(i), path.getMergeArraysOption());
            }
        } else {
            for (String param : paramList) {
                if (path.isArray()) {
                    final int size = path.containerSize();
                    final JsonNode[] orderedNodes = new JsonNode[size];
                    if (size < minArraySizeToUseMultiThread || threadPoolSize == 1) {
                        for (int i = 0; i < size; i++) {
                            orderedNodes[i] = getNodeByExpression(path, i, param);
                        }
                    } else {
                        submitTasks(size, (i) -> orderedNodes[i] = getNodeByExpression(path, i, param));
                    }
                    for (int i = 0; i < size; i++) {
                        mergeArrays(array, orderedNodes[i], path.getMergeArraysOption());
                    }
                } else {
                    mergeArrays(array, getNodeByExpression(path, param), path.getMergeArraysOption());
                }
            }
        }
        return path.push(array);
    }

    static PathTrace funcMergeObjects(final PathTrace path, final String params) {
        final PathTrace array = getParamArrayOrItself(path, params);
        if (array == null) {
            return null;
        }
        ObjectNode result = null;
        for (JsonNode elem : array.node()) {
            if (elem.isObject()) {
                if (result == null) {
                    result = elem.deepCopy();
                } else {
                    mergeObjects(result, elem, path.getMergeArraysOption());
                }
            }
        }
        return path.push(result);
    }

    static PathTrace funcRemoveRetain(final PathTrace path, final String params, final boolean remove) {
        final List<String[]> paramPaths = getParamPath(new SyntaxDecomposer(params).deFunctionParameters(1, UNLIMITED_WITH_PATH));
        if (path.isValueNode()) {
            return null;
        }
        JsonNode structure = null;
        for (String[] pathAndFlag : paramPaths) {
            if (pathAndFlag[0] == null) {
                pathAndFlag[0] = pathAndFlag[1];
            } else if (!pathAndFlag[1].isEmpty()) {
                if (!asBoolean(getNodeByExpression(path, StringUtils.removeStart(pathAndFlag[1], UNRESOLVABLE_AS_NULL)))) {
                    continue;
                }
            }
            final String flatten = pathAndFlag[0].startsWith(EVALUATE_KEY_NAME)
                ? getNodeAsText(path, pathAndFlag[0].substring(1))
                : pathAndFlag[0];
            final String[] steps = StringUtils.split(flatten, ".[]");
            if (steps.length > 0) {
                structure = funcUnflatten(structure, steps, 0, BooleanNode.TRUE);
            }
        }
        return path.push(Mapper.struCopy(path.node(), structure, remove));
    }

    static PathTrace funcToArray(final PathTrace path, final String params) {
        final PathTrace container = getParamArrayOrItselfIsContainer(path, params);
        if (container == null) {
            return null;
        }
        final ArrayNode array = MAPPER.createArrayNode();
        if (container.isArray()) {
            container.node().forEach(elem -> {
                if (elem.isArray()) {
                    array.addAll((ArrayNode) elem);
                } else if (elem.isObject()) {
                    elem.forEach(array::add);
                } else {
                    array.add(elem);
                }
            });
        } else {
            container.node().forEach(array::add);
        }
        return path.push(array);
    }

    static PathTrace funcToObject(final PathTrace path, final String params) {
        final Pair<PathTrace, List<String>> pathAndParams = getParamPathAndStrings(path, params, 1, 1);
        final PathTrace dataPath = pathAndParams.getKey();
        if (dataPath == null) {
            return null;
        }
        final List<String> paramList = pathAndParams.getValue();
        return path.push(MAPPER.createObjectNode().set(getNodeAsText(path, paramList.get(0)), dataPath.node()));
    }

    static PathTrace funcUnflatten(final PathTrace path, final String params) {
        final Pair<PathTrace, List<String>> pathAndParams = getParamPathAndStrings(path, params, 1, 1);
        final PathTrace dataPath = pathAndParams.getKey();
        if (dataPath == null || !dataPath.isContainer()) {
            return null;
        }
        final String separator = getNodeAsText(path, pathAndParams.getValue().get(0));
        if (StringUtils.isEmpty(separator)) {
            return null;
        }
        JsonNode root = null;
        for (Map.Entry<String, JsonNode> entry : dataPath.node().properties()) {
            final String[] steps = StringUtils.split(entry.getKey(), separator);
            if (steps.length > 0) {
                root = funcUnflatten(root, steps, 0, entry.getValue());
            }
        }
        if (root != null) {
            funcUnflattenSort(root);
        }
        return path.push(root);
    }

    private static JsonNode funcUnflatten(JsonNode parent, final String[] steps, final int pos, JsonNode value) {
        final String step = steps[pos];
        final Integer index = parseInteger(step);
        if (parent == null) {
            parent = index == null ? MAPPER.createObjectNode() : MAPPER.createArrayNode();
        } else if ((index == null && parent.isArray()) || (index != null && parent.isObject())) {
            throw new UnsupportedOperationException("A node cannot be both an object and an array");
        }
        final boolean notEnd = pos < steps.length - 1;
        if (index == null) {
            return ((ObjectNode) parent).set(step, notEnd ? funcUnflatten(parent.get(step), steps, pos + 1, value) : value);
        }
        JsonNode v = null;
        if (notEnd) {
            for (int i = parent.size() - 1; i >= 0; i--) {
                final JsonNode elem = parent.get(i);
                if (elem.get("i").asInt() == index) {
                    v = elem.get("v");
                    break;
                }
            }
            value = funcUnflatten(v, steps, pos + 1, value);
        }
        if (v == null) {
            final ObjectNode elem = MAPPER.createObjectNode();
            elem.set("i", IntNode.valueOf(index));
            elem.set("v", value);
            ((ArrayNode) parent).add(elem);
        }
        return parent;
    }

    private static void funcUnflattenSort(final JsonNode node) {
        if (!node.isContainerNode()) {
            return;
        }
        if (node.isArray()) {
            final List<Pair<Integer, JsonNode>> list = new ArrayList<>();
            node.elements().forEachRemaining(elem -> list.add(Pair.of(elem.get("i").asInt(), elem.get("v"))));
            list.sort(Comparator.comparingInt(Pair::getKey));
            ((ArrayNode) node).removeAll();
            ((ArrayNode) node).addAll(list.stream().map(Pair::getValue).collect(Collectors.toList()));
        }
        node.elements().forEachRemaining(FuncStructural::funcUnflattenSort);
    }

    static PathTrace funcUnwind(final PathTrace path, final String params) {
        final Pair<PathTrace, List<String>> pathAndParams = getParamPathAndStrings(path, params, 1, 1);
        final PathTrace dataPath = pathAndParams.getKey();
        if (dataPath == null) {
            return null;
        }
        final String[] nameAndPath = new SyntaxDecomposer(pathAndParams.getValue().get(0)).deNameAndPath(null);
        if (nameAndPath[1] == null) {
            throw new SyntaxErrorException("Missing path '" + params + "'");
        }
        final ArrayNode unwind = MAPPER.createArrayNode();
        if (dataPath.isObject()) {
            funcUnwind(unwind, dataPath, evaluateNameAndPath(nameAndPath, dataPath, NON_ARRAY_INDEX));
        } else if (dataPath.isArray()) {
            for (int i = 0; i < dataPath.containerSize(); i++) {
                funcUnwind(unwind, dataPath.push(dataPath.get(i)), evaluateNameAndPath(nameAndPath, dataPath, i));
            }
        }
        return path.push(unwind);
    }

    private static void funcUnwind(final ArrayNode unwind, final PathTrace path, final String[] nameAndPath) {
        final JsonNode array = getNodeByExpression(path, nameAndPath[1]);
        if (array == null) {
            if (nameAndPath[2] != null) {
                unwind.add(path.node());
            }
            return;
        } else if (!array.isArray()) {
            return;
        }
        array.elements().forEachRemaining(
            elem -> {
                final ObjectNode object = MAPPER.createObjectNode();
                path.node().properties().forEach(field -> {
                    if (!field.getKey().equals(nameAndPath[1])) {
                        object.set(field.getKey(), field.getValue());
                    }
                });
                if (elem.isObject()) {
                    object.setAll((ObjectNode) elem);
                } else {
                    object.set(nameAndPath[0], elem);
                }
                unwind.add(object);
            }
        );
    }

    static PathTrace funcWrap(final PathTrace path, final String params) {
        final PathTrace paramPath = getParamPath(path, params);
        if (paramPath == null) {
            return null;
        }
        return path.push(MAPPER.createArrayNode().add(paramPath.node()));
    }
}

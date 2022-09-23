/*
 * Copyright 2020-2022 Octomix Software Technology Limited
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

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.*;
import com.octomix.josson.commons.StringUtils;
import com.octomix.josson.exception.SyntaxErrorException;

import java.util.*;
import java.util.stream.Collectors;

import static com.octomix.josson.FuncExecutor.*;
import static com.octomix.josson.Josson.readJsonNode;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Mapper.*;
import static com.octomix.josson.PatternMatcher.*;
import static com.octomix.josson.Utils.*;
import static com.octomix.josson.commons.StringUtils.EMPTY;

/**
 * Structural functions.
 */
final class FuncStructural {

    private FuncStructural() {
    }

    static JsonNode funcCollect(final JsonNode node, final String params) {
        final List<String> paramList = decomposeFunctionParameters(params, 1, UNLIMITED_AND_NO_PATH);
        final ArrayNode array = MAPPER.createArrayNode();
        paramList.forEach(param -> addArrayElement(array, getNodeByPath(node, param)));
        return array;
    }

    static JsonNode funcCumulateCollect(final JsonNode node, final String params) {
        final Pair<JsonNode, List<String>> nodeAndParams = getParamNodeAndStrings(node, params, 2, 2);
        final ArrayNode array = MAPPER.createArrayNode();
        funcCumulateCollect(array, nodeAndParams.getKey(), null, nodeAndParams.getValue().get(0), nodeAndParams.getValue().get(1));
        return array;
    }

    private static void funcCumulateCollect(final ArrayNode array, final JsonNode node, final Integer index,
                                            final String path, final String next) {
        final JsonNode tryNode = index == null ? node : getNodeByPath(node, index, next);
        if (tryNode == null || (index != null && Operator.EQ.relationalCompare(node, tryNode))) {
            return;
        }
        if (tryNode.isArray()) {
            for (int i = 0; i < tryNode.size(); i++) {
                addArrayElement(array, getNodeByPath(tryNode, i, path));
                funcCumulateCollect(array, tryNode, i, path, next);
            }
            return;
        }
        addArrayElement(array, getNodeByPath(tryNode, path));
        if (tryNode.isObject()) {
            funcCumulateCollect(array, tryNode, NON_ARRAY_INDEX, path, next);
        }
    }

    static JsonNode funcDepthLimit(final JsonNode node, final String params) {
        final Pair<JsonNode, List<String>> nodeAndParams = getParamNodeAndStrings(node, params, 1, 1);
        final JsonNode workNode = nodeAndParams.getKey();
        if (workNode == null) {
            return null;
        }
        final int depth = getNodeAsInt(node, nodeAndParams.getValue().get(0));
        if (depth < 1) {
            return null;
        }
        if (workNode.isObject()) {
            return deepCopy((ObjectNode) workNode, depth);
        }
        if (workNode.isArray()) {
            return deepCopy((ArrayNode) workNode, depth);
        }
        return workNode;
    }

    static JsonNode funcEntries(final JsonNode node, final String params) {
        final JsonNode workNode = getParamNode(node, params);
        if (workNode == null || !workNode.isContainerNode()) {
            return null;
        }
        final ArrayNode array = MAPPER.createArrayNode();
        if (workNode.isArray()) {
            workNode.forEach(elem -> elem.fields()
                .forEachRemaining(field ->
                    array.add(Josson.createObjectNode().put(ENTRY_KEY_NAME, field.getKey()).set(ENTRY_VALUE_NAME, field.getValue()))));
        } else {
            workNode.fields().forEachRemaining(field ->
                array.add(Josson.createObjectNode().put(ENTRY_KEY_NAME, field.getKey()).set(ENTRY_VALUE_NAME, field.getValue())));
        }
        return array;
    }

    static JsonNode funcField(final JsonNode node, final String params) {
        final Map<String, String> args = getParamNamePath(decomposeFunctionParameters(params, 1, UNLIMITED_WITH_PATH));
        if (node.isObject()) {
            return funcMap(cloneObject((ObjectNode) node), node, args, NON_ARRAY_INDEX);
        }
        if (node.isArray()) {
            final ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                final JsonNode elem = node.get(i);
                array.add(elem.isObject() ? funcMap(cloneObject((ObjectNode) elem), node, args, i) : null);
            }
            return array;
        }
        return null;
    }

    static JsonNode funcFlatten(final JsonNode node, final String params) {
        final Pair<JsonNode, List<String>> nodeAndParams = getParamNodeAndStrings(node, params, 0, 2);
        final JsonNode workNode = nodeAndParams.getKey();
        if (workNode != null && workNode.isContainerNode()) {
            final List<String> paramList = nodeAndParams.getValue();
            final JsonNode param = paramList.size() > 0 ? getNodeByPath(node, paramList.get(0)) : null;
            final JsonNode indexFormat = paramList.size() > 1 ? getNodeByPath(node, paramList.get(1)) : null;
            if (param != null && (param.isTextual() || param.isNull() || !nodeIsNull(indexFormat))) {
                final ObjectNode object = MAPPER.createObjectNode();
                funcFlatten(object, null, workNode, param.isNull() ? null : param.asText(),
                        nodeIsNull(indexFormat) ? null : indexFormat.asText());
                return object;
            }
            if (workNode.isArray()) {
                final ArrayNode array = MAPPER.createArrayNode();
                funcFlatten(array, workNode, param == null ? 0 : param.asInt());
                return array;
            }
        }
        return workNode;
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
            node.fields().forEachRemaining(elem ->
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

    static JsonNode funcGroup(final JsonNode node, final String params) {
        final Pair<JsonNode, List<String>> nodeAndParams = getParamNodeAndStrings(node, params, 1, 2);
        final JsonNode workNode = nodeAndParams.getKey();
        if (workNode == null || !workNode.isArray()) {
            return null;
        }
        final List<String> paramList = nodeAndParams.getValue();
        String[] nameAndPath;
        try {
            nameAndPath = decomposeNameAndPath(paramList.get(0));
        } catch (UnknownFormatConversionException e) {
            nameAndPath = new String[]{ENTRY_KEY_NAME, paramList.get(0)};
        }
        String[] grouping;
        if (paramList.size() > 1) {
            try {
                grouping = decomposeNameAndPath(paramList.get(1));
            } catch (UnknownFormatConversionException e) {
                grouping = new String[]{GROUP_VALUE_NAME, paramList.get(1)};
            }
        } else {
            grouping = new String[]{GROUP_VALUE_NAME, null};
        }
        final ArrayNode array = MAPPER.createArrayNode();
        for (int i = 0; i < workNode.size(); i++) {
            final String[] evalNameAndPath = evaluateNameAndPath(nameAndPath, workNode, i);
            final JsonNode valueNode = getNodeByPath(workNode, i, evalNameAndPath[1] == null
                    ? evalNameAndPath[0] : evalNameAndPath[1]);
            if (valueNode != null) {
                ArrayNode values = null;
                final String[] evalGrouping = evaluateNameAndPath(grouping, workNode, i);
                for (JsonNode elem : array) {
                    if (Operator.EQ.relationalCompare(elem.get(evalNameAndPath[0]), valueNode)) {
                        values = (ArrayNode) elem.get(evalGrouping[0]);
                        break;
                    }
                }
                if (values == null) {
                    values = MAPPER.createArrayNode();
                    final ObjectNode entry = Josson.createObjectNode();
                    entry.set(evalNameAndPath[0], valueNode);
                    entry.set(evalGrouping[0], values);
                    array.add(entry);
                }
                values.add(evalGrouping[1] == null ? workNode.get(i) : getNodeByPath(workNode, i, evalGrouping[1]));
            }
        }
        return array;
    }

    static JsonNode funcJson(final JsonNode node, final String params) {
        return applyWithoutParam(node, params, JsonNode::isTextual,
            (data, paramList) -> {
                try {
                    return readJsonNode(data.getKey().asText());
                } catch (JsonProcessingException e) {
                    throw new IllegalArgumentException(e.getMessage());
                }
            });
    }

    static JsonNode funcKeys(final JsonNode node, final String params) {
        final Pair<JsonNode, List<String>> nodeAndParams = getParamNodeAndStrings(node, params, 0, 1);
        final JsonNode workNode = nodeAndParams.getKey();
        if (workNode == null || !workNode.isObject()) {
            return null;
        }
        final ArrayNode array = MAPPER.createArrayNode();
        funcKeys(array, workNode,
                nodeAndParams.getValue().size() > 0 ? getNodeAsInt(workNode, nodeAndParams.getValue().get(0)) : 1);
        return array;
    }

    private static void funcKeys(final ArrayNode array, final JsonNode node, final int levels) {
        node.fields().forEachRemaining(field -> {
            array.add(field.getKey());
            if (levels != 1 && field.getValue().isObject()) {
                funcKeys(array, field.getValue(), levels - 1);
            }
        });
    }

    static JsonNode funcMap(final JsonNode node, final String params) {
        final Map<String, String> args = getParamNamePath(decomposeFunctionParameters(params, 1, UNLIMITED_WITH_PATH));
        if (!node.isArray()) {
            return funcMap(MAPPER.createObjectNode(), node, args, NON_ARRAY_INDEX);
        }
        final ArrayNode array = MAPPER.createArrayNode();
        for (int i = 0; i < node.size(); i++) {
            array.add(funcMap(MAPPER.createObjectNode(), node, args, i));
        }
        return array;
    }

    private static ObjectNode funcMap(final ObjectNode base, final JsonNode node,
                                      final Map<String, String> args, final int index) {
        for (Map.Entry<String, String> arg : args.entrySet()) {
            final String[] evalNameAndPath = evaluateNameAndPath(new String[]{arg.getKey(), arg.getValue()}, node, index);
            if (evalNameAndPath[1] == null) {
                base.remove(evalNameAndPath[0]);
            } else {
                final JsonNode tryNode = getNodeByPath(node, index, evalNameAndPath[1]);
                if (tryNode == null) {
                    base.remove(evalNameAndPath[0]);
                } else {
                    base.set(evalNameAndPath[0], tryNode);
                }
            }
        }
        return base;
    }

    static ObjectNode funcMergeObjects(final JsonNode node, final String params) {
        final ArrayNode array = getParamArrayOrItself(node, params);
        if (array == null) {
            return null;
        }
        ObjectNode result = null;
        for (JsonNode tryNode : array) {
            if (tryNode.isObject()) {
                if (result == null) {
                    result = tryNode.deepCopy();
                } else {
                    mergeObjects(result, tryNode);
                }
            }
        }
        return result;
    }

    static JsonNode funcToArray(final JsonNode node, final String params) {
        final JsonNode container = getParamArrayOrItselfIsContainer(node, params);
        if (container == null) {
            return null;
        }
        final ArrayNode array = MAPPER.createArrayNode();
        if (container.isArray()) {
            container.forEach(elem -> {
                if (elem.isArray()) {
                    array.addAll((ArrayNode) elem);
                } else if (elem.isObject()) {
                    elem.forEach(array::add);
                } else {
                    array.add(elem);
                }
            });
        } else {
            container.forEach(array::add);
        }
        return array;
    }

    static JsonNode funcToObject(final JsonNode node, final String params) {
        final Pair<JsonNode, List<String>> nodeAndParams = getParamNodeAndStrings(node, params, 1, 1);
        final JsonNode workNode = nodeAndParams.getKey();
        if (workNode == null) {
            return null;
        }
        final List<String> paramList = nodeAndParams.getValue();
        return MAPPER.createObjectNode().set(getNodeAsText(node, paramList.get(0)), workNode);
    }

    static JsonNode funcUnflatten(final JsonNode node, final String params) {
        final Pair<JsonNode, List<String>> nodeAndParams = getParamNodeAndStrings(node, params, 1, 1);
        final JsonNode workNode = nodeAndParams.getKey();
        if (workNode == null || !workNode.isContainerNode()) {
            return null;
        }
        final String separator = getNodeAsText(node, nodeAndParams.getValue().get(0));
        if (StringUtils.isEmpty(separator)) {
            return null;
        }
        JsonNode root = null;
        final Iterator<Map.Entry<String, JsonNode>> iterator = workNode.fields();
        while (iterator.hasNext()) {
            final Map.Entry<String, JsonNode> entry = iterator.next();
            final String[] steps = StringUtils.split(entry.getKey(), separator);
            if (steps.length > 0) {
                root = funcUnflatten(root, steps, 0, entry.getValue());
            }
        }
        if (root != null) {
            funcUnflattenSort(root);
        }
        return root;
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
            ((ObjectNode) parent).set(step, notEnd ? funcUnflatten(parent.get(step), steps, pos + 1, value) : value);
        } else {
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
                ObjectNode elem = MAPPER.createObjectNode();
                elem.set("i", IntNode.valueOf(index));
                elem.set("v", value);
                ((ArrayNode) parent).add(elem);
            }
        }
        return parent;
    }

    private static void funcUnflattenSort(JsonNode node) {
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

    static JsonNode funcUnwind(final JsonNode node, final String params) {
        final Pair<JsonNode, List<String>> nodeAndParams = getParamNodeAndStrings(node, params, 1, 1);
        final JsonNode workNode = nodeAndParams.getKey();
        if (workNode == null) {
            return null;
        }
        final String[] nameAndPath = decomposeNameAndPath(nodeAndParams.getValue().get(0));
        if (nameAndPath[1] == null) {
            throw new SyntaxErrorException("Missing path '" + params + "'");
        }
        final ArrayNode unwind = MAPPER.createArrayNode();
        if (workNode.isObject()) {
            funcUnwind(unwind, workNode, evaluateNameAndPath(nameAndPath, workNode, NON_ARRAY_INDEX));
        } else if (workNode.isArray()) {
            for (int i = 0; i < workNode.size(); i++) {
                funcUnwind(unwind, workNode.get(i), evaluateNameAndPath(nameAndPath, workNode, i));
            }
        }
        return unwind;
    }

    private static void funcUnwind(final ArrayNode unwind, final JsonNode node, final String[] nameAndPath) {
        final JsonNode array = getNodeByPath(node, nameAndPath[1]);
        if (!array.isArray()) {
            return;
        }
        array.elements().forEachRemaining(
            element -> {
                final ObjectNode object = MAPPER.createObjectNode();
                node.fields().forEachRemaining(field -> {
                    if (!field.getKey().equals(nameAndPath[1])) {
                        object.set(field.getKey(), field.getValue());
                    }
                });
                if (element.isObject()) {
                    object.setAll((ObjectNode) element);
                } else {
                    object.set(nameAndPath[0], element);
                }
                unwind.add(object);
            }
        );
    }

    static JsonNode funcWrap(final JsonNode node, final String params) {
        final JsonNode workNode = getParamNode(node, params);
        if (workNode == null) {
            return null;
        }
        return MAPPER.createArrayNode().add(workNode);
    }
}

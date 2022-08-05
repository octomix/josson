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
import com.octomix.josson.exception.SyntaxErrorException;

import java.util.List;
import java.util.Map;
import java.util.UnknownFormatConversionException;

import static com.octomix.josson.FuncExecutor.*;
import static com.octomix.josson.Josson.readJsonNode;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Mapper.*;
import static com.octomix.josson.PatternMatcher.*;

/**
 * Structural functions.
 */
final class FuncStructural {

    private FuncStructural() {
    }

    static JsonNode funcEntries(final JsonNode node, final String params) {
        final JsonNode workNode = getParamNode(node, params);
        if (workNode == null || !workNode.isContainerNode()) {
            return null;
        }
        final ArrayNode array = MAPPER.createArrayNode();
        if (workNode.isArray()) {
            workNode.forEach(elem -> elem.fields()
                .forEachRemaining((Map.Entry<String, JsonNode> field) ->
                    array.add(Josson.createObjectNode().put("key", field.getKey()).set("value", field.getValue()))));
        } else {
            workNode.fields().forEachRemaining((Map.Entry<String, JsonNode> field) ->
                array.add(Josson.createObjectNode().put("key", field.getKey()).set("value", field.getValue())));
        }
        return array;
    }

    static JsonNode funcField(final JsonNode node, final String params) {
        final Map<String, String> args = getParamNamePath(decomposeFunctionParameters(params, 1, UNLIMITED_WITH_PATH));
        if (node.isObject()) {
            return funcMap(cloneObjectNode((ObjectNode) node), node, args, NON_ARRAY_INDEX);
        }
        if (node.isArray()) {
            final ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                final JsonNode elem = node.get(i);
                array.add(elem.isObject() ? funcMap(cloneObjectNode((ObjectNode) elem), node, args, i) : null);
            }
            return array;
        }
        return null;
    }

    static JsonNode funcFlatten(final JsonNode node, final String params) {
        final Pair<JsonNode, List<String>> nodeAndParams = getParamNodeAndStrings(node, params, 0, 1);
        final JsonNode workNode = nodeAndParams.getKey();
        if (workNode == null || !workNode.isArray()) {
            return workNode;
        }
        final int levels = nodeAndParams.getValue().size() > 0 ? getNodeAsInt(node, nodeAndParams.getValue().get(0)) : 1;
        if (levels < 1) {
            return workNode;
        }
        final ArrayNode array = MAPPER.createArrayNode();
        funcFlatten(array, workNode, levels);
        return array;
    }

    private static void funcFlatten(final ArrayNode array, final JsonNode node, final int levels) {
        for (int i = 0; i < node.size(); i++) {
            if (node.get(i).isArray()) {
                if (levels == 1) {
                    array.addAll((ArrayNode) node.get(i));
                } else {
                    funcFlatten(array, node.get(i), levels - 1);
                }
            } else {
                array.add(node.get(i));
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
        Pair<String, String> nameAndPath;
        try {
            nameAndPath = decomposeNameAndPath(paramList.get(0));
        } catch (UnknownFormatConversionException e) {
            nameAndPath = Pair.of("key", paramList.get(0));
        }
        Pair<String, String> grouping;
        if (paramList.size() > 1) {
            try {
                grouping = decomposeNameAndPath(paramList.get(1));
            } catch (UnknownFormatConversionException e) {
                grouping = Pair.of("elements", paramList.get(1));
            }
        } else {
            grouping = Pair.of("elements", null);
        }
        final ArrayNode array = MAPPER.createArrayNode();
        for (int i = 0; i < workNode.size(); i++) {
            final Pair<String, String> evalNameAndPath = evaluateNameAndPath(nameAndPath, workNode, i);
            final JsonNode valueNode = getNodeByPath(workNode, i, evalNameAndPath.getValue() == null
                    ? evalNameAndPath.getKey() : evalNameAndPath.getValue());
            if (valueNode != null && valueNode.isValueNode()) {
                ArrayNode values = null;
                final Pair<String, String> evalGrouping = evaluateNameAndPath(grouping, workNode, i);
                for (int j = 0; j < array.size(); j++) {
                    if (array.get(j).get(evalNameAndPath.getKey()).equals(valueNode)) {
                        values = (ArrayNode) array.get(j).get(evalGrouping.getKey());
                        break;
                    }
                }
                if (values == null) {
                    values = MAPPER.createArrayNode();
                    final ObjectNode entry = Josson.createObjectNode();
                    entry.set(evalNameAndPath.getKey(), valueNode);
                    entry.set(evalGrouping.getKey(), values);
                    array.add(entry);
                }
                values.add(evalGrouping.getValue() == null ? workNode.get(i) : getNodeByPath(workNode, i, evalGrouping.getValue()));
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
        node.fields().forEachRemaining((Map.Entry<String, JsonNode> field) -> {
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
            final Pair<String, String> evalNameAndPath = evaluateNameAndPath(Pair.of(arg.getKey(), arg.getValue()), node, index);
            if (evalNameAndPath.getValue() == null) {
                base.remove(evalNameAndPath.getKey());
            } else {
                base.set(evalNameAndPath.getKey(), getNodeByPath(node, index, evalNameAndPath.getValue()));
            }
        }
        return base;
    }

    static JsonNode funcToArray(final JsonNode node, final String params) {
        final JsonNode container = getParamArrayOrItselfIsContainer(node, params);
        if (container == null) {
            return null;
        }
        final ArrayNode array = MAPPER.createArrayNode();
        if (container.isArray()) {
            for (int i = 0; i < container.size(); i++) {
                if (container.get(i).isArray()) {
                    array.addAll((ArrayNode) container.get(i));
                } else if (container.get(i).isObject()) {
                    container.get(i).forEach(array::add);
                } else {
                    array.add(container.get(i));
                }
            }
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
        return MAPPER.createObjectNode().set(getNodeAsText(workNode, NON_ARRAY_INDEX, paramList.get(0)), workNode);
    }

    static JsonNode funcUnwind(final JsonNode node, final String params) {
        final Pair<JsonNode, List<String>> nodeAndParams = getParamNodeAndStrings(node, params, 1, 1);
        final JsonNode workNode = nodeAndParams.getKey();
        if (workNode == null) {
            return null;
        }
        final Pair<String, String> nameAndPath = decomposeNameAndPath(nodeAndParams.getValue().get(0));
        if (nameAndPath.getValue() == null) {
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

    private static void funcUnwind(final ArrayNode unwind, final JsonNode node, final Pair<String, String> nameAndPath) {
        final JsonNode array = getNodeByPath(node, nameAndPath.getValue());
        if (!array.isArray()) {
            return;
        }
        array.elements().forEachRemaining(
                element -> {
                    final ObjectNode object = MAPPER.createObjectNode();
                    node.fields().forEachRemaining((Map.Entry<String, JsonNode> field) -> {
                        if (!field.getKey().equals(nameAndPath.getValue())) {
                            object.set(field.getKey(), field.getValue());
                        }
                    });
                    if (element.isObject()) {
                        object.setAll((ObjectNode) element);
                    } else {
                        object.set(nameAndPath.getKey(), element);
                    }
                    unwind.add(object);
                }
        );
    }
}

/*
 * Copyright 2020-2024 Choi Wai Man Raymond
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
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;

import java.text.DateFormat;
import java.util.HashMap;
import java.util.Map;

/**
 * Extends Jackson object mapper.
 */
class Mapper extends ObjectMapper {

    static final Mapper MAPPER = new Mapper();

    Mapper() {
        this.registerModule(new JavaTimeModule());
        this.setDateFormat(DateFormat.getInstance());
    }

    /**
     * Get the static instance of Josson's ObjectMapper.
     *
     * @return the static instance of Josson's ObjectMapper.
     */
    public static Mapper mapper() {
        return MAPPER;
    }

    static ObjectNode cloneObject(final ObjectNode objectNode) {
        return MAPPER.createObjectNode().setAll(objectNode);
    }

    static ArrayNode cloneArray(final ArrayNode arrayNode) {
        return MAPPER.createArrayNode().addAll(arrayNode);
    }

    static ArrayNode intoNewArray(final JsonNode jsonNode) {
        return MAPPER.createArrayNode().add(jsonNode);
    }

    static ObjectNode deepCopy(final ObjectNode object, final int depth) {
        final ObjectNode copy = MAPPER.createObjectNode();
        object.fields().forEachRemaining(entry -> {
            final JsonNode value;
            if (entry.getValue().isValueNode()) {
                value = entry.getValue();
            } else if (depth <= 1) {
                return;
            } else if (entry.getValue().isObject()) {
                value = deepCopy((ObjectNode) entry.getValue(), depth - 1);
            } else {
                value = deepCopy((ArrayNode) entry.getValue(), depth - 1);
            }
            copy.set(entry.getKey(), value);
        });
        return copy;
    }

    static ArrayNode deepCopy(final ArrayNode array, final int depth) {
        final ArrayNode copy = MAPPER.createArrayNode();
        array.forEach(elem -> {
            if (elem.isValueNode()) {
                copy.add(elem);
            } else if (elem.isObject()) {
                copy.add(deepCopy((ObjectNode) elem, depth));
            } else if (depth > 0) {
                copy.add(deepCopy((ArrayNode) elem, depth - 1));
            }
        });
        return copy;
    }

    static JsonNode struCopy(final JsonNode node, JsonNode structure, boolean remove) {
        if (!remove && structure != null && structure.asBoolean()) {
            structure = null;
            remove = true;
        }
        return node.isObject() ? struCopy((ObjectNode) node, structure, remove) : struCopy((ArrayNode) node, structure, remove);
    }

    static ObjectNode struCopy(final ObjectNode object, final JsonNode structure, final boolean remove) {
        final ObjectNode copy = MAPPER.createObjectNode();
        object.fields().forEachRemaining(entry -> {
            final JsonNode struNode = structure != null && structure.isObject() ? structure.get(entry.getKey()) : null;
            if (struNode == null ? remove : !remove || !struNode.asBoolean()) {
                final JsonNode value = entry.getValue();
                copy.set(entry.getKey(), value.isValueNode() ? value : struCopy(value, struNode, remove));
            }
        });
        return copy;
    }

    static ArrayNode struCopy(final ArrayNode array, final JsonNode structure, final boolean remove) {
        final Map<Integer, JsonNode> find = new HashMap<>();
        if (structure != null && structure.isArray()) {
            structure.forEach(struNode -> find.put(struNode.get("i").asInt(), struNode.get("v")));
        }
        final ArrayNode copy = MAPPER.createArrayNode();
        for (int i = 0; i < array.size(); i++) {
            final JsonNode struNode = find.remove(i);
            if (struNode == null ? remove : !remove || !struNode.asBoolean()) {
                final JsonNode value = array.get(i);
                copy.add(value.isValueNode() ? value : struCopy(value, struNode, remove));
            }
        }
        return copy;
    }
}

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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;

import java.text.DateFormat;

/**
 * Extends Jackson object mapper.
 */
class Mapper extends ObjectMapper {

    static final Mapper MAPPER = new Mapper();

    Mapper() {
        this.registerModule(new JavaTimeModule());
        this.setDateFormat(DateFormat.getInstance());
    }

    static ObjectNode cloneObjectNode(final ObjectNode objectNode) {
        return MAPPER.createObjectNode().setAll(objectNode);
    }

    static ArrayNode cloneArrayNode(final ArrayNode arrayNode) {
        return MAPPER.createArrayNode().addAll(arrayNode);
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
}

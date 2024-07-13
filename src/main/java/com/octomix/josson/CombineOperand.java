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
import com.octomix.josson.commons.StringUtils;

import java.util.UnknownFormatConversionException;
import java.util.function.Function;

import static com.octomix.josson.Utils.getLastElementName;

/**
 * Defines dataset for join and set operation.
 */
final class CombineOperand {

    private final String query;

    private final String[] keys;

    private JsonNode node;

    CombineOperand(final String query, final String[] keys) {
        this.query = query;
        this.keys = keys;
    }

    String[] getKeys() {
        return keys;
    }

    JsonNode getNode() {
        return node;
    }

    void setNode(JsonNode node) {
        this.node = node;
    }

    boolean hasNoQuery() {
        return query.isEmpty();
    }

    String resolveArrayName() {
        final int pos = keys[0].indexOf(':');
        if (pos < 0) {
            try {
                return getLastElementName(query);
            } catch (UnknownFormatConversionException e) {
                return e.getConversion();
            }
        }
        final String arrayName = StringUtils.strip(keys[0].substring(0, pos));
        keys[0] = keys[0].substring(pos + 1);
        return arrayName;
    }

    void apply(final Function<String, JsonNode> evaluateQuery) {
        node = evaluateQuery.apply(query);
        if (node == null) {
            throw new IllegalArgumentException("unresolvable " + query);
        }
        if (!node.isContainerNode()) {
            throw new IllegalArgumentException("result is not a container node " + query);
        }
    }
}

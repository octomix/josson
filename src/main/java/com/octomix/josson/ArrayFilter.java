/*
 * Copyright 2020-2023 Octomix Software Technology Limited
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
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.octomix.josson.commons.StringUtils;

import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.Utils.asBoolean;
import static com.octomix.josson.Utils.parseInteger;

/**
 * Stores an array filter details.
 */
class ArrayFilter {

    /**
     * Filter modes.
     */
    enum FilterMode {

        /**
         * Query first matching element.
         */
        FILTRATE_FIND_FIRST(' '),

        /**
         * Query all matching elements and output them inside an array node.
         */
        FILTRATE_COLLECT_ALL('*'),

        /**
         * Query all matching elements and divert each element to separate branch for upcoming manipulation.
         */
        FILTRATE_DIVERT_ALL('@');

        private final char symbol;

        FilterMode(final char symbol) {
            this.symbol = symbol;
        }

        static FilterMode fromSymbol(final char symbol) {
            for (FilterMode mode : values()) {
                if (mode.symbol == symbol) {
                    return mode;
                }
            }
            return null;
        }

        char getSymbol() {
            return symbol;
        }

        boolean equals(final char ch) {
            return symbol == ch;
        }
    }

    private final String nodeName;

    private final String filter;

    private final FilterMode mode;

    ArrayFilter(final String nodeName, final String filter, final FilterMode mode) {
        this.nodeName = nodeName;
        this.filter = filter;
        this.mode = mode;
    }

    String getNodeName() {
        return nodeName;
    }

    String getFilter() {
        return filter;
    }

    FilterMode getMode() {
        return mode;
    }

    /**
     * Find an element or filter an array node.
     *
     * @param path      the Jackson JsonNode to be processed
     * @param statement multiple relational operations combined with logical operators
     * @return The 1st matched element for {@code FILTRATE_FIRST_FOUND} or
     *         all matched elements in an array node for {@code FILTRATE_COLLECT_ALL} and {@code FILTRATE_DIVERT_ALL}
     */
    JsonNode evaluateFilter(final PathTrace path, final String statement) {
        if (path == null) {
            return null;
        }
        final JsonNode node = path.node();
        if (StringUtils.isEmpty(statement)) {
            return node;
        }
        if (!node.isArray()) {
            return asBoolean(new OperationStackForJsonNode(path).evaluateStatement(statement)) ? node : null;
        }
        if (node.size() == 0) {
            return null;
        }
        final ArrayNode matchedNodes = mode == FilterMode.FILTRATE_FIND_FIRST ? null : MAPPER.createArrayNode();
        final Integer index = parseInteger(statement);
        if (index != null) {
            if (matchedNodes == null) {
                return node.get(index);
            }
            matchedNodes.add(node.get(index));
        } else {
            final OperationStack opStack = new OperationStackForJsonNode(path);
            for (int i = 0; i < node.size(); i++) {
                if (asBoolean(opStack.evaluate(statement, i))) {
                    if (matchedNodes == null) {
                        return node.get(i);
                    }
                    matchedNodes.add(node.get(i));
                }
            }
        }
        return matchedNodes;
    }
}

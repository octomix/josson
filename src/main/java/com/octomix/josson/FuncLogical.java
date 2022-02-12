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
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.BooleanNode;
import com.octomix.josson.commons.StringUtils;

import java.time.chrono.IsoChronology;
import java.time.temporal.ChronoField;
import java.util.Iterator;
import java.util.List;

import static com.octomix.josson.FuncExecutor.*;
import static com.octomix.josson.JossonCore.*;

class FuncLogical {

    private FuncLogical() {
    }

    static BooleanNode funcContains(JsonNode node, final String params, final boolean ignoreCase, final boolean not) {
        final Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        final JsonNode valueNode = getNodeByPath(node, pathAndParams.getValue().get(0));
        if (valueNode == null || valueNode.isContainerNode()) {
            return BooleanNode.FALSE;
        }
        if (pathAndParams.hasKey()) {
            node = getNodeByPath(node, pathAndParams.getKey());
            if (node == null) {
                return BooleanNode.FALSE;
            }
        }
        if (valueNode.isNumber()) {
            final double value = valueNode.asDouble();
            if (node.isArray()) {
                for (int i = 0; i < node.size(); i++) {
                    final JsonNode elem = node.get(i);
                    if ((elem.isNumber() || elem.isTextual()) && elem.asDouble() == value) {
                        return BooleanNode.valueOf(!not);
                    }
                }
            }
            return BooleanNode.valueOf(not);
        }
        if (valueNode.isNull()) {
            if (node.isArray()) {
                for (int i = 0; i < node.size(); i++) {
                    if (node.get(i).isNull()) {
                        return BooleanNode.valueOf(!not);
                    }
                }
            }
            return BooleanNode.valueOf(not);
        }
        final String value = valueNode.asText();
        if (node.isObject()) {
            if (ignoreCase) {
                for (Iterator<String> it = node.fieldNames(); it.hasNext();) {
                    if (value.equalsIgnoreCase(it.next())) {
                        return BooleanNode.valueOf(!not);
                    }
                }
                return BooleanNode.valueOf(not);
            }
            return BooleanNode.valueOf(not ^ node.get(value) != null);
        }
        if (node.isArray()) {
            for (int i = 0; i < node.size(); i++) {
                final JsonNode elem = node.get(i);
                if (elem.isTextual() || elem.isNumber()) {
                    if (ignoreCase) {
                        if (value.equalsIgnoreCase(elem.asText())) {
                            return BooleanNode.valueOf(!not);
                        }
                    } else if (value.equals(elem.asText())) {
                        return BooleanNode.valueOf(!not);
                    }
                }
            }
            return BooleanNode.valueOf(not);
        }
        return BooleanNode.valueOf(not ^ (ignoreCase
                ? StringUtils.containsIgnoreCase(node.asText(), value)
                : StringUtils.contains(node.asText(), value)));
    }

    static JsonNode funcEndsWith(final JsonNode node, final String params,
                                 final boolean ignoreCase, final boolean not) {
        return applyFuncWithParamAsText(node, params,
                JsonNode::isTextual,
                (jsonNode, objVar) -> BooleanNode.valueOf(not ^ (ignoreCase
                        ? StringUtils.endsWithIgnoreCase(jsonNode.asText(), (String) objVar)
                        : StringUtils.endsWith(jsonNode.asText(), (String) objVar)))
        );
    }

    static JsonNode funcEquals(final JsonNode node, final String params, final boolean ignoreCase, final boolean not) {
        return applyFuncWithParamAsText(node, params,
                JsonNode::isTextual,
                (jsonNode, objVar) -> BooleanNode.valueOf(not ^ (ignoreCase
                        ? StringUtils.equalsIgnoreCase(jsonNode.asText(), (String) objVar)
                        : StringUtils.equals(jsonNode.asText(), (String) objVar)))
        );
    }

    static BooleanNode funcIn(final JsonNode node, final String params, final boolean ignoreCase, final boolean not) {
        final ArrayNode array = getParamArray(params, node);
        if (node.isNumber()) {
            final double num = node.asDouble();
            for (int i = array.size() - 1; i >= 0; i--) {
                final JsonNode value = array.get(i);
                if ((value.isNumber() || value.isTextual()) && value.asDouble() == num) {
                    return BooleanNode.valueOf(!not);
                }
            }
            return BooleanNode.valueOf(not);
        } else if (node.isTextual()) {
            final String text = node.asText();
            for (int i = array.size() - 1; i >= 0; i--) {
                final JsonNode value = array.get(i);
                if (value.isTextual() || value.isNumber()) {
                    if (ignoreCase) {
                        if (value.asText().equalsIgnoreCase(text)) {
                            return BooleanNode.valueOf(!not);
                        }
                    } else if (value.asText().equals(text)) {
                        return BooleanNode.valueOf(!not);
                    }
                }
            }
            return BooleanNode.valueOf(not);
        }
        return BooleanNode.FALSE;
    }

    static JsonNode funcIsBlank(final JsonNode node, final String params, final boolean not) {
        return applyFunc(node, params,
                jsonNode -> BooleanNode.valueOf(jsonNode.isTextual() && (not ^ StringUtils.isBlank(jsonNode.asText())))
        );
    }

    static JsonNode funcIsBoolean(final JsonNode node, final String params) {
        return applyFunc(node, params,
                jsonNode -> BooleanNode.valueOf(jsonNode.isBoolean())
        );
    }

    static JsonNode funcIsEmpty(final JsonNode node, final String params, final boolean not) {
        return applyFunc(node, params,
                jsonNode -> BooleanNode.valueOf(jsonNode.isTextual() && (not ^ jsonNode.asText().isEmpty()))
        );
    }

    static JsonNode funcIsEven(final JsonNode node, final String params) {
        return applyFunc(node, params,
                jsonNode -> nodeHasValue(jsonNode)
                        ? BooleanNode.valueOf((jsonNode.asInt() & 1) == 0) : BooleanNode.FALSE
        );
    }

    static JsonNode funcIsNull(final JsonNode node, final String params, final boolean not) {
        return applyFunc(node, params,
                jsonNode -> BooleanNode.valueOf(not ^ jsonNode.isNull())
        );
    }

    static JsonNode funcIsNumber(final JsonNode node, final String params) {
        return applyFunc(node, params,
                jsonNode -> BooleanNode.valueOf(jsonNode.isNumber())
        );
    }

    static JsonNode funcIsOdd(final JsonNode node, final String params) {
        return applyFunc(node, params,
                jsonNode -> nodeHasValue(jsonNode)
                        ? BooleanNode.valueOf((jsonNode.asInt() & 1) != 0) : BooleanNode.FALSE
        );
    }

    static JsonNode funcIsText(final JsonNode node, final String params) {
        return applyFunc(node, params,
                jsonNode -> BooleanNode.valueOf(jsonNode.isTextual())
        );
    }

    static JsonNode funcNot(final JsonNode node, final String params) {
        return applyFunc(node, params,
                jsonNode -> jsonNode.isBoolean() ? BooleanNode.valueOf(!jsonNode.asBoolean()) : BooleanNode.FALSE
        );
    }

    static JsonNode funcStartsWith(final JsonNode node, final String params,
                                   final boolean ignoreCase, final boolean not) {
        return applyFuncWithParamAsText(node, params,
                JsonNode::isTextual,
                (jsonNode, objVar) -> BooleanNode.valueOf(not ^ (ignoreCase ?
                        StringUtils.startsWithIgnoreCase(jsonNode.asText(), (String) objVar) :
                        StringUtils.startsWith(jsonNode.asText(), (String) objVar)))
        );
    }

    static JsonNode funcIsWeekday(final JsonNode node, final String params) {
        return applyFunc(node, params,
                jsonNode -> jsonNode.isTextual()
                        ? BooleanNode.valueOf(toLocalDateTime(jsonNode).get(ChronoField.DAY_OF_WEEK) <= 5)
                        : null
        );
    }

    static JsonNode funcIsWeekend(final JsonNode node, final String params) {
        return applyFunc(node, params,
                jsonNode -> jsonNode.isTextual()
                        ? BooleanNode.valueOf(toLocalDateTime(jsonNode).get(ChronoField.DAY_OF_WEEK) > 5)
                        : null
        );
    }

    static JsonNode funcIsLeapYear(final JsonNode node, final String params) {
        return applyFunc(node, params,
                jsonNode -> jsonNode.isTextual()
                        ? BooleanNode.valueOf(IsoChronology.INSTANCE.isLeapYear(toLocalDateTime(jsonNode).getYear()))
                        : null
        );
    }
}

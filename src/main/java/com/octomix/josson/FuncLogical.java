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
import com.fasterxml.jackson.databind.node.BooleanNode;
import com.octomix.josson.commons.StringUtils;

import java.time.chrono.IsoChronology;
import java.time.temporal.ChronoField;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Pattern;

import static com.octomix.josson.FuncExecutor.*;
import static com.octomix.josson.JossonCore.*;

/**
 * Logical functions.
 */
final class FuncLogical {

    private FuncLogical() {
    }

    static BooleanNode funcContains(final JsonNode node, final String params, final boolean ignoreCase, final boolean not) {
        final Pair<JsonNode, List<String>> nodeAndParams = getParamNodeAndStrings(node, params, 1, 1);
        final JsonNode workNode = nodeAndParams.getKey();
        if (workNode == null) {
            return BooleanNode.FALSE;
        }
        final JsonNode valueNode = getNodeByPath(node, nodeAndParams.getValue().get(0));
        if (valueNode == null || valueNode.isContainerNode()) {
            return BooleanNode.FALSE;
        }
        if (valueNode.isNumber()) {
            final double value = valueNode.asDouble();
            if (workNode.isArray()) {
                for (int i = 0; i < workNode.size(); i++) {
                    final JsonNode elem = workNode.get(i);
                    if ((elem.isNumber() || elem.isTextual()) && elem.asDouble() == value) {
                        return BooleanNode.valueOf(!not);
                    }
                }
            }
            return BooleanNode.valueOf(not);
        }
        if (valueNode.isNull()) {
            if (workNode.isArray()) {
                for (int i = 0; i < workNode.size(); i++) {
                    if (workNode.get(i).isNull()) {
                        return BooleanNode.valueOf(!not);
                    }
                }
            }
            return BooleanNode.valueOf(not);
        }
        final String value = valueNode.asText();
        if (workNode.isObject()) {
            if (ignoreCase) {
                for (Iterator<String> it = workNode.fieldNames(); it.hasNext();) {
                    if (value.equalsIgnoreCase(it.next())) {
                        return BooleanNode.valueOf(!not);
                    }
                }
                return BooleanNode.valueOf(not);
            }
            return BooleanNode.valueOf(not ^ workNode.get(value) != null);
        }
        if (workNode.isArray()) {
            for (int i = 0; i < workNode.size(); i++) {
                final JsonNode elem = workNode.get(i);
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
                ? StringUtils.containsIgnoreCase(workNode.asText(), value)
                : StringUtils.contains(workNode.asText(), value)));
    }

    static JsonNode funcEndsWith(final JsonNode node, final String params,
                                 final boolean ignoreCase, final boolean not) {
        return applyTextNodeWithParamAsText(node, params, not,
                (str, param) -> ignoreCase
                        ? StringUtils.endsWithIgnoreCase(str, param)
                        : StringUtils.endsWith(str, param)
        );
    }

    static JsonNode funcEquals(final JsonNode node, final String params, final boolean ignoreCase, final boolean not) {
        return applyTextNodeWithParamAsText(node, params, not,
                (str, param) -> ignoreCase
                        ? StringUtils.equalsIgnoreCase(str, param)
                        : StringUtils.equals(str, param)
        );
    }

    static JsonNode funcIn(final JsonNode node, final String params, final boolean ignoreCase, final boolean not) {
        return applyWithArrayNode(node, params, null,
                (jsonNode, arrayNode) -> {
                    if (jsonNode.isNumber()) {
                        final double num = jsonNode.asDouble();
                        for (int i = arrayNode.size() - 1; i >= 0; i--) {
                            final JsonNode value = arrayNode.get(i);
                            if ((value.isNumber() || value.isTextual()) && value.asDouble() == num) {
                                return BooleanNode.valueOf(!not);
                            }
                        }
                        return BooleanNode.valueOf(not);
                    } else if (jsonNode.isValueNode()) {
                        final String text = jsonNode.asText();
                        for (int i = arrayNode.size() - 1; i >= 0; i--) {
                            final JsonNode value = arrayNode.get(i);
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
                });
    }

    static JsonNode funcIsArray(final JsonNode node, final String params) {
        return applyWithoutParam(node, params, jsonNode -> BooleanNode.valueOf(jsonNode.isArray()));
    }

    static JsonNode funcIsBlank(final JsonNode node, final String params, final boolean not) {
        return applyWithoutParam(node, params, null,
                (data, paramList) -> {
                    final JsonNode dataNode = data.getKey();
                    return BooleanNode.valueOf(dataNode.isTextual() && (not ^ StringUtils.isBlank(dataNode.asText())));
                });
    }

    static JsonNode funcIsBoolean(final JsonNode node, final String params) {
        return applyWithoutParam(node, params, null,
                (data, paramList) -> BooleanNode.valueOf(data.getKey().isBoolean()));
    }

    static JsonNode funcIsEmpty(final JsonNode node, final String params, final boolean not) {
        return applyWithoutParam(node, params, null,
                (data, paramList) -> {
                    final JsonNode dataNode = data.getKey();
                    return BooleanNode.valueOf(
                            dataNode.isTextual() ? not ^ dataNode.asText().isEmpty() : dataNode.isNull() != not);
                });
    }

    static JsonNode funcIsEmptyArray(final JsonNode node, final String params) {
        return applyWithoutParam(node, params,
                jsonNode -> BooleanNode.valueOf(jsonNode.isArray() && jsonNode.isEmpty()));
    }

    static JsonNode funcIsEmptyObject(final JsonNode node, final String params) {
        return applyWithoutParam(node, params,
                jsonNode -> BooleanNode.valueOf(jsonNode.isObject() && jsonNode.isEmpty()));
    }

    static JsonNode funcIsEven(final JsonNode node, final String params) {
        return applyWithoutParam(node, params, null,
                (data, paramList) -> {
                    final JsonNode dataNode = data.getKey();
                    final int number;
                    if (dataNode.isNumber()) {
                        number = dataNode.asInt();
                    } else if (dataNode.isTextual()) {
                        try {
                            number = Integer.parseInt(dataNode.asText());
                        } catch (NumberFormatException e) {
                            return BooleanNode.FALSE;
                        }
                    } else {
                        return BooleanNode.FALSE;
                    }
                    return BooleanNode.valueOf((number & 1) == 0);
                });
    }

    static JsonNode funcIsNull(final JsonNode node, final String params, final boolean not) {
        return applyWithoutParam(node, params, null,
                (data, paramList) -> BooleanNode.valueOf(not ^ data.getKey().isNull()));
    }

    static JsonNode funcIsNumber(final JsonNode node, final String params) {
        return applyWithoutParam(node, params, null,
                (data, paramList) -> BooleanNode.valueOf(data.getKey().isNumber()));
    }

    static JsonNode funcIsObject(final JsonNode node, final String params) {
        return applyWithoutParam(node, params, jsonNode -> BooleanNode.valueOf(jsonNode.isObject()));
    }

    static JsonNode funcIsOdd(final JsonNode node, final String params) {
        return applyWithoutParam(node, params, null,
                (data, paramList) -> {
                    final JsonNode dataNode = data.getKey();
                    final int number;
                    if (dataNode.isNumber()) {
                        number = dataNode.asInt();
                    } else if (dataNode.isTextual()) {
                        try {
                            number = Integer.parseInt(dataNode.asText());
                        } catch (NumberFormatException e) {
                            return BooleanNode.FALSE;
                        }
                    } else {
                        return BooleanNode.FALSE;
                    }
                    return BooleanNode.valueOf((number & 1) != 0);
                });
    }

    static JsonNode funcIsText(final JsonNode node, final String params) {
        return applyWithoutParam(node, params, null,
                (data, paramList) -> BooleanNode.valueOf(data.getKey().isTextual()));
    }

    static JsonNode funcMatches(final JsonNode node, final String params, final boolean not) {
        return applyTextNodeWithParamAsText(node, params, not,
                (str, param) -> Pattern.compile(param).matcher(str).matches());
    }

    static JsonNode funcNot(final JsonNode node, final String params) {
        return applyWithoutParam(node, params, null,
                (data, paramList) -> {
                    final JsonNode dataNode = data.getKey();
                    return dataNode.isBoolean() ? BooleanNode.valueOf(!dataNode.asBoolean())
                            : BooleanNode.FALSE;
                });
    }

    static JsonNode funcStartsWith(final JsonNode node, final String params,
                                   final boolean ignoreCase, final boolean not) {
        return applyTextNodeWithParamAsText(node, params, not,
                (str, param) -> ignoreCase
                        ? StringUtils.startsWithIgnoreCase(str, param)
                        : StringUtils.startsWith(str, param)
        );
    }

    static JsonNode funcIsWeekday(final JsonNode node, final String params) {
        return applyWithoutParam(node, params, null,
                (data, paramList) -> {
                    final JsonNode dataNode = data.getKey();
                    return dataNode.isTextual()
                            ? BooleanNode.valueOf(toLocalDateTime(dataNode).get(ChronoField.DAY_OF_WEEK) <= 5)
                            : null;
                });
    }

    static JsonNode funcIsWeekend(final JsonNode node, final String params) {
        return applyWithoutParam(node, params, null,
                (data, paramList) -> {
                    final JsonNode dataNode = data.getKey();
                    return dataNode.isTextual()
                            ? BooleanNode.valueOf(toLocalDateTime(dataNode).get(ChronoField.DAY_OF_WEEK) > 5)
                            : null;
                });
    }

    static JsonNode funcIsLeapYear(final JsonNode node, final String params) {
        return applyWithoutParam(node, params, null,
                (data, paramList) -> {
                    final JsonNode dataNode = data.getKey();
                    return dataNode.isTextual()
                            ? BooleanNode.valueOf(IsoChronology.INSTANCE.isLeapYear(toLocalDateTime(dataNode).getYear()))
                            : null;
                });
    }
}

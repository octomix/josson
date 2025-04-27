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
import com.fasterxml.jackson.databind.node.BooleanNode;
import com.octomix.josson.commons.StringUtils;

import java.time.chrono.IsoChronology;
import java.time.temporal.ChronoField;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Pattern;

import static com.octomix.josson.FunctionExecutor.*;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Utils.toLocalDateTime;

/**
 * Logical functions.
 */
final class FuncLogical {

    private FuncLogical() {
    }

    static PathTrace funcContains(final PathTrace path, final String params, final boolean ignoreCase, final boolean not) {
        final Pair<PathTrace, List<String>> pathAndParams = getParamPathAndStrings(path, params, 1, 1);
        final PathTrace dataPath = pathAndParams.getKey();
        if (dataPath == null) {
            return path.push(BooleanNode.FALSE);
        }
        final JsonNode result = getNodeByExpression(path, pathAndParams.getValue().get(0));
        if (result == null || result.isContainerNode()) {
            return path.push(BooleanNode.FALSE);
        }
        if (result.isNumber()) {
            final double value = result.asDouble();
            if (dataPath.isArray()) {
                for (JsonNode elem : dataPath.node()) {
                    if ((elem.isNumber() || elem.isTextual()) && elem.asDouble() == value) {
                        return path.push(BooleanNode.valueOf(!not));
                    }
                }
            }
            return path.push(BooleanNode.valueOf(not));
        }
        if (result.isNull()) {
            if (dataPath.isArray()) {
                for (JsonNode elem : dataPath.node()) {
                    if (elem.isNull()) {
                        return path.push(BooleanNode.valueOf(!not));
                    }
                }
            }
            return path.push(BooleanNode.valueOf(not));
        }
        final String value = result.asText();
        if (dataPath.isObject()) {
            if (ignoreCase) {
                for (Iterator<String> it = dataPath.node().fieldNames(); it.hasNext();) {
                    if (value.equalsIgnoreCase(it.next())) {
                        return path.push(BooleanNode.valueOf(!not));
                    }
                }
                return path.push(BooleanNode.valueOf(not));
            }
            return path.push(BooleanNode.valueOf(not ^ dataPath.get(value) != null));
        }
        if (dataPath.isArray()) {
            for (JsonNode elem : dataPath.node()) {
                if (elem.isTextual() || elem.isNumber()) {
                    if (ignoreCase) {
                        if (value.equalsIgnoreCase(elem.asText())) {
                            return path.push(BooleanNode.valueOf(!not));
                        }
                    } else if (value.equals(elem.asText())) {
                        return path.push(BooleanNode.valueOf(!not));
                    }
                }
            }
            return path.push(BooleanNode.valueOf(not));
        }
        return path.push(BooleanNode.valueOf(not ^ (ignoreCase
            ? StringUtils.containsIgnoreCase(dataPath.asText(), value)
            : StringUtils.contains(dataPath.asText(), value))));
    }

    static PathTrace funcEndsWith(final PathTrace path, final String params,
                                  final boolean ignoreCase, final boolean not) {
        return applyTextNodeWithParamAsText(path, params, not,
            (str, param) -> ignoreCase ? StringUtils.endsWithIgnoreCase(str, param) : StringUtils.endsWith(str, param)
        );
    }

    static PathTrace funcEquals(final PathTrace path, final String params, final boolean ignoreCase, final boolean not) {
        return applyTextNodeWithParamAsText(path, params, not,
            (str, param) -> ignoreCase ? StringUtils.equalsIgnoreCase(str, param) : StringUtils.equals(str, param)
        );
    }

    static PathTrace funcIn(final PathTrace path, final String params, final boolean ignoreCase, final boolean not) {
        return applyWithArrayNode(path, params, null,
            (dataPath, paramPath) -> {
                if (dataPath.isNumber()) {
                    final double num = dataPath.asDouble();
                    for (JsonNode value : paramPath.node()) {
                        if ((value.isNumber() || value.isTextual()) && value.asDouble() == num) {
                            return path.push(BooleanNode.valueOf(!not));
                        }
                    }
                    return path.push(BooleanNode.valueOf(not));
                } else if (dataPath.isValueNode()) {
                    final String text = dataPath.asText();
                    for (JsonNode value : paramPath.node()) {
                        if (value.isTextual() || value.isNumber()) {
                            if (ignoreCase) {
                                if (value.asText().equalsIgnoreCase(text)) {
                                    return path.push(BooleanNode.valueOf(!not));
                                }
                            } else if (value.asText().equals(text)) {
                                return path.push(BooleanNode.valueOf(!not));
                            }
                        }
                    }
                    return path.push(BooleanNode.valueOf(not));
                }
                return path.push(BooleanNode.FALSE);
            });
    }

    static PathTrace funcIsArray(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, dataPath -> path.push(BooleanNode.valueOf(dataPath.isArray())));
    }

    static PathTrace funcIsBlank(final PathTrace path, final String params, final boolean not) {
        return applyWithoutParam(path, params, null,
            (data, paramList) -> path.push(BooleanNode.valueOf(
                data.getKey().isTextual() && (not ^ StringUtils.isBlank(data.getKey().asText())))));
    }

    static PathTrace funcIsBoolean(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, null,
            (data, paramList) -> path.push(BooleanNode.valueOf(data.getKey().isBoolean())));
    }

    static PathTrace funcIsEmpty(final PathTrace path, final String params, final boolean not) {
        return applyWithoutParam(path, params, null,
            (data, paramList) -> path.push(BooleanNode.valueOf(data.getKey().isTextual()
                ? not ^ data.getKey().asText().isEmpty()
                : data.getKey().isNull() != not)));
    }

    static PathTrace funcIsEmptyArray(final PathTrace path, final String params) {
        return applyWithoutParam(path, params,
            dataPath -> path.push(BooleanNode.valueOf(dataPath.isArray() && dataPath.isEmpty())));
    }

    static PathTrace funcIsEmptyObject(final PathTrace path, final String params) {
        return applyWithoutParam(path, params,
            dataPath -> path.push(BooleanNode.valueOf(dataPath.isObject() && dataPath.isEmpty())));
    }

    static PathTrace funcIsEvenOdd(final PathTrace path, final String params, final int parity) {
        return applyWithoutParam(path, params, null,
            (data, paramList) -> {
                final PathTrace dataPath = data.getKey();
                final int number;
                if (dataPath.isNumber()) {
                    number = dataPath.asInt();
                } else if (dataPath.isTextual()) {
                    try {
                        number = Integer.parseInt(dataPath.asText());
                    } catch (NumberFormatException e) {
                        return path.push(BooleanNode.FALSE);
                    }
                } else {
                    return path.push(BooleanNode.FALSE);
                }
                return path.push(BooleanNode.valueOf((number & 1) == parity));
            });
    }

    static PathTrace funcIsNull(final PathTrace path, final String params, final boolean not) {
        return applyWithoutParam(path, params, null,
            (data, paramList) -> path.push(BooleanNode.valueOf(not ^ data.getKey().isNull())));
    }

    static PathTrace funcIsNumber(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, null,
            (data, paramList) -> path.push(BooleanNode.valueOf(data.getKey().isNumber())));
    }

    static PathTrace funcIsObject(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, dataPath -> path.push(BooleanNode.valueOf(dataPath.isObject())));
    }

    static PathTrace funcIsText(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, null,
            (data, paramList) -> path.push(BooleanNode.valueOf(data.getKey().isTextual())));
    }

    static PathTrace funcMatches(final PathTrace path, final String params, final boolean not) {
        return applyTextNodeWithParamAsText(path, params, not,
            (str, param) -> Pattern.compile(param).matcher(str).matches());
    }

    static PathTrace funcNot(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, null,
            (data, paramList) -> path.push(data.getKey().isBoolean()
                ? BooleanNode.valueOf(!data.getKey().asBoolean())
                : BooleanNode.FALSE));
    }

    static PathTrace funcStartsWith(final PathTrace path, final String params, final boolean ignoreCase, final boolean not) {
        return applyTextNodeWithParamAsText(path, params, not,
            (str, param) -> ignoreCase ? StringUtils.startsWithIgnoreCase(str, param) : StringUtils.startsWith(str, param)
        );
    }

    static PathTrace funcIsWeekday(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, null,
            (data, paramList) -> data.getKey().isTextual()
                ? path.push(BooleanNode.valueOf(toLocalDateTime(data.getKey().node()).get(ChronoField.DAY_OF_WEEK) <= 5))
                : null);
    }

    static PathTrace funcIsWeekend(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, null,
            (data, paramList) -> data.getKey().isTextual()
                ? path.push(BooleanNode.valueOf(toLocalDateTime(data.getKey().node()).get(ChronoField.DAY_OF_WEEK) > 5))
                : null);
    }

    static PathTrace funcIsLeapYear(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, null,
            (data, paramList) -> data.getKey().isTextual()
                ? path.push(BooleanNode.valueOf(IsoChronology.INSTANCE.isLeapYear(toLocalDateTime(data.getKey().node()).getYear())))
                : null);
    }
}

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
        final PathTrace result = getPathByExpression(path, pathAndParams.getValue().get(0));
        if (result == null || result.node().isContainerNode()) {
            return path.push(BooleanNode.FALSE);
        }
        if (result.node().isNumber()) {
            final double value = result.node().asDouble();
            if (dataPath.node().isArray()) {
                for (JsonNode elem : dataPath.node()) {
                    if ((elem.isNumber() || elem.isTextual()) && elem.asDouble() == value) {
                        return path.push(BooleanNode.valueOf(!not));
                    }
                }
            }
            return path.push(BooleanNode.valueOf(not));
        }
        if (result.node().isNull()) {
            if (dataPath.node().isArray()) {
                for (JsonNode elem : dataPath.node()) {
                    if (elem.isNull()) {
                        return path.push(BooleanNode.valueOf(!not));
                    }
                }
            }
            return path.push(BooleanNode.valueOf(not));
        }
        final String value = result.node().asText();
        if (dataPath.node().isObject()) {
            if (ignoreCase) {
                for (Iterator<String> it = dataPath.node().fieldNames(); it.hasNext();) {
                    if (value.equalsIgnoreCase(it.next())) {
                        return path.push(BooleanNode.valueOf(!not));
                    }
                }
                return path.push(BooleanNode.valueOf(not));
            }
            return path.push(BooleanNode.valueOf(not ^ dataPath.node().get(value) != null));
        }
        if (dataPath.node().isArray()) {
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
            ? StringUtils.containsIgnoreCase(dataPath.node().asText(), value)
            : StringUtils.contains(dataPath.node().asText(), value))));
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
                if (dataPath.node().isNumber()) {
                    final double num = dataPath.node().asDouble();
                    for (JsonNode value : paramPath.node()) {
                        if ((value.isNumber() || value.isTextual()) && value.asDouble() == num) {
                            return path.push(BooleanNode.valueOf(!not));
                        }
                    }
                    return path.push(BooleanNode.valueOf(not));
                } else if (dataPath.node().isValueNode()) {
                    final String text = dataPath.node().asText();
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
        return applyWithoutParam(path, params, dataPath -> path.push(BooleanNode.valueOf(dataPath.node().isArray())));
    }

    static PathTrace funcIsBlank(final PathTrace path, final String params, final boolean not) {
        return applyWithoutParam(path, params, null,
            (data, paramList) -> path.push(BooleanNode.valueOf(
                data.getKey().node().isTextual() && (not ^ StringUtils.isBlank(data.getKey().node().asText())))));
    }

    static PathTrace funcIsBoolean(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, null,
            (data, paramList) -> path.push(BooleanNode.valueOf(data.getKey().node().isBoolean())));
    }

    static PathTrace funcIsEmpty(final PathTrace path, final String params, final boolean not) {
        return applyWithoutParam(path, params, null,
            (data, paramList) -> path.push(BooleanNode.valueOf(data.getKey().node().isTextual()
                ? not ^ data.getKey().node().asText().isEmpty()
                : data.getKey().node().isNull() != not)));
    }

    static PathTrace funcIsEmptyArray(final PathTrace path, final String params) {
        return applyWithoutParam(path, params,
            dataPath -> path.push(BooleanNode.valueOf(dataPath.node().isArray() && dataPath.node().isEmpty())));
    }

    static PathTrace funcIsEmptyObject(final PathTrace path, final String params) {
        return applyWithoutParam(path, params,
            dataPath -> path.push(BooleanNode.valueOf(dataPath.node().isObject() && dataPath.node().isEmpty())));
    }

    static PathTrace funcIsEvenOdd(final PathTrace path, final String params, final int parity) {
        return applyWithoutParam(path, params, null,
            (data, paramList) -> {
                final PathTrace dataPath = data.getKey();
                final int number;
                if (dataPath.node().isNumber()) {
                    number = dataPath.node().asInt();
                } else if (dataPath.node().isTextual()) {
                    try {
                        number = Integer.parseInt(dataPath.node().asText());
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
            (data, paramList) -> path.push(BooleanNode.valueOf(not ^ data.getKey().node().isNull())));
    }

    static PathTrace funcIsNumber(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, null,
            (data, paramList) -> path.push(BooleanNode.valueOf(data.getKey().node().isNumber())));
    }

    static PathTrace funcIsObject(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, dataPath -> path.push(BooleanNode.valueOf(dataPath.node().isObject())));
    }

    static PathTrace funcIsText(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, null,
            (data, paramList) -> path.push(BooleanNode.valueOf(data.getKey().node().isTextual())));
    }

    static PathTrace funcMatches(final PathTrace path, final String params, final boolean not) {
        return applyTextNodeWithParamAsText(path, params, not,
            (str, param) -> Pattern.compile(param).matcher(str).matches());
    }

    static PathTrace funcNot(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, null,
            (data, paramList) -> path.push(data.getKey().node().isBoolean()
                ? BooleanNode.valueOf(!data.getKey().node().asBoolean())
                : BooleanNode.FALSE));
    }

    static PathTrace funcStartsWith(final PathTrace path, final String params, final boolean ignoreCase, final boolean not) {
        return applyTextNodeWithParamAsText(path, params, not,
            (str, param) -> ignoreCase ? StringUtils.startsWithIgnoreCase(str, param) : StringUtils.startsWith(str, param)
        );
    }

    static PathTrace funcIsWeekday(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, null,
            (data, paramList) -> data.getKey().node().isTextual()
                ? path.push(BooleanNode.valueOf(toLocalDateTime(data.getKey().node()).get(ChronoField.DAY_OF_WEEK) <= 5))
                : null);
    }

    static PathTrace funcIsWeekend(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, null,
            (data, paramList) -> data.getKey().node().isTextual()
                ? path.push(BooleanNode.valueOf(toLocalDateTime(data.getKey().node()).get(ChronoField.DAY_OF_WEEK) > 5))
                : null);
    }

    static PathTrace funcIsLeapYear(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, null,
            (data, paramList) -> data.getKey().node().isTextual()
                ? path.push(BooleanNode.valueOf(IsoChronology.INSTANCE.isLeapYear(toLocalDateTime(data.getKey().node()).getYear())))
                : null);
    }
}

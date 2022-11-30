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
import com.fasterxml.jackson.databind.node.LongNode;
import com.fasterxml.jackson.databind.node.TextNode;

import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.temporal.ChronoField;
import java.time.temporal.ChronoUnit;

import static com.octomix.josson.FuncExecutor.*;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Utils.*;

/**
 * Date functions.
 */
final class FuncDate {

    private FuncDate() {
    }

    static PathTrace funcAmPmOfDay(final PathTrace path, final String params) {
        return applyTextNode(path, params,
            dataPath -> toLocalDateTime(dataPath.node()).get(ChronoField.AMPM_OF_DAY) == 0 ? "AM" : "PM");
    }

    static PathTrace funcChronometry(final PathTrace path, final String params, final ChronoField field) {
        return applyTextNodeToInt(path, params, dataPath -> toLocalDateTime(dataPath.node()).get(field));
    }

    static PathTrace funcDatePlus(final PathTrace path, final String params, final ChronoUnit unit) {
        return applyWithParams(path, params, 1, 1, JsonNode::isTextual,
            (data, paramList) -> {
                final PathTrace dataPath = data.getKey();
                final PathTrace paramPath = data.getValue() < 0 ? dataPath : path;
                final int amount = getNodeAsInt(paramPath, data.getValue(), paramList.get(0));
                return path.push(TextNode.valueOf(toLocalDateTime(dataPath.node()).plus(amount, unit).toString()));
            });
    }

    static PathTrace funcDateMinus(final PathTrace path, final String params, final ChronoUnit unit) {
        return applyWithParams(path, params, 1, 1, JsonNode::isTextual,
            (data, paramList) -> {
                final PathTrace dataPath = data.getKey();
                final PathTrace paramPath = data.getValue() < 0 ? dataPath : path;
                final int amount = getNodeAsInt(paramPath, data.getValue(), paramList.get(0));
                return path.push(TextNode.valueOf(toLocalDateTime(dataPath.node()).minus(amount, unit).toString()));
            });
    }

    static PathTrace funcDateTruncateTo(final PathTrace path, final String params, final ChronoUnit unit) {
        return applyTextNode(path, params, dataPath -> toLocalDateTime(dataPath.node()).truncatedTo(unit).toString());
    }

    static PathTrace funcDateTruncateToMonth(final PathTrace path, final String params) {
        return applyTextNode(path, params, dataPath -> toLocalDate(dataPath.node()).withDayOfMonth(1).toString());
    }

    static PathTrace funcDateTruncateToYear(final PathTrace path, final String params) {
        return applyTextNode(path, params, dataPath -> toLocalDate(dataPath.node()).withDayOfYear(1).toString());
    }

    static PathTrace funcDateWith(final PathTrace path, final String params, final ChronoField field) {
        return applyWithParams(path, params, 1, 1, JsonNode::isTextual,
            (data, paramList) -> {
                final PathTrace dataPath = data.getKey();
                final PathTrace paramPath = data.getValue() < 0 ? dataPath : path;
                final int value = getNodeAsInt(paramPath, data.getValue(), paramList.get(0));
                return path.push(TextNode.valueOf(toLocalDateTime(dataPath.node()).with(field, value).toString()));
            });
    }

    static PathTrace funcDayEnd(final PathTrace path, final String params) {
        return applyTextNode(path, params, dataPath -> toLocalDate(dataPath.node()).plusDays(1).minusNanos(1).toString());
    }

    static PathTrace funcMonthEnd(final PathTrace path, final String params) {
        return applyTextNode(path, params,
            dataPath -> toLocalDate(dataPath.node()).withDayOfMonth(1).plusMonths(1).minusNanos(1).toString());
    }

    static PathTrace funcYearEnd(final PathTrace path, final String params) {
        return applyTextNode(path, params,
            dataPath -> toLocalDate(dataPath.node()).withDayOfYear(1).plusYears(1).minusNanos(1).toString());
    }

    static PathTrace funcLengthOfMonth(final PathTrace path, final String params) {
        return applyTextNodeToInt(path, params, dataPath -> toLocalDateTime(dataPath.node()).toLocalDate().lengthOfMonth());
    }

    static PathTrace funcLengthOfYear(final PathTrace path, final String params) {
        return applyTextNodeToInt(path, params, dataPath -> toLocalDateTime(dataPath.node()).toLocalDate().lengthOfYear());
    }

    static PathTrace funcUntil(final PathTrace path, final String params, final ChronoUnit unit) {
        return applyWithParams(path, params, 1, 1, JsonNode::isTextual,
            (data, paramList) -> {
                final PathTrace dataPath = data.getKey();
                final PathTrace paramPath = data.getValue() < 0 ? dataPath : path;
                final JsonNode later = getNodeByExpression(paramPath, data.getValue(), paramList.get(0));
                if (nodeIsNull(later)) {
                    return null;
                }
                return path.push(LongNode.valueOf(toLocalDateTime(dataPath.node()).until(toLocalDateTime(later), unit)));
            });
    }

    static PathTrace funcLocalToOffsetDate(final PathTrace path, final String params) {
        return applyTextNode(path, params, dataPath -> localToOffsetDateTime(dataPath.node()).toString());
    }

    static PathTrace funcLocalDateToEpochMilli(final PathTrace path, final String params) {
        return applyTextNodeToLong(path, params, dataPath -> Utils.localDateTimeToEpochMilli(dataPath.node()));
    }

    static PathTrace funcLocalDateToEpochSecond(final PathTrace path, final String params) {
        return applyTextNodeToLong(path, params, dataPath -> Utils.localDateTimeToEpochSecond(dataPath.node()));
    }

    static PathTrace funcOffsetToLocalDate(final PathTrace path, final String params) {
        return applyTextNode(path, params, dataPath -> offsetToLocalDateTime(dataPath.node()).toString());
    }

    static PathTrace funcOffsetDateToEpochMilli(final PathTrace path, final String params) {
        return applyTextNodeToLong(path, params, dataPath -> Utils.offsetDateTimeToEpochMilli(dataPath.node()));
    }

    static PathTrace funcOffsetDateToEpochSecond(final PathTrace path, final String params) {
        return applyTextNodeToLong(path, params, dataPath -> Utils.offsetDateTimeToEpochSecond(dataPath.node()));
    }

    static PathTrace funcEpochMilliToLocalDate(final PathTrace path, final String params) {
        return applyNumberNodeToText(path, params, dataPath -> epochMilliToLocalDateTime(dataPath.node()).toString());
    }

    static PathTrace funcEpochMilliToOffsetDate(final PathTrace path, final String params) {
        return applyNumberNodeToText(path, params, dataPath -> epochMilliToOffsetDateTime(dataPath.node()).toString());
    }

    static PathTrace funcEpochSecondToLocalDate(final PathTrace path, final String params) {
        return applyNumberNodeToText(path, params, dataPath -> epochSecondToLocalDateTime(dataPath.node()).toString());
    }

    static PathTrace funcEpochSecondToOffsetDate(final PathTrace path, final String params) {
        return applyNumberNodeToText(path, params, dataPath -> epochSecondToOffsetDateTime(dataPath.node()).toString());
    }

    static PathTrace funcNow(final PathTrace path, final String params) {
        return applyWithParams(path, params, 0, 1, null,
            (data, paramList) -> {
                final PathTrace dataPath = data.getKey();
                final PathTrace paramPath = data.getValue() < 0 ? dataPath : path;
                final String type = paramList.size() > 0 ? getNodeAsText(paramPath, data.getValue(), paramList.get(0)) : null;
                if (type == null || type.equalsIgnoreCase("local")) {
                    return path.push(TextNode.valueOf(LocalDateTime.now().toString()));
                }
                switch (type.toLowerCase()) {
                    case "offset":
                        return path.push(TextNode.valueOf(OffsetDateTime.now().toString()));
                    case "millis":
                        return path.push(LongNode.valueOf(OffsetDateTime.now().toInstant().toEpochMilli()));
                    case "seconds":
                        return path.push(LongNode.valueOf(OffsetDateTime.now().toEpochSecond()));
                    default:
                        return null;
                }
            });
    }
}

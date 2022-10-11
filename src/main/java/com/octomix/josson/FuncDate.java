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

    static JsonNode funcAmPmOfDay(final JsonNode node, final String params) {
        return applyTextNode(node, params,
            jsonNode -> toLocalDateTime(jsonNode).get(ChronoField.AMPM_OF_DAY) == 0 ? "AM" : "PM");
    }

    static JsonNode funcChronometry(final JsonNode node, final String params, final ChronoField field) {
        return applyTextNodeToInt(node, params, jsonNode -> toLocalDateTime(jsonNode).get(field));
    }

    static JsonNode funcDatePlus(final JsonNode node, final String params, final ChronoUnit unit) {
        return applyWithParams(node, params, 1, 1, JsonNode::isTextual,
            (data, paramList) -> {
                final JsonNode dataNode = data.getKey();
                final JsonNode paramNode = data.getValue() < 0 ? dataNode : node;
                final int amount = getNodeAsInt(paramNode, data.getValue(), paramList.get(0));
                return TextNode.valueOf(toLocalDateTime(dataNode).plus(amount, unit).toString());
            });
    }

    static JsonNode funcDateMinus(final JsonNode node, final String params, final ChronoUnit unit) {
        return applyWithParams(node, params, 1, 1, JsonNode::isTextual,
            (data, paramList) -> {
                final JsonNode dataNode = data.getKey();
                final JsonNode paramNode = data.getValue() < 0 ? dataNode : node;
                final int amount = getNodeAsInt(paramNode, data.getValue(), paramList.get(0));
                return TextNode.valueOf(toLocalDateTime(dataNode).minus(amount, unit).toString());
            });
    }

    static JsonNode funcDateTruncateTo(final JsonNode node, final String params, final ChronoUnit unit) {
        return applyTextNode(node, params, jsonNode -> toLocalDateTime(jsonNode).truncatedTo(unit).toString());
    }

    static JsonNode funcDateTruncateToMonth(final JsonNode node, final String params) {
        return applyTextNode(node, params, jsonNode -> toLocalDate(jsonNode).withDayOfMonth(1).toString());
    }

    static JsonNode funcDateTruncateToYear(final JsonNode node, final String params) {
        return applyTextNode(node, params, jsonNode -> toLocalDate(jsonNode).withDayOfYear(1).toString());
    }

    static JsonNode funcDateWith(final JsonNode node, final String params, final ChronoField field) {
        return applyWithParams(node, params, 1, 1, JsonNode::isTextual,
            (data, paramList) -> {
                final JsonNode dataNode = data.getKey();
                final JsonNode paramNode = data.getValue() < 0 ? dataNode : node;
                final int value = getNodeAsInt(paramNode, data.getValue(), paramList.get(0));
                return TextNode.valueOf(toLocalDateTime(dataNode).with(field, value).toString());
            });
    }

    static JsonNode funcDayEnd(final JsonNode node, final String params) {
        return applyTextNode(node, params, jsonNode -> toLocalDate(jsonNode).plusDays(1).minusNanos(1).toString());
    }

    static JsonNode funcMonthEnd(final JsonNode node, final String params) {
        return applyTextNode(node, params,
            jsonNode -> toLocalDate(jsonNode).withDayOfMonth(1).plusMonths(1).minusNanos(1).toString());
    }

    static JsonNode funcYearEnd(final JsonNode node, final String params) {
        return applyTextNode(node, params,
            jsonNode -> toLocalDate(jsonNode).withDayOfYear(1).plusYears(1).minusNanos(1).toString());
    }

    static JsonNode funcLengthOfMonth(final JsonNode node, final String params) {
        return applyTextNodeToInt(node, params, jsonNode -> toLocalDateTime(jsonNode).toLocalDate().lengthOfMonth());
    }

    static JsonNode funcLengthOfYear(final JsonNode node, final String params) {
        return applyTextNodeToInt(node, params, jsonNode -> toLocalDateTime(jsonNode).toLocalDate().lengthOfYear());
    }

    static JsonNode funcUntil(final JsonNode node, final String params, final ChronoUnit unit) {
        return applyWithParams(node, params, 1, 1, JsonNode::isTextual,
            (data, paramList) -> {
                final JsonNode dataNode = data.getKey();
                final JsonNode paramNode = data.getValue() < 0 ? dataNode : node;
                final JsonNode later = getNodeByPath(paramNode, data.getValue(), paramList.get(0));
                return LongNode.valueOf(toLocalDateTime(dataNode).until(toLocalDateTime(later), unit));
            });
    }

    static JsonNode funcLocalToOffsetDate(final JsonNode node, final String params) {
        return applyTextNode(node, params, jsonNode -> localToOffsetDateTime(jsonNode).toString());
    }

    static JsonNode funcLocalDateToEpochMilli(final JsonNode node, final String params) {
        return applyTextNodeToLong(node, params, Utils::localDateTimeToEpochMilli);
    }

    static JsonNode funcLocalDateToEpochSecond(final JsonNode node, final String params) {
        return applyTextNodeToLong(node, params, Utils::localDateTimeToEpochSecond);
    }

    static JsonNode funcOffsetToLocalDate(final JsonNode node, final String params) {
        return applyTextNode(node, params, jsonNode -> offsetToLocalDateTime(jsonNode).toString());
    }

    static JsonNode funcOffsetDateToEpochMilli(final JsonNode node, final String params) {
        return applyTextNodeToLong(node, params, Utils::offsetDateTimeToEpochMilli);
    }

    static JsonNode funcOffsetDateToEpochSecond(final JsonNode node, final String params) {
        return applyTextNodeToLong(node, params, Utils::offsetDateTimeToEpochSecond);
    }

    static JsonNode funcEpochMilliToLocalDate(final JsonNode node, final String params) {
        return applyNumberNodeToText(node, params, jsonNode -> epochMilliToLocalDateTime(jsonNode).toString());
    }

    static JsonNode funcEpochMilliToOffsetDate(final JsonNode node, final String params) {
        return applyNumberNodeToText(node, params, jsonNode -> epochMilliToOffsetDateTime(jsonNode).toString());
    }

    static JsonNode funcEpochSecondToLocalDate(final JsonNode node, final String params) {
        return applyNumberNodeToText(node, params, jsonNode -> epochSecondToLocalDateTime(jsonNode).toString());
    }

    static JsonNode funcEpochSecondToOffsetDate(final JsonNode node, final String params) {
        return applyNumberNodeToText(node, params, jsonNode -> epochSecondToOffsetDateTime(jsonNode).toString());
    }

    static JsonNode funcNow(final JsonNode node, final String params) {
        return applyWithParams(node, params, 0, 1, null,
            (data, paramList) -> {
                final JsonNode dataNode = data.getKey();
                final JsonNode paramNode = data.getValue() < 0 ? dataNode : node;
                final String type = paramList.size() > 0 ? getNodeAsText(paramNode, data.getValue(), paramList.get(0)) : null;
                if (type == null || type.equalsIgnoreCase("local")) {
                    return TextNode.valueOf(LocalDateTime.now().toString());
                }
                switch (type) {
                    case "offset":
                        return TextNode.valueOf(OffsetDateTime.now().toString());
                    case "millis":
                        return LongNode.valueOf(OffsetDateTime.now().toInstant().toEpochMilli());
                    case "seconds":
                        return LongNode.valueOf(OffsetDateTime.now().toEpochSecond());
                    default:
                        return null;
                }
            });
    }
}

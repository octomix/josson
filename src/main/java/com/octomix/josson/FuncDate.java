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
import com.fasterxml.jackson.databind.node.IntNode;
import com.fasterxml.jackson.databind.node.TextNode;

import java.time.temporal.ChronoField;
import java.time.temporal.ChronoUnit;

import static com.octomix.josson.FuncExecutor.*;
import static com.octomix.josson.JossonCore.*;

class FuncDate {
    static JsonNode funcAmPmOfDay(JsonNode node, String params) {
        return applyWithoutArgument(node, params,
                JsonNode::isTextual,
                jsonNode -> TextNode.valueOf(toLocalDateTime(jsonNode).get(ChronoField.AMPM_OF_DAY) == 0 ? "AM" : "PM")
        );
    }

    static JsonNode funcChronometry(JsonNode node, String params, ChronoField field) {
        return applyWithoutArgument(node, params,
                JsonNode::isTextual,
                jsonNode -> IntNode.valueOf(toLocalDateTime(jsonNode).get(field))
        );
    }

    static JsonNode funcDatePlus(JsonNode node, String params, ChronoUnit unit) {
        return applyWithArguments(node, params, 1, 1,
                JsonNode::isTextual,
                (jsonNode, paramList) -> getNodeAsInt(jsonNode, paramList.get(0)),
                (jsonNode, objVar) -> TextNode.valueOf(toLocalDateTime(jsonNode).plus((int) objVar, unit).toString())
        );
    }

    static JsonNode funcDateMinus(JsonNode node, String params, ChronoUnit unit) {
        return applyWithArguments(node, params, 1, 1,
                JsonNode::isTextual,
                (jsonNode, paramList) -> getNodeAsInt(jsonNode, paramList.get(0)),
                (jsonNode, objVar) -> TextNode.valueOf(toLocalDateTime(jsonNode).minus((int) objVar, unit).toString())
        );
    }

    static JsonNode funcDateTruncateTo(JsonNode node, String params, ChronoUnit unit) {
        return applyWithoutArgument(node, params,
                JsonNode::isTextual,
                jsonNode -> TextNode.valueOf(toLocalDateTime(jsonNode).truncatedTo(unit).toString())
        );
    }

    static JsonNode funcDateTruncateToMonth(JsonNode node, String params) {
        return applyWithoutArgument(node, params,
                JsonNode::isTextual,
                jsonNode -> TextNode.valueOf(toLocalDateTime(jsonNode).truncatedTo(ChronoUnit.DAYS)
                        .withDayOfMonth(1).toString())
        );
    }

    static JsonNode funcDateTruncateToYear(JsonNode node, String params) {
        return applyWithoutArgument(node, params,
                JsonNode::isTextual,
                jsonNode -> TextNode.valueOf(toLocalDateTime(jsonNode).truncatedTo(ChronoUnit.DAYS)
                        .withDayOfYear(1).toString())
        );
    }

    static JsonNode funcDateWith(JsonNode node, String params, ChronoField field) {
        return applyWithArguments(node, params, 1, 1,
                JsonNode::isTextual,
                (jsonNode, paramList) -> getNodeAsInt(jsonNode, paramList.get(0)),
                (jsonNode, objVar) -> TextNode.valueOf(toLocalDateTime(jsonNode).with(field, (int) objVar).toString())
        );
    }

    static JsonNode funcDayEnd(JsonNode node, String params) {
        return applyWithoutArgument(node, params,
                JsonNode::isTextual,
                jsonNode -> TextNode.valueOf(toLocalDateTime(jsonNode).truncatedTo(ChronoUnit.DAYS)
                        .plusDays(1).minusNanos(1).toString())
        );
    }

    static JsonNode funcMonthEnd(JsonNode node, String params) {
        return applyWithoutArgument(node, params,
                JsonNode::isTextual,
                jsonNode -> TextNode.valueOf(toLocalDateTime(jsonNode).truncatedTo(ChronoUnit.DAYS)
                        .withDayOfMonth(1).plusMonths(1).minusNanos(1).toString())
        );
    }

    static JsonNode funcYearEnd(JsonNode node, String params) {
        return applyWithoutArgument(node, params,
                JsonNode::isTextual,
                jsonNode -> TextNode.valueOf(toLocalDateTime(jsonNode).truncatedTo(ChronoUnit.DAYS)
                        .withDayOfYear(1).plusYears(1).minusNanos(1).toString())
        );
    }

    static JsonNode funcLengthOfMonth(JsonNode node, String params) {
        return applyWithoutArgument(node, params,
                JsonNode::isTextual,
                jsonNode -> IntNode.valueOf(toLocalDateTime(jsonNode).toLocalDate().lengthOfMonth())
        );
    }

    static JsonNode funcLengthOfYear(JsonNode node, String params) {
        return applyWithoutArgument(node, params,
                JsonNode::isTextual,
                jsonNode -> IntNode.valueOf(toLocalDateTime(jsonNode).toLocalDate().lengthOfYear())
        );
    }

    static JsonNode funcLocalToOffsetDate(JsonNode node, String params) {
        return applyWithoutArgument(node, params,
                JsonNode::isTextual,
                jsonNode -> TextNode.valueOf(localToOffsetDateTime(jsonNode).toString())
        );
    }

    static JsonNode funcOffsetToLocalDate(JsonNode node, String params) {
        return applyWithoutArgument(node, params,
                JsonNode::isTextual,
                jsonNode -> TextNode.valueOf(offsetToLocalDateTime(jsonNode).toString())
        );
    }
}

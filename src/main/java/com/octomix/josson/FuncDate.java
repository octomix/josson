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

    private FuncDate() {
    }

    static JsonNode funcAmPmOfDay(final JsonNode node, final String params) {
        return applyFunc(node, params,
                JsonNode::isTextual,
                jsonNode -> TextNode.valueOf(toLocalDateTime(jsonNode).get(ChronoField.AMPM_OF_DAY) == 0 ? "AM" : "PM")
        );
    }

    static JsonNode funcChronometry(final JsonNode node, final String params, final ChronoField field) {
        return applyFunc(node, params,
                JsonNode::isTextual,
                jsonNode -> IntNode.valueOf(toLocalDateTime(jsonNode).get(field))
        );
    }

    static JsonNode funcDatePlus(final JsonNode node, final String params, final ChronoUnit unit) {
        return applyFuncWithParamAsInt(node, params,
                JsonNode::isTextual,
                (jsonNode, objVar) -> TextNode.valueOf(toLocalDateTime(jsonNode).plus((int) objVar, unit).toString())
        );
    }

    static JsonNode funcDateMinus(final JsonNode node, final String params, final ChronoUnit unit) {
        return applyFuncWithParamAsInt(node, params,
                JsonNode::isTextual,
                (jsonNode, objVar) -> TextNode.valueOf(toLocalDateTime(jsonNode).minus((int) objVar, unit).toString())
        );
    }

    static JsonNode funcDateTruncateTo(final JsonNode node, final String params, final ChronoUnit unit) {
        return applyFunc(node, params,
                JsonNode::isTextual,
                jsonNode -> TextNode.valueOf(toLocalDateTime(jsonNode).truncatedTo(unit).toString())
        );
    }

    static JsonNode funcDateTruncateToMonth(final JsonNode node, final String params) {
        return applyFunc(node, params,
                JsonNode::isTextual,
                jsonNode -> TextNode.valueOf(toLocalDate(jsonNode)
                        .withDayOfMonth(1).toString())
        );
    }

    static JsonNode funcDateTruncateToYear(final JsonNode node, final String params) {
        return applyFunc(node, params,
                JsonNode::isTextual,
                jsonNode -> TextNode.valueOf(toLocalDate(jsonNode)
                        .withDayOfYear(1).toString())
        );
    }

    static JsonNode funcDateWith(final JsonNode node, final String params, final ChronoField field) {
        return applyFuncWithParamAsInt(node, params,
                JsonNode::isTextual,
                (jsonNode, objVar) -> TextNode.valueOf(toLocalDateTime(jsonNode).with(field, (int) objVar).toString())
        );
    }

    static JsonNode funcDayEnd(final JsonNode node, final String params) {
        return applyFunc(node, params,
                JsonNode::isTextual,
                jsonNode -> TextNode.valueOf(toLocalDate(jsonNode)
                        .plusDays(1).minusNanos(1).toString())
        );
    }

    static JsonNode funcMonthEnd(final JsonNode node, final String params) {
        return applyFunc(node, params,
                JsonNode::isTextual,
                jsonNode -> TextNode.valueOf(toLocalDate(jsonNode)
                        .withDayOfMonth(1).plusMonths(1).minusNanos(1).toString())
        );
    }

    static JsonNode funcYearEnd(final JsonNode node, final String params) {
        return applyFunc(node, params,
                JsonNode::isTextual,
                jsonNode -> TextNode.valueOf(toLocalDate(jsonNode)
                        .withDayOfYear(1).plusYears(1).minusNanos(1).toString())
        );
    }

    static JsonNode funcLengthOfMonth(final JsonNode node, final String params) {
        return applyFunc(node, params,
                JsonNode::isTextual,
                jsonNode -> IntNode.valueOf(toLocalDateTime(jsonNode).toLocalDate().lengthOfMonth())
        );
    }

    static JsonNode funcLengthOfYear(final JsonNode node, final String params) {
        return applyFunc(node, params,
                JsonNode::isTextual,
                jsonNode -> IntNode.valueOf(toLocalDateTime(jsonNode).toLocalDate().lengthOfYear())
        );
    }

    static JsonNode funcLocalToOffsetDate(final JsonNode node, final String params) {
        return applyFunc(node, params,
                JsonNode::isTextual,
                jsonNode -> TextNode.valueOf(localToOffsetDateTime(jsonNode).toString())
        );
    }

    static JsonNode funcOffsetToLocalDate(final JsonNode node, final String params) {
        return applyFunc(node, params,
                JsonNode::isTextual,
                jsonNode -> TextNode.valueOf(offsetToLocalDateTime(jsonNode).toString())
        );
    }
}

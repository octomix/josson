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
import java.util.List;

import static com.octomix.josson.FuncExecutor.*;
import static com.octomix.josson.JossonCore.*;

class FuncLogical {
    static BooleanNode funcContains(JsonNode node, String params, boolean ignoreCase, boolean not) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        JsonNode valueNode = getNodeByPath(node, pathAndParams.getValue().get(0));
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
            double value = valueNode.asDouble();
            if (node.isArray()) {
                for (int i = 0; i < node.size(); i++) {
                    if (node.get(i).isNumber() || node.get(i).isTextual()) {
                        if (node.get(i).asDouble() == value) {
                            return BooleanNode.valueOf(!not);
                        }
                    }
                }
            }
            return BooleanNode.valueOf(not);
        }
        String value = valueNode.asText();
        if (node.isTextual()) {
            return BooleanNode.valueOf(not ^ (ignoreCase ?
                    StringUtils.containsIgnoreCase(node.asText(), value) :
                    StringUtils.contains(node.asText(), value)));
        }
        if (node.isObject()) {
            return BooleanNode.valueOf(not ^ node.get(value) != null);
        }
        if (node.isArray()) {
            for (int i = 0; i < node.size(); i++) {
                if (node.get(i).isTextual()) {
                    if (ignoreCase) {
                        if (value.equalsIgnoreCase(node.get(i).asText())) {
                            return BooleanNode.valueOf(!not);
                        }
                    } else if (value.equals(node.get(i).asText())) {
                        return BooleanNode.valueOf(!not);
                    }
                }
            }
            return BooleanNode.valueOf(not);
        }
        return BooleanNode.FALSE;
    }

    static JsonNode funcEndsWith(JsonNode node, String params, boolean ignoreCase, boolean not) {
        return applyWithArguments(node, params, 1, 1,
                JsonNode::isTextual,
                (jsonNode, paramList) -> getNodeAsText(jsonNode, paramList.get(0)),
                (jsonNode, objVar) -> BooleanNode.valueOf(not ^ (ignoreCase ?
                        StringUtils.endsWithIgnoreCase(jsonNode.asText(), (String) objVar) :
                        StringUtils.endsWith(jsonNode.asText(), (String) objVar)))
        );
    }

    static JsonNode funcEquals(JsonNode node, String params, boolean ignoreCase, boolean not) {
        return applyWithArguments(node, params, 1, 1,
                JsonNode::isTextual,
                (jsonNode, paramList) -> getNodeAsText(jsonNode, paramList.get(0)),
                (jsonNode, objVar) -> BooleanNode.valueOf(not ^ (ignoreCase ?
                        StringUtils.equalsIgnoreCase(jsonNode.asText(), (String) objVar) :
                        StringUtils.equals(jsonNode.asText(), (String) objVar)))
        );
    }

    static BooleanNode funcIn(JsonNode node, String params, boolean ignoreCase, boolean not) {
        ArrayNode array = getParamArray(params, node);
        if (node.isNumber()) {
            double num = node.asDouble();
            for (int i = array.size() - 1; i >= 0; i--) {
                JsonNode value = array.get(i);
                if (value.isNumber() || value.isTextual()) {
                    if (value.asDouble() == num) {
                        return BooleanNode.valueOf(!not);
                    }
                }
            }
            return BooleanNode.valueOf(not);
        } else if (node.isTextual()) {
            String text = node.asText();
            for (int i = array.size() - 1; i >= 0; i--) {
                JsonNode value = array.get(i);
                if (value.isNumber() || value.isTextual()) {
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

    static JsonNode funcIsBlank(JsonNode node, String params, boolean not) {
        return applyWithoutArgument(node, params,
                jsonNode -> BooleanNode.valueOf(jsonNode.isTextual() && (not ^ StringUtils.isBlank(jsonNode.asText())))
        );
    }

    static JsonNode funcIsBoolean(JsonNode node, String params) {
        return applyWithoutArgument(node, params,
                jsonNode -> BooleanNode.valueOf(jsonNode.isBoolean())
        );
    }

    static JsonNode funcIsEmpty(JsonNode node, String params, boolean not) {
        return applyWithoutArgument(node, params,
                jsonNode -> BooleanNode.valueOf(jsonNode.isTextual() && (not ^ jsonNode.asText().isEmpty()))
        );
    }

    static JsonNode funcIsEven(JsonNode node, String params) {
        return applyWithoutArgument(node, params,
                jsonNode -> nodeHasValue(jsonNode) ? BooleanNode.valueOf((jsonNode.asInt() & 1) == 0) : BooleanNode.FALSE
        );
    }

    static JsonNode funcIsNull(JsonNode node, String params, boolean not) {
        return applyWithoutArgument(node, params,
                jsonNode -> BooleanNode.valueOf(not ^ jsonNode.isNull())
        );
    }

    static JsonNode funcIsNumber(JsonNode node, String params) {
        return applyWithoutArgument(node, params,
                jsonNode -> BooleanNode.valueOf(jsonNode.isNumber())
        );
    }

    static JsonNode funcIsOdd(JsonNode node, String params) {
        return applyWithoutArgument(node, params,
                jsonNode -> nodeHasValue(jsonNode) ? BooleanNode.valueOf((jsonNode.asInt() & 1) != 0) : BooleanNode.FALSE
        );
    }

    static JsonNode funcIsText(JsonNode node, String params) {
        return applyWithoutArgument(node, params,
                jsonNode -> BooleanNode.valueOf(jsonNode.isTextual())
        );
    }

    static JsonNode funcNot(JsonNode node, String params) {
        return applyWithoutArgument(node, params,
                jsonNode -> jsonNode.isBoolean() ? BooleanNode.valueOf(!jsonNode.asBoolean()) : BooleanNode.FALSE
        );
    }

    static JsonNode funcStartsWith(JsonNode node, String params, boolean ignoreCase, boolean not) {
        return applyWithArguments(node, params, 1, 1,
                JsonNode::isTextual,
                (jsonNode, paramList) -> getNodeAsText(jsonNode, paramList.get(0)),
                (jsonNode, objVar) -> BooleanNode.valueOf(not ^ (ignoreCase ?
                        StringUtils.startsWithIgnoreCase(jsonNode.asText(), (String) objVar) :
                        StringUtils.startsWith(jsonNode.asText(), (String) objVar)))
        );
    }

    static JsonNode funcIsWeekDay(JsonNode node, String params) {
        return applyWithoutArgument(node, params,
                jsonNode -> jsonNode.isTextual()
                        ? BooleanNode.valueOf(toLocalDateTime(jsonNode).get(ChronoField.DAY_OF_WEEK) <= 5)
                        : null
        );
    }

    static JsonNode funcIsWeekEnd(JsonNode node, String params) {
        return applyWithoutArgument(node, params,
                jsonNode -> jsonNode.isTextual()
                        ? BooleanNode.valueOf(toLocalDateTime(jsonNode).get(ChronoField.DAY_OF_WEEK) > 5)
                        : null
        );
    }

    static JsonNode funcIsLeapYear(JsonNode node, String params) {
        return applyWithoutArgument(node, params,
                jsonNode -> jsonNode.isTextual()
                        ? BooleanNode.valueOf(IsoChronology.INSTANCE.isLeapYear(toLocalDateTime(jsonNode).getYear()))
                        : null
        );
    }
}

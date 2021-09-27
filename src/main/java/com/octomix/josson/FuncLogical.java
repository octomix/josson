/*
 * Copyright 2020 Octomix Software Technology Limited
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

import java.util.List;

import static com.octomix.josson.GetFuncParam.*;
import static com.octomix.josson.Josson.getNode;
import static com.octomix.josson.JossonCore.*;

class FuncLogical {
    static BooleanNode funcContains(JsonNode node, String params, boolean ignoreCase, boolean not) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        if (pathAndParams.hasKey()) {
            node = getNode(node, pathAndParams.getKey());
            if (node == null) {
                return BooleanNode.FALSE;
            }
        }
        JsonNode valueNode = getNode(node, pathAndParams.getValue().get(0));
        if (valueNode.isContainerNode()) {
            return BooleanNode.FALSE;
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

    static BooleanNode funcEndsWith(JsonNode node, String params, boolean ignoreCase, boolean not) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        if (pathAndParams.hasKey()) {
            node = getNode(node, pathAndParams.getKey());
            if (node == null) {
                return BooleanNode.FALSE;
            }
        }
        String value = getNodeAsText(node, pathAndParams.getValue().get(0));
        if (!node.isTextual()) {
            return BooleanNode.FALSE;
        }
        return BooleanNode.valueOf(not ^ (ignoreCase ?
                StringUtils.endsWithIgnoreCase(node.asText(), value) :
                StringUtils.endsWith(node.asText(), value)));
    }

    static BooleanNode funcEquals(JsonNode node, String params, boolean ignoreCase, boolean not) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        if (pathAndParams.hasKey()) {
            node = getNode(node, pathAndParams.getKey());
            if (node == null) {
                return BooleanNode.FALSE;
            }
        }
        String value = getNodeAsText(node, pathAndParams.getValue().get(0));
        if (!node.isTextual()) {
            return BooleanNode.FALSE;
        }
        return BooleanNode.valueOf(not ^ (ignoreCase ?
                StringUtils.equalsIgnoreCase(node.asText(), value) :
                StringUtils.equals(node.asText(), value)));
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

    static BooleanNode funcIsBlank(JsonNode node, String params, boolean not) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        return BooleanNode.valueOf(node.isTextual() && (not ^ StringUtils.isBlank(node.asText())));
    }

    static BooleanNode funcIsBoolean(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        return BooleanNode.valueOf(node.isBoolean());
    }

    static BooleanNode funcIsEmpty(JsonNode node, String params, boolean not) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        return BooleanNode.valueOf(node.isTextual() && (not ^ node.asText().isEmpty()));
    }

    static BooleanNode funcIsEven(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        if (nodeHasValue(node)) {
            return BooleanNode.valueOf((node.asInt() & 1) == 0);
        }
        return BooleanNode.FALSE;
    }

    static BooleanNode funcIsNull(JsonNode node, String params, boolean not) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        return BooleanNode.valueOf(not ^ node.isNull());
    }

    static BooleanNode funcIsNumber(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        return BooleanNode.valueOf(node.isNumber());
    }

    static BooleanNode funcIsOdd(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        if (nodeHasValue(node)) {
            return BooleanNode.valueOf((node.asInt() & 1) != 0);
        }
        return BooleanNode.FALSE;
    }

    static BooleanNode funcIsText(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        return BooleanNode.valueOf(node.isTextual());
    }

    static BooleanNode funcNot(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        if (!node.isBoolean()) {
            return BooleanNode.FALSE;
        }
        return BooleanNode.valueOf(!node.asBoolean());
    }

    static BooleanNode funcStartsWith(JsonNode node, String params, boolean ignoreCase, boolean not) {
        Pair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        if (pathAndParams.hasKey()) {
            node = getNode(node, pathAndParams.getKey());
            if (node == null) {
                return BooleanNode.FALSE;
            }
        }
        String value = getNodeAsText(node, pathAndParams.getValue().get(0));
        if (!node.isTextual()) {
            return BooleanNode.FALSE;
        }
        return BooleanNode.valueOf(not ^ (ignoreCase ?
                StringUtils.startsWithIgnoreCase(node.asText(), value) :
                StringUtils.startsWith(node.asText(), value)));
    }
}

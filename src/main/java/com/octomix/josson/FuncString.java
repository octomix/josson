package com.octomix.josson;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.TextNode;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;

import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.regex.Matcher;

import static com.octomix.josson.GetFuncParam.*;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Josson.getNode;

class FuncString {
    static JsonNode funcAbbreviate(JsonNode node, String params) {
        ImmutablePair<Integer, Integer> startEnd = getParamStartEnd(params);
        int offset = startEnd.left;
        int maxWidth;
        if (startEnd.right < Integer.MAX_VALUE) {
            maxWidth = startEnd.right;
        } else {
            maxWidth = offset;
            offset = 0;
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(StringUtils.abbreviate(textNode.asText(), offset, maxWidth)));
                }
            }
            return array;
        } else if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.abbreviate(node.asText(), offset, maxWidth));
    }

    static JsonNode funcAppendIfMissing(JsonNode node, String params, boolean ignoreCase) {
        String suffix = getParamStringLiteral(params);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(ignoreCase ?
                            StringUtils.appendIfMissingIgnoreCase(textNode.asText(), suffix) :
                            StringUtils.appendIfMissing(textNode.asText(), suffix)));
                }
            }
            return array;
        } else if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(ignoreCase ?
                StringUtils.appendIfMissingIgnoreCase(node.asText(), suffix) :
                StringUtils.appendIfMissing(node.asText(), suffix));
    }

    static JsonNode funcB64Decode(JsonNode node, String params) {
        String value = getParamStringLiteral(params, false);
        if (value != null) {
            return TextNode.valueOf(new String(Base64.getDecoder().decode(value)));
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(
                            new String(Base64.getDecoder().decode(textNode.asText()))));
                }
            }
            return array;
        } else if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(new String(Base64.getDecoder().decode(node.asText())));
    }

    static JsonNode funcB64Encode(JsonNode node, String params) {
        String value = getParamStringLiteral(params, false);
        if (value != null) {
            return TextNode.valueOf(Base64.getEncoder().encodeToString(value.getBytes()));
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(
                            Base64.getEncoder().encodeToString(textNode.asText().getBytes())));
                }
            }
            return array;
        } else if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(Base64.getEncoder().encodeToString(node.asText().getBytes()));
    }

    static JsonNode funcCapitalize(JsonNode node, String params) {
        getParamNotAccept(params);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(StringUtils.capitalize(textNode.asText())));
                }
            }
            return array;
        } else if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.capitalize(node.asText()));
    }

    static JsonNode funcCenter(JsonNode node, String params) {
        ImmutablePair<Integer, String> args = getParamIntAndString(params);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isValueNode()) {
                    array.add(TextNode.valueOf(StringUtils.center(textNode.asText(), args.left, args.right)));
                }
            }
            return array;
        } else if (!node.isValueNode()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.center(node.asText(), args.left, args.right));
    }

    static JsonNode funcConcat(JsonNode node, String params) {
        List<ImmutablePair<Character, String>> args = new ArrayList<>();
        Matcher m = DECOMPOSE_PARAMETERS.matcher(params);
        while (m.find()) {
            String param = m.group(0).trim();
            if (param.isEmpty()) {
                continue;
            }
            if (param.charAt(0) == '\'') {
                args.add(new ImmutablePair<>('\'', unquoteString(param)));
            } else if ("#".equals(param)) {
                args.add(new ImmutablePair<>('#', null));
            } else if ("?".equals(param)) {
                args.add(new ImmutablePair<>('?', null));
            } else {
                args.add(new ImmutablePair<>('.', param));
            }
        }
        if (args.isEmpty()) {
            getParamThrowMissing();
        }
        if (!node.isArray()) {
            return TextNode.valueOf(funcConcatElement(node, args, 0));
        }
        ArrayNode array = MAPPER.createArrayNode();
        for (int i  = 0; i < node.size(); i++) {
            String text = funcConcatElement(node.get(i), args, i);
            if (text != null) {
                array.add(text);
            }
        }
        return array;
    }

    private static String funcConcatElement(JsonNode node, List<ImmutablePair<Character, String>> args, int index) {
        StringBuilder sb = new StringBuilder();
        for (ImmutablePair<Character, String> arg : args) {
            switch (arg.left) {
                case '\'':
                    sb.append(arg.right);
                    break;
                case '#':
                    sb.append(index + 1);
                    break;
                case '?':
                    sb.append(node.asText());
                    break;
                default:
                    JsonNode tryNode = getNode(node, arg.right);
                    if (tryNode == null || tryNode.isNull() || !tryNode.isValueNode()) {
                        return null;
                    }
                    sb.append(tryNode.asText());
            }
        }
        return sb.toString();
    }

    static JsonNode funcJoin(JsonNode node, String params) {
        String delimiter = getParamStringLiteral(params, false);
        if (!node.isArray()) {
            return node;
        }
        List<String> texts = new ArrayList<>();
        for (int i = 0; i < node.size(); i++) {
            JsonNode valueNode = node.get(i);
            if (valueNode.isValueNode()) {
                texts.add(valueNode.asText());
            }
        }
        return TextNode.valueOf(String.join(delimiter == null ? "" : delimiter, texts));
    }

    static JsonNode funcLeftPad(JsonNode node, String params) {
        ImmutablePair<Integer, String> args = getParamIntAndString(params);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isValueNode()) {
                    array.add(TextNode.valueOf(StringUtils.leftPad(textNode.asText(), args.left, args.right)));
                }
            }
            return array;
        } else if (!node.isValueNode()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.leftPad(node.asText(), args.left, args.right));
    }

    static JsonNode funcLowerCase(JsonNode node, String params) {
        getParamNotAccept(params);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(StringUtils.lowerCase(textNode.asText())));
                }
            }
            return array;
        } else if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.lowerCase(node.asText()));
    }

    static JsonNode funcPrependIfMissing(JsonNode node, String params, boolean ignoreCase) {
        String prefix = getParamStringLiteral(params);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(ignoreCase ?
                            StringUtils.prependIfMissingIgnoreCase(textNode.asText(), prefix) :
                            StringUtils.prependIfMissing(textNode.asText(), prefix)));
                }
            }
            return array;
        } else if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(ignoreCase ?
                StringUtils.prependIfMissingIgnoreCase(node.asText(), prefix) :
                StringUtils.prependIfMissing(node.asText(), prefix));
    }

    static JsonNode funcRightPad(JsonNode node, String params) {
        ImmutablePair<Integer, String> args = getParamIntAndString(params);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isValueNode()) {
                    array.add(TextNode.valueOf(StringUtils.rightPad(textNode.asText(), args.left, args.right)));
                }
            }
            return array;
        } else if (!node.isValueNode()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.rightPad(node.asText(), args.left, args.right));
    }

    static ArrayNode funcSplit(JsonNode node, String params) {
        String separator = getParamStringLiteral(params, false);
        ArrayNode array = MAPPER.createArrayNode();
        if (node.isArray()) {
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    for (String text : StringUtils.split(textNode.asText(), separator)) {
                        array.add(TextNode.valueOf(text));
                    }
                }
            }
        } else if (!node.isTextual()) {
            return null;
        } else {
            for (String text : StringUtils.split(node.asText(), separator)) {
                array.add(TextNode.valueOf(text));
            }
        }
        return array;
    }

    static JsonNode funcSubstr(JsonNode node, String params) {
        ImmutablePair<Integer, Integer> startEnd = getParamStartEnd(params);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(StringUtils.substring(
                            textNode.asText(), startEnd.left, startEnd.right)));
                }
            }
            return array;
        } else if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.substring(node.asText(), startEnd.left, startEnd.right));
    }

    static JsonNode funcTrim(JsonNode node, String params) {
        getParamNotAccept(params);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(StringUtils.trim(textNode.asText())));
                }
            }
            return array;
        } else if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.trim(node.asText()));
    }

    static JsonNode funcUpperCase(JsonNode node, String params) {
        getParamNotAccept(params);
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(StringUtils.upperCase(textNode.asText())));
                }
            }
            return array;
        } else if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.upperCase(node.asText()));
    }
}

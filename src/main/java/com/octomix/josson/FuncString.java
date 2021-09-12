package com.octomix.josson;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.IntNode;
import com.fasterxml.jackson.databind.node.TextNode;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.ImmutableTriple;

import java.util.ArrayList;
import java.util.List;

import static com.octomix.josson.GetFuncParam.*;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Josson.getNode;
import static com.octomix.josson.PatternMatcher.decomposeFunctionParameters;

class FuncString {
    static JsonNode funcAbbreviate(JsonNode node, String params) {
        ImmutablePair<String, Integer[]> pathAndParams = getParamPathAndStartEnd(params);
        if (pathAndParams.left != null) {
            node = getNode(node, pathAndParams.left);
            if (node == null) {
                return null;
            }
        }
        int offset = pathAndParams.right[0];
        int maxWidth;
        if (pathAndParams.right[1] < Integer.MAX_VALUE) {
            maxWidth = pathAndParams.right[1];
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
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.abbreviate(node.asText(), offset, maxWidth));
    }

    static JsonNode funcAppendIfMissing(JsonNode node, String params, boolean ignoreCase) {
        ImmutablePair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        if (pathAndParams.left != null) {
            node = getNode(node, pathAndParams.left);
            if (node == null) {
                return null;
            }
        }
        String suffix = getNodeAsText(node, pathAndParams.right.get(0));
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
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(ignoreCase ?
                StringUtils.appendIfMissingIgnoreCase(node.asText(), suffix) :
                StringUtils.appendIfMissing(node.asText(), suffix));
    }

    static JsonNode funcCapitalize(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(StringUtils.capitalize(textNode.asText())));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.capitalize(node.asText()));
    }

    static JsonNode funcCenter(JsonNode node, String params) {
        ImmutableTriple<String, Integer, String> pathAndParams = getParamPathAndAlignment(params);
        if (pathAndParams.left != null) {
            node = getNode(node, pathAndParams.left);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (nodeHasValue(textNode)) {
                    array.add(TextNode.valueOf(StringUtils.center(
                            textNode.asText(), pathAndParams.middle, pathAndParams.right)));
                }
            }
            return array;
        }
        if (!nodeHasValue(node)) {
            return null;
        }
        return TextNode.valueOf(StringUtils.center(node.asText(), pathAndParams.middle, pathAndParams.right));
    }

    static JsonNode funcConcat(JsonNode node, String params) {
        List<String> paramList = decomposeFunctionParameters(params, 1, -1);
        List<ImmutablePair<Character, String>> args = new ArrayList<>();
        for (String param : paramList) {
            if (param.isEmpty()) {
                continue;
            }
            if (param.charAt(0) == '\'') {
                args.add(ImmutablePair.of('\'', unquoteString(param)));
            } else if ("?".equals(param)) {
                args.add(ImmutablePair.of('?', null));
            } else if (param.startsWith("#")) {
                args.add(ImmutablePair.of('#', param));
            } else {
                args.add(ImmutablePair.of('.', param));
            }
        }
        if (!node.isArray()) {
            return TextNode.valueOf(funcConcatElement(node, args, 0));
        }
        ArrayNode array = MAPPER.createArrayNode();
        for (int i  = 0; i < node.size(); i++) {
            String text = funcConcatElement(node.get(i), args, array.size());
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
                    continue;
                case '?':
                    sb.append(node.asText());
                    continue;
            }
            JsonNode tryNode = arg.left == '#' ? getIndexId(index, arg.right) : getNode(node, arg.right);
            if (!nodeHasValue(tryNode)) {
                return null;
            }
            sb.append(tryNode.asText());
        }
        return sb.toString();
    }

    static JsonNode funcJoin(JsonNode node, String params) {
        ImmutablePair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 0, 1);
        if (pathAndParams.left != null) {
            node = getNode(node, pathAndParams.left);
            if (node == null) {
                return null;
            }
        }
        String delimiter = pathAndParams.right.size() > 0 ? getNodeAsText(node, pathAndParams.right.get(0)) : "";
        List<String> texts = new ArrayList<>();
        for (int i = 0; i < node.size(); i++) {
            JsonNode valueNode = node.get(i);
            if (nodeHasValue(valueNode)) {
                texts.add(valueNode.asText());
            }
        }
        return TextNode.valueOf(String.join(delimiter, texts));
    }

    static JsonNode funcKeepAfter(JsonNode node, String params, boolean ignoreCase, boolean last) {
        ImmutablePair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        if (pathAndParams.left != null) {
            node = getNode(node, pathAndParams.left);
            if (node == null) {
                return null;
            }
        }
        String find = getNodeAsText(node, pathAndParams.right.get(0));
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    if (find.isEmpty()) {
                        array.add(textNode);
                    } else {
                        String text = textNode.asText();
                        int pos = last ?
                                (ignoreCase ?
                                        StringUtils.lastIndexOfIgnoreCase(text, find) :
                                        StringUtils.lastIndexOf(text, find)) :
                                (ignoreCase ?
                                        StringUtils.indexOfIgnoreCase(text, find) :
                                        StringUtils.indexOf(text, find));
                        array.add(pos < 0 ? "" : text.substring(pos + find.length()));
                    }
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        if (find.isEmpty()) {
            return node;
        }
        String text = node.asText();
        int pos = last ?
                (ignoreCase ?
                        StringUtils.lastIndexOfIgnoreCase(text, find) :
                        StringUtils.lastIndexOf(text, find)) :
                (ignoreCase ?
                        StringUtils.indexOfIgnoreCase(text, find) :
                        StringUtils.indexOf(text, find));
        return TextNode.valueOf(pos < 0 ? "" : text.substring(pos + find.length()));
    }

    static JsonNode funcKeepBefore(JsonNode node, String params, boolean ignoreCase, boolean last) {
        ImmutablePair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        if (pathAndParams.left != null) {
            node = getNode(node, pathAndParams.left);
            if (node == null) {
                return null;
            }
        }
        String find = getNodeAsText(node, pathAndParams.right.get(0));
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    String text = textNode.asText();
                    if (find.isEmpty()) {
                        array.add(text);
                    } else {
                        int pos = last ?
                                (ignoreCase ?
                                        StringUtils.lastIndexOfIgnoreCase(text, find) :
                                        StringUtils.lastIndexOf(text, find)) :
                                (ignoreCase ?
                                        StringUtils.indexOfIgnoreCase(text, find) :
                                        StringUtils.indexOf(text, find));
                        array.add(pos < 0 ? "" : text.substring(0, pos));
                    }
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        if (find.isEmpty()) {
            return node;
        }
        String text = node.asText();
        int pos = last ?
                (ignoreCase ?
                        StringUtils.lastIndexOfIgnoreCase(text, find) :
                        StringUtils.lastIndexOf(text, find)) :
                (ignoreCase ?
                        StringUtils.indexOfIgnoreCase(text, find) :
                        StringUtils.indexOf(text, find));
        return TextNode.valueOf(pos < 0 ? "" : text.substring(0, pos));
    }

    static JsonNode funcLeftPad(JsonNode node, String params) {
        ImmutableTriple<String, Integer, String> pathAndParams = getParamPathAndAlignment(params);
        if (pathAndParams.left != null) {
            node = getNode(node, pathAndParams.left);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (nodeHasValue(textNode)) {
                    array.add(TextNode.valueOf(StringUtils.leftPad(
                            textNode.asText(), pathAndParams.middle, pathAndParams.right)));
                }
            }
            return array;
        }
        if (!nodeHasValue(node)) {
            return null;
        }
        return TextNode.valueOf(StringUtils.leftPad(node.asText(), pathAndParams.middle, pathAndParams.right));
    }

    static JsonNode funcLength(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isValueNode()) {
                    array.add(IntNode.valueOf(textNode.asText().length()));
                }
            }
            return array;
        }
        if (!node.isValueNode()) {
            return null;
        }
        return IntNode.valueOf(node.asText().length());
    }

    static JsonNode funcLowerCase(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(StringUtils.lowerCase(textNode.asText())));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.lowerCase(node.asText()));
    }

    static JsonNode funcNotBlank(JsonNode node, String params) {
        List<String> paramList = decomposeFunctionParameters(params, 1, -1);
        if (node.isArray() && paramList.size() == 1) {
            for (int i = 0; i < node.size(); i++) {
                JsonNode tryNode = getNode(node.get(i), paramList.get(0));
                if (tryNode != null && tryNode.isTextual() && StringUtils.isNotBlank(tryNode.asText())) {
                    return tryNode;
                }
            }
            return null;
        }
        return funcNotBlank(node, paramList);
    }

    static JsonNode funcNotBlank(JsonNode node, List<String> paramList) {
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                JsonNode tryNode = funcNotBlank(node.get(i), paramList);
                if (tryNode != null) {
                    array.add(tryNode);
                }
            }
            return array;
        }
        if (node.isValueNode()) {
            if (node.isTextual() && StringUtils.isNotBlank(node.asText())) {
                return node;
            }
            for (String path : paramList) {
                if (path.charAt(0) == '\'') {
                    String text = unquoteString(path);
                    if (StringUtils.isNotBlank(text)) {
                        return TextNode.valueOf(text);
                    }
                }
            }
        } else if (node.isObject()) {
            for (String path : paramList) {
                JsonNode tryNode = getNode(node, path);
                if (tryNode != null && tryNode.isTextual() && StringUtils.isNotBlank(tryNode.asText())) {
                    return tryNode;
                }
            }
        }
        return null;
    }

    static JsonNode funcNotEmpty(JsonNode node, String params) {
        List<String> paramList = decomposeFunctionParameters(params, 1, -1);
        if (node.isArray() && paramList.size() == 1) {
            for (int i = 0; i < node.size(); i++) {
                JsonNode tryNode = getNode(node.get(i), paramList.get(0));
                if (tryNode != null && tryNode.isTextual() && !tryNode.asText().isEmpty()) {
                    return tryNode;
                }
            }
            return null;
        }
        return funcNotEmpty(node, paramList);
    }

    static JsonNode funcNotEmpty(JsonNode node, List<String> paramList) {
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i = 0; i < node.size(); i++) {
                JsonNode tryNode = funcNotEmpty(node.get(i), paramList);
                if (tryNode != null) {
                    array.add(tryNode);
                }
            }
            return array;
        }
        if (node.isValueNode()) {
            if (node.isTextual() && !node.asText().isEmpty()) {
                return node;
            }
            for (String path : paramList) {
                if (path.charAt(0) == '\'') {
                    String text = unquoteString(path);
                    if (!text.isEmpty()) {
                        return TextNode.valueOf(text);
                    }
                }
            }
        } else if (node.isObject()) {
            for (String path : paramList) {
                JsonNode tryNode = getNode(node, path);
                if (tryNode != null && tryNode.isTextual() && !tryNode.asText().isEmpty()) {
                    return tryNode;
                }
            }
        }
        return null;
    }

    static JsonNode funcPrependIfMissing(JsonNode node, String params, boolean ignoreCase) {
        ImmutablePair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        if (pathAndParams.left != null) {
            node = getNode(node, pathAndParams.left);
            if (node == null) {
                return null;
            }
        }
        String prefix = getNodeAsText(node, pathAndParams.right.get(0));
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
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(ignoreCase ?
                StringUtils.prependIfMissingIgnoreCase(node.asText(), prefix) :
                StringUtils.prependIfMissing(node.asText(), prefix));
    }

    static JsonNode funcRemoveEnd(JsonNode node, String params, boolean ignoreCase) {
        ImmutablePair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        if (pathAndParams.left != null) {
            node = getNode(node, pathAndParams.left);
            if (node == null) {
                return null;
            }
        }
        String remove = getNodeAsText(node, pathAndParams.right.get(0));
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(ignoreCase ?
                            StringUtils.removeEndIgnoreCase(textNode.asText(), remove) :
                            StringUtils.removeEnd(textNode.asText(), remove)));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(ignoreCase ?
                StringUtils.removeEndIgnoreCase(node.asText(), remove) :
                StringUtils.removeEnd(node.asText(), remove));
    }

    static JsonNode funcRemoveStart(JsonNode node, String params, boolean ignoreCase) {
        ImmutablePair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 1, 1);
        if (pathAndParams.left != null) {
            node = getNode(node, pathAndParams.left);
            if (node == null) {
                return null;
            }
        }
        String remove = getNodeAsText(node, pathAndParams.right.get(0));
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(ignoreCase ?
                            StringUtils.removeStartIgnoreCase(textNode.asText(), remove) :
                            StringUtils.removeStart(textNode.asText(), remove)));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(ignoreCase ?
                StringUtils.removeStartIgnoreCase(node.asText(), remove) :
                StringUtils.removeStart(node.asText(), remove));
    }

    static JsonNode funcReplace(JsonNode node, String params, boolean ignoreCase) {
        ImmutableTriple<String, String[], Integer> pathAndParams = getParamPathAnd2StringAndInt(params);
        if (pathAndParams.left != null) {
            node = getNode(node, pathAndParams.left);
            if (node == null) {
                return null;
            }
        }

        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(ignoreCase ?
                            StringUtils.replaceIgnoreCase(
                                    textNode.asText(), pathAndParams.middle[0], pathAndParams.middle[1], pathAndParams.right == null ? -1 : pathAndParams.right) :
                            StringUtils.replace(
                                    textNode.asText(), pathAndParams.middle[0], pathAndParams.middle[1], pathAndParams.right == null ? -1 : pathAndParams.right)));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(ignoreCase ?
                StringUtils.replaceIgnoreCase(
                        node.asText(), pathAndParams.middle[0], pathAndParams.middle[1], pathAndParams.right == null ? -1 : pathAndParams.right) :
                StringUtils.replace(
                        node.asText(), pathAndParams.middle[0], pathAndParams.middle[1], pathAndParams.right == null ? -1 : pathAndParams.right));
    }

    static JsonNode funcRightPad(JsonNode node, String params) {
        ImmutableTriple<String, Integer, String> pathAndParams = getParamPathAndAlignment(params);
        if (pathAndParams.left != null) {
            node = getNode(node, pathAndParams.left);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (nodeHasValue(textNode)) {
                    array.add(TextNode.valueOf(StringUtils.rightPad(
                            textNode.asText(), pathAndParams.middle, pathAndParams.right)));
                }
            }
            return array;
        }
        if (!nodeHasValue(node)) {
            return null;
        }
        return TextNode.valueOf(StringUtils.rightPad(node.asText(), pathAndParams.middle, pathAndParams.right));
    }

    static ArrayNode funcSplit(JsonNode node, String params) {
        ImmutablePair<String, List<String>> pathAndParams = getParamPathAndStrings(params, 0, 1);
        if (pathAndParams.left != null) {
            node = getNode(node, pathAndParams.left);
            if (node == null) {
                return null;
            }
        }
        String separator = pathAndParams.right.size() > 0 ? getNodeAsText(node, pathAndParams.right.get(0)) : null;
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
        ImmutablePair<String, Integer[]> pathAndParams = getParamPathAndStartEnd(params);
        if (pathAndParams.left != null) {
            node = getNode(node, pathAndParams.left);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(StringUtils.substring(
                            textNode.asText(), pathAndParams.right[0], pathAndParams.right[1])));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.substring(node.asText(), pathAndParams.right[0], pathAndParams.right[1]));
    }

    static JsonNode funcTrim(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(StringUtils.trim(textNode.asText())));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.trim(node.asText()));
    }

    static JsonNode funcUpperCase(JsonNode node, String params) {
        String path = getParamPath(params);
        if (path != null) {
            node = getNode(node, path);
            if (node == null) {
                return null;
            }
        }
        if (node.isArray()) {
            ArrayNode array = MAPPER.createArrayNode();
            for (int i  = 0; i < node.size(); i++) {
                JsonNode textNode = node.get(i);
                if (textNode.isTextual()) {
                    array.add(TextNode.valueOf(StringUtils.upperCase(textNode.asText())));
                }
            }
            return array;
        }
        if (!node.isTextual()) {
            return null;
        }
        return TextNode.valueOf(StringUtils.upperCase(node.asText()));
    }
}

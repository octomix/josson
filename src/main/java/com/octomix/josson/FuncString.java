/*
 * Copyright 2020-2023 Octomix Software Technology Limited
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
import com.fasterxml.jackson.databind.node.IntNode;
import com.fasterxml.jackson.databind.node.TextNode;
import com.octomix.josson.commons.CaseUtils;
import com.octomix.josson.commons.StringUtils;

import java.util.function.Function;

import static com.octomix.josson.FuncExecutor.*;
import static com.octomix.josson.JossonCore.*;
import static com.octomix.josson.Mapper.MAPPER;
import static com.octomix.josson.Utils.nodeHasValue;
import static com.octomix.josson.Utils.quoteText;
import static com.octomix.josson.commons.StringUtils.EMPTY;

/**
 * String functions.
 */
final class FuncString {

    private FuncString() {
    }

    static PathTrace funcAbbreviate(final PathTrace path, final String params) {
        return applyWithParams(path, params, 1, 2, JsonNode::isTextual,
            (data, paramList) -> {
                final PathTrace dataPath = data.getKey();
                final PathTrace paramPath = data.getValue() < 0 ? dataPath : path;
                int offset = paramList.get(0).isEmpty() ? 0 : getNodeAsInt(paramPath, data.getValue(), paramList.get(0));
                final int maxWidth;
                if (paramList.size() > 1) {
                    maxWidth = getNodeAsInt(paramPath, data.getValue(), paramList.get(1));
                } else {
                    maxWidth = offset;
                    offset = 0;
                }
                return path.push(TextNode.valueOf(StringUtils.abbreviate(dataPath.node().asText(), offset, maxWidth)));
            });
    }

    static PathTrace funcAppend(final PathTrace path, final String params) {
        return applyTextNodeWithParamAsText(path, params, (str, param) -> str + param);
    }

    static PathTrace funcAppendIfMissing(final PathTrace path, final String params, final boolean ignoreCase) {
        return applyTextNodeWithParamAsText(path, params,
            (str, param) -> ignoreCase ? StringUtils.appendIfMissingIgnoreCase(str, param) : StringUtils.appendIfMissing(str, param)
        );
    }

    static PathTrace funcCamelCase(final PathTrace path, final String params, final boolean capitalizeFirstLetter) {
        return applyWithParams(path, params, 0, 1, JsonNode::isTextual,
            (data, paramList) -> {
                final PathTrace dataPath = data.getKey();
                final PathTrace paramPath = data.getValue() < 0 ? dataPath : path;
                final String delimiters = paramList.size() > 0 ? getNodeAsText(paramPath, data.getValue(), paramList.get(0)) : " _.";
                return path.push(TextNode.valueOf(CaseUtils.toCamelCase(dataPath.node().asText(), capitalizeFirstLetter, delimiters)));
            });
    }

    static PathTrace funcCapitalize(final PathTrace path, final String params) {
        return applyTextNode(path, params, dataPath -> StringUtils.capitalize(dataPath.node().asText()));
    }

    static PathTrace funcConcat(final PathTrace path, final String params, final boolean notNull) {
        return applyWithParams(path, params, 1, UNLIMITED_AND_NO_PATH, null,
            (data, paramList) -> {
                final StringBuilder sb = new StringBuilder();
                for (String expression : paramList) {
                    final JsonNode result = getNodeByExpression(path, data.getValue(), expression);
                    if (nodeHasValue(result)) {
                        sb.append(result.asText());
                    } else if (notNull) {
                        return null;
                    }
                }
                return path.push(TextNode.valueOf(sb.toString()));
            });
    }

    static PathTrace funcDoubleQuote(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, Utils::nodeHasValue,
            (data, paramList) -> path.push(TextNode.valueOf(
                    String.format("\"%s\"", data.getKey().node().asText().replace("\"", "\\\""))))
        );
    }

    static PathTrace funcEval(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, JsonNode::isTextual,
            (data, paramList) -> path.push(getNodeByExpression(path, data.getKey().node().asText())));
    }

    static PathTrace funcKeep(final PathTrace path, final String params,
                              final boolean ignoreCase, final boolean after, final boolean last) {
        return applyTextNodeWithParamAsText(path, params,
            (str, param) -> {
                final int pos = last
                    ? (ignoreCase ? StringUtils.lastIndexOfIgnoreCase(str, param) : StringUtils.lastIndexOf(str, param))
                    : (ignoreCase ? StringUtils.indexOfIgnoreCase(str, param) : StringUtils.indexOf(str, param));
                return pos < 0 ? EMPTY
                    : after ? str.substring(pos + param.length())
                    : str.substring(0, pos);
            });
    }

    static PathTrace funcLength(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, Utils::nodeHasValue,
            (data, paramList) -> path.push(IntNode.valueOf(data.getKey().node().asText().length())));
    }

    static PathTrace funcLowerCase(final PathTrace path, final String params) {
        return applyTextNode(path, params, dataPath -> StringUtils.lowerCase(dataPath.node().asText()));
    }

    static PathTrace funcNotBlankOrEmpty(final PathTrace path, final String params,
                                         final Function<CharSequence, Boolean> transform) {
        return applyWithParams(path, params, 1, UNLIMITED_AND_NO_PATH, null,
            (data, paramList) -> {
                final PathTrace dataPath = data.getKey();
                if (dataPath.node().isTextual() && transform.apply(dataPath.node().asText())) {
                    return dataPath;
                }
                for (String expression : paramList) {
                    final PathTrace result = getPathByExpression(path, data.getValue(), expression);
                    if (result != null && result.node().isTextual() && transform.apply(result.node().asText())) {
                        return result;
                    }
                }
                return null;
            });
    }

    static PathTrace funcPadding(final PathTrace path, final String params, final int alignment) {
        return applyWithParams(path, params, 1, 2, Utils::nodeHasValue,
            (data, paramList) -> {
                final PathTrace dataPath = data.getKey();
                final PathTrace paramPath = data.getValue() < 0 ? dataPath : path;
                final int size = getNodeAsInt(paramPath, data.getValue(), paramList.get(0));
                final String padStr = paramList.size() > 1 ? getNodeAsTextExceptNull(paramPath, data.getValue(), paramList.get(1)) : null;
                return path.push(TextNode.valueOf(
                        alignment < 0 ? StringUtils.leftPad(dataPath.node().asText(), size, padStr)
                        : alignment > 0 ? StringUtils.rightPad(dataPath.node().asText(), size, padStr)
                        : StringUtils.center(dataPath.node().asText(), size, padStr))
                );
            });
    }

    static PathTrace funcPrepend(final PathTrace path, final String params) {
        return applyTextNodeWithParamAsText(path, params, (str, param) -> param + str);
    }

    static PathTrace funcPrependIfMissing(final PathTrace path, final String params, final boolean ignoreCase) {
        return applyTextNodeWithParamAsText(path, params,
            (str, param) -> ignoreCase ? StringUtils.prependIfMissingIgnoreCase(str, param) : StringUtils.prependIfMissing(str, param)
        );
    }

    static PathTrace funcRemoveEnd(final PathTrace path, final String params, final boolean ignoreCase) {
        return applyTextNodeWithParamAsText(path, params,
            (str, param) -> ignoreCase ? StringUtils.removeEndIgnoreCase(str, param) : StringUtils.removeEnd(str, param)
        );
    }

    static PathTrace funcRemoveStart(final PathTrace path, final String params, final boolean ignoreCase) {
        return applyTextNodeWithParamAsText(path, params,
            (str, param) -> ignoreCase ? StringUtils.removeStartIgnoreCase(str, param) : StringUtils.removeStart(str, param)
        );
    }

    static PathTrace funcRepeat(final PathTrace path, final String params) {
        return applyWithParams(path, params, 1, 1, Utils::nodeHasValue,
            (data, paramList) -> {
                final PathTrace dataPath = data.getKey();
                final PathTrace paramPath = data.getValue() < 0 ? dataPath : path;
                final int param = getNodeAsInt(paramPath, data.getValue(), paramList.get(0));
                return path.push(TextNode.valueOf(StringUtils.repeat(dataPath.node().asText(), param)));
            });
    }

    static PathTrace funcReplace(final PathTrace path, final String params, final boolean ignoreCase) {
        return applyWithParams(path, params, 2, 3, JsonNode::isTextual,
            (data, paramList) -> {
                final PathTrace dataPath = data.getKey();
                final PathTrace paramPath = data.getValue() < 0 ? dataPath : path;
                final String searchString = getNodeAsText(paramPath, data.getValue(), paramList.get(0));
                final String replacement = getNodeAsText(paramPath, data.getValue(), paramList.get(1));
                final int max = paramList.size() > 2 ? getNodeAsInt(paramPath, data.getValue(), paramList.get(2)) : -1;
                return path.push(TextNode.valueOf(StringUtils.replace(dataPath.node().asText(), searchString, replacement, max, ignoreCase)));
            });
    }

    static PathTrace funcSingleQuote(final PathTrace path, final String params) {
        return applyWithoutParam(path, params, Utils::nodeHasValue,
            (data, paramList) -> path.push(TextNode.valueOf(quoteText(data.getKey().node().asText())))
        );
    }

    static PathTrace funcSnakeCase(final PathTrace path, final String params, final CaseUtils.Type type) {
        return applyWithoutParam(path, params, JsonNode::isTextual,
            (data, paramList) -> path.push(TextNode.valueOf(CaseUtils.toSnakeCase(data.getKey().node().asText(), type))));
    }

    static PathTrace funcSplit(final PathTrace path, final String params, final boolean wholeSeparator) {
        return applyWithParams(path, params, 0, 1, JsonNode::isTextual,
            (data, paramList) -> {
                final PathTrace dataPath = data.getKey();
                final PathTrace paramPath = data.getValue() < 0 ? dataPath : path;
                final String separator = paramList.size() > 0 ? getNodeAsTextExceptNull(paramPath, data.getValue(), paramList.get(0)) : null;
                final ArrayNode array = MAPPER.createArrayNode();
                for (String text : wholeSeparator
                        ? StringUtils.separate(dataPath.node().asText(), separator)
                        : StringUtils.split(dataPath.node().asText(), separator)) {
                    array.add(TextNode.valueOf(text));
                }
                return path.push(array);
            });
    }

    static PathTrace funcSplitMax(final PathTrace path, final String params, final boolean wholeSeparator) {
        return applyWithParams(path, params, 2, 3, JsonNode::isTextual,
            (data, paramList) -> {
                final PathTrace dataPath = data.getKey();
                final PathTrace paramPath = data.getValue() < 0 ? dataPath : path;
                final String separator = getNodeAsTextExceptNull(paramPath, data.getValue(), paramList.get(0));
                final int max = getNodeAsInt(paramPath, data.getValue(), paramList.get(1));
                final boolean preserveAllTokens = paramList.size() > 2 && getNodeAsBoolean(paramPath, data.getValue(), paramList.get(2));
                final ArrayNode array = MAPPER.createArrayNode();
                for (String text : wholeSeparator
                        ? StringUtils.separateWorker(dataPath.node().asText(), separator, max, preserveAllTokens)
                        : StringUtils.splitWorker(dataPath.node().asText(), separator, max, preserveAllTokens)) {
                    array.add(TextNode.valueOf(text));
                }
                return path.push(array);
            });
    }

    static PathTrace funcStrip(final PathTrace path, final String params) {
        return applyTextNodeWithParamAsText(path, params, StringUtils::strip);
    }

    static PathTrace funcStripEnd(final PathTrace path, final String params) {
        return applyTextNodeWithParamAsText(path, params, StringUtils::stripEnd);
    }

    static PathTrace funcStripStart(final PathTrace path, final String params) {
        return applyTextNodeWithParamAsText(path, params, StringUtils::stripStart);
    }

    static PathTrace funcSubstr(final PathTrace path, final String params) {
        return applyWithParams(path, params, 1, 2, JsonNode::isTextual,
            (data, paramList) -> {
                final PathTrace dataPath = data.getKey();
                final PathTrace paramPath = data.getValue() < 0 ? dataPath : path;
                final int start = (paramList.get(0)).isEmpty() ? 0 : getNodeAsInt(paramPath, data.getValue(), paramList.get(0));
                final int end = paramList.size() > 1 ? getNodeAsInt(paramPath, data.getValue(), paramList.get(1)) : Integer.MAX_VALUE;
                return path.push(TextNode.valueOf(StringUtils.substring(dataPath.node().asText(), start, end)));
            });
    }

    static PathTrace funcTrim(final PathTrace path, final String params) {
        return applyTextNode(path, params, dataPath -> StringUtils.trim(dataPath.node().asText()));
    }

    static PathTrace funcUncapitalize(final PathTrace path, final String params) {
        return applyTextNode(path, params, dataPath -> StringUtils.uncapitalize(dataPath.node().asText()));
    }

    static PathTrace funcUpperCase(final PathTrace path, final String params) {
        return applyTextNode(path, params, dataPath -> StringUtils.upperCase(dataPath.node().asText()));
    }
}

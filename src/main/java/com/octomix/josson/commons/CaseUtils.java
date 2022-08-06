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

package com.octomix.josson.commons;

import static com.octomix.josson.commons.StringUtils.EMPTY;

/**
 * <p>Case manipulation operations on Strings that contain words.</p>
 */
 public class CaseUtils {

    /**
     * Case types
     */
    public enum Type {

        /**
         * Upper case.
         */
        UPPER,

        /**
         * Lower case.
         */
        LOWER,

        /**
         * Camel case.
         */
        CAMEL,

        /**
         * No change case operation.
         */
        UNDEFINED,
    }

    public static String toCamelCase(final String str, final boolean capitalizeFirstLetter, final String delimiters) {
        final String lower = str.toLowerCase();
        final int strLen = lower.length();
        final int[] newCodePoints = new int[strLen];
        int outOffset = 0;
        boolean capitalizeNext = capitalizeFirstLetter;
        for (int index = 0; index < strLen;) {
            final int codePoint = lower.codePointAt(index);
            if (delimiters.indexOf(codePoint) >= 0) {
                capitalizeNext = outOffset != 0;
                index += Character.charCount(codePoint);
            } else if (capitalizeNext || outOffset == 0 && capitalizeFirstLetter) {
                final int titleCaseCodePoint = Character.toTitleCase(codePoint);
                newCodePoints[outOffset++] = titleCaseCodePoint;
                index += Character.charCount(titleCaseCodePoint);
                capitalizeNext = false;
            } else {
                newCodePoints[outOffset++] = codePoint;
                index += Character.charCount(codePoint);
            }
        }
        if (outOffset != 0) {
            return new String(newCodePoints, 0, outOffset);
        }
        return lower;
    }

    public static String toSnakeCase(final String str, final Type type) {
        final int strLen = str.length();
        final int[] newCodePoints = new int[strLen * 2];
        int addCodePoint = '_';
        int outOffset = 0;
        for (int index = 0; index < strLen;) {
            final int codePoint = str.codePointAt(index);
            if (codePoint == ' ' || codePoint == '_') {
                if (addCodePoint == '_') {
                    index++;
                    continue;
                }
                addCodePoint = '_';
            } else {
                if (addCodePoint != '_' && (Character.isUpperCase(codePoint) || Character.isTitleCase(codePoint))) {
                    addCodePoint = '_';
                    newCodePoints[outOffset++] = addCodePoint;
                }
                if (type == Type.UNDEFINED) {
                    addCodePoint = codePoint;
                } else if (type == Type.CAMEL && addCodePoint == '_') {
                    addCodePoint = Character.toTitleCase(codePoint);
                } else if (type == Type.UPPER) {
                    addCodePoint = Character.toUpperCase(codePoint);
                } else {
                    addCodePoint = Character.toLowerCase(codePoint);
                }
            }
            newCodePoints[outOffset++] = addCodePoint;
            index += Character.charCount(codePoint);
        }
        if (outOffset != 0 && (addCodePoint != '_' || --outOffset != 0)) {
            return new String(newCodePoints, 0, outOffset);
        }
        return EMPTY;
    }
}

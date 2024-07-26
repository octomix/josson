/*
 * Copyright 2020-2024 Choi Wai Man Raymond
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

import com.octomix.josson.commons.StringUtils;
import com.octomix.josson.exception.SyntaxErrorException;

/**
 * Different action behaviours for the function {@code mergeArrays()}.
 * <br>{@link #APPEND}
 * <br>{@link #INTEGRATE}
 * <br>{@link #REPLACE_WHOLE}
 * <br>{@link #REPLACE_BY_INDEX}
 */
public enum MergeArraysOption {

    /**
     * Append all the 2nd array elements to the 1st array.
     */
    APPEND("append"),

    /**
     * Append 2nd array element to the 1st array only if the element does not exist.
     */
    INTEGRATE("integrate"),

    /**
     * Replace the whole array by the 2nd array.
     */
    REPLACE_WHOLE("replacewhole"),

    /**
     * Replace by the 2nd array. If the 1st array is longer than the 2nd one, the remaining elements remain intact.
     */
    REPLACE_BY_INDEX("replacebyindex");

    private final String value;

    MergeArraysOption(final String value) {
        this.value = value;
    }

    public static MergeArraysOption fromValue(final String value) {
        if (StringUtils.isBlank(value)) {
            throw new SyntaxErrorException(value, "Missing option");
        }
        final String lowercase = value.toLowerCase();
        for (MergeArraysOption option : values()) {
            if (option.value.equals(lowercase)) {
                return option;
            }
        }
        throw new SyntaxErrorException(value, "Invalid option");
    }
}

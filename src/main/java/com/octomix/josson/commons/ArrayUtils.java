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

package com.octomix.josson.commons;

import java.lang.reflect.Array;

class ArrayUtils {

    static final String[] EMPTY_STRING_ARRAY = new String[0];

    static boolean isEmpty(Object[] array) {
        return array == null || Array.getLength(array) == 0;
    }

    static <T> boolean isNotEmpty(T[] array) {
        return !isEmpty(array);
    }

}

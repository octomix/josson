/*
 * Copyright 2020-2025 Choi Wai Man Raymond
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

/**
 * A key-value map entry.
 *
 * @param <K> key
 * @param <V> value
 */
class Pair<K, V> {

    private final K key;

    private final V value;

    private Pair(final K key, final V value) {
        this.key = key;
        this.value = value;
    }

    static <K, V> Pair<K, V> of(final K key, final V value) {
        return new Pair<>(key, value);
    }

    K getKey() {
        return key;
    }

    V getValue() {
        return value;
    }

    public String toString() {
        return "(" + key + ',' + value + ')';
    }
}

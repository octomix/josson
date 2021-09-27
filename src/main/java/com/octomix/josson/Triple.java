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

class Triple<L, M, R> {
    private final L left;
    private final M middle;
    private final R right;

    static <L, M, R> Triple<L, M, R> of(L left, M middle, R right) {
        return new Triple<>(left, middle, right);
    }

    Triple(L left, M middle, R right) {
        this.left = left;
        this.middle = middle;
        this.right = right;
    }

    L getLeft() {
        return left;
    }

    M getMiddle() {
        return middle;
    }

    R getRight() {
        return right;
    }

    public String toString() {
        return "(" + left + "," + middle + "," + right + ")";
    }
}

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

class PathTrace {

    private final JsonNode[] steps;

    private PathTrace(final int size) {
        steps = new JsonNode[size];
    }

    private PathTrace(final JsonNode node) {
        steps = new JsonNode[]{node};
    }

    static PathTrace from(final JsonNode node) {
        return new PathTrace(node);
    }

    PathTrace root() {
        return steps.length == 1 ? this : new PathTrace(steps[0]);
    }

    PathTrace push(final JsonNode node) {
        final PathTrace clone = new PathTrace(steps.length + 1);
        System.arraycopy(steps, 0, clone.steps, 0, steps.length);
        clone.steps[steps.length] = node;
        return clone;
    }

    PathTrace pop(final int levels) {
        final PathTrace clone = new PathTrace(steps.length - levels);
        System.arraycopy(steps, 0, clone.steps, 0, steps.length - levels);
        return clone;
    }

    JsonNode node() {
        return steps[steps.length - 1];
    }

    int level() {
        return steps.length - 1;
    }
}

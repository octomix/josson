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

import com.fasterxml.jackson.databind.JsonNode;

import java.util.function.BiFunction;
import java.util.function.Function;

class CustomFunction {

    private final Function<JsonNode, JsonNode> function;
    private final BiFunction<JsonNode, Integer, JsonNode> biFunction;

    CustomFunction(final Function<JsonNode, JsonNode> function, final BiFunction<JsonNode, Integer, JsonNode> biFunction) {
        this.function = function;
        this.biFunction = biFunction;
    }

    static CustomFunction of(final Function<JsonNode, JsonNode> function) {
        return new CustomFunction(function, null);
    }

    static CustomFunction of(final BiFunction<JsonNode, Integer, JsonNode> biFunction) {
        return new CustomFunction(null, biFunction);
    }

    JsonNode apply(final JsonNode node, final int index) {
        if (function != null) {
            return function.apply(node);
        }
        if (biFunction != null) {
            return biFunction.apply(node, index);
        }
        return null;
    }
}

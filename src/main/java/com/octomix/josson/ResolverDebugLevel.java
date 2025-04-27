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
 * Debug levels for resolution progress.
 * {@link #SHOW_CONTENT_OF_VALUE_NODE_ONLY}
 * {@link #SHOW_CONTENT_UP_TO_OBJECT_NODE}
 * {@link #SHOW_CONTENT_UP_TO_ARRAY_NODE}
 */
public enum ResolverDebugLevel {

    /**
     * Level 1: Show content of value node only.
     */
    SHOW_CONTENT_OF_VALUE_NODE_ONLY,

    /**
     * Level 2: Show content up to object node.
     */
    SHOW_CONTENT_UP_TO_OBJECT_NODE,

    /**
     * Level 3: Show content up to array node.
     */
    SHOW_CONTENT_UP_TO_ARRAY_NODE
}

/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.octomix.josson.commons;

import java.io.IOException;
import java.io.Writer;

/**
 * <p>From org.apache.commons:commons-text:1.9</p>
 *
 * Translates code points to their XML numeric entity escaped value.
 *
 * @since 1.0
 */
class NumericEntityEscaper extends CodePointTranslator {

    /**
     * Constructs a {@code NumericEntityEscaper} between the specified values (inclusive).
     *
     * @param codePointLow above which to escape
     * @param codePointHigh below which to escape
     * @return The newly created {@code NumericEntityEscaper} instance
     */
    static NumericEntityEscaper between(final int codePointLow, final int codePointHigh) {
        return new NumericEntityEscaper(codePointLow, codePointHigh, true);
    }

    /** whether to escape between the boundaries or outside them. */
    private final boolean between;

    /** range from lowest code point to highest code point. */
    private final Range<Integer> range;

    /**
     * Constructs a {@code NumericEntityEscaper} for the specified range. This is
     * the underlying method for the other constructors/builders. The {@code below}
     * and {@code above} boundaries are inclusive when {@code between} is
     * {@code true} and exclusive when it is {@code false}.
     *
     * @param below int value representing the lowest code point boundary
     * @param above int value representing the highest code point boundary
     * @param between whether to escape between the boundaries or outside them
     */
    private NumericEntityEscaper(final int below, final int above, final boolean between) {
        this.range = Range.between(below, above);
        this.between = between;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    boolean translate(final int codePoint, final Writer writer) throws IOException {
        if (this.between != this.range.contains(codePoint)) {
            return false;
        }
        writer.write("&#");
        writer.write(Integer.toString(codePoint, 10));
        writer.write(';');
        return true;
    }
}
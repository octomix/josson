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

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * <p>From org.apache.commons:commons-text:1.9</p>
 *
 * <p>
 * Escapes and unescapes {@code String}s for Java, Java Script, HTML and XML.
 * </p>
 *
 * <p>
 * #ThreadSafe#
 * </p>
 *
 * <p>
 * This code has been adapted from Apache Commons Lang 3.5.
 * </p>
 *
 * @since 1.0
 */
public class StringEscapeUtils {

    /**
     * Translator object for escaping XML 1.1.
     *
     * While {@link #escapeXml11(String)} is the expected method of use, this
     * object allows the XML escaping functionality to be used
     * as the foundation for a custom translator.
     */
    static final CharSequenceTranslator ESCAPE_XML11;
    static {
        final Map<CharSequence, CharSequence> escapeXml11Map = new HashMap<>();
        escapeXml11Map.put("\u0000", StringUtils.EMPTY);
        escapeXml11Map.put("\u000b", "&#11;");
        escapeXml11Map.put("\u000c", "&#12;");
        escapeXml11Map.put("\ufffe", StringUtils.EMPTY);
        escapeXml11Map.put("\uffff", StringUtils.EMPTY);
        ESCAPE_XML11 = new AggregateTranslator(
                new LookupTranslator(EntityArrays.BASIC_ESCAPE),
                new LookupTranslator(EntityArrays.APOS_ESCAPE),
                new LookupTranslator(Collections.unmodifiableMap(escapeXml11Map)),
                NumericEntityEscaper.between(0x1, 0x8),
                NumericEntityEscaper.between(0xe, 0x1f),
                NumericEntityEscaper.between(0x7f, 0x84),
                NumericEntityEscaper.between(0x86, 0x9f),
                new UnicodeUnpairedSurrogateRemover()
        );
    }

    /**
     * Translator object for escaping HTML version 4.0.
     *
     * While {@link #escapeHtml4(String)} is the expected method of use, this
     * object allows the HTML escaping functionality to be used
     * as the foundation for a custom translator.
     */
    static final CharSequenceTranslator ESCAPE_HTML4 =
            new AggregateTranslator(
                    new LookupTranslator(EntityArrays.BASIC_ESCAPE),
                    new LookupTranslator(EntityArrays.ISO8859_1_ESCAPE),
                    new LookupTranslator(EntityArrays.HTML40_EXTENDED_ESCAPE)
            );

    /**
     * Translator object for unescaping escaped HTML 4.0.
     *
     * While {@link #unescapeHtml4(String)} is the expected method of use, this
     * object allows the HTML unescaping functionality to be used
     * as the foundation for a custom translator.
     */
    static final CharSequenceTranslator UNESCAPE_HTML4 =
            new AggregateTranslator(
                    new LookupTranslator(EntityArrays.BASIC_UNESCAPE),
                    new LookupTranslator(EntityArrays.ISO8859_1_UNESCAPE),
                    new LookupTranslator(EntityArrays.HTML40_EXTENDED_UNESCAPE),
                    new NumericEntityUnescaper()
            );

    /**
     * Translator object for unescaping escaped XML.
     *
     * While {@link #unescapeXml(String)} is the expected method of use, this
     * object allows the XML unescaping functionality to be used
     * as the foundation for a custom translator.
     */
    static final CharSequenceTranslator UNESCAPE_XML =
            new AggregateTranslator(
                    new LookupTranslator(EntityArrays.BASIC_UNESCAPE),
                    new LookupTranslator(EntityArrays.APOS_UNESCAPE),
                    new NumericEntityUnescaper()
            );

    // HTML and XML
    /**
     * Escapes the characters in a {@code String} using HTML entities.
     *
     * <p>
     * For example:
     * </p>
     * <p>{@code "bread" &amp; "butter"}</p>
     * becomes:
     * <p>
     * {@code &amp;quot;bread&amp;quot; &amp;amp; &amp;quot;butter&amp;quot;}.
     * </p>
     *
     * <p>Supports all known HTML 4.0 entities, including funky accents.
     * Note that the commonly used apostrophe escape character (&amp;apos;)
     * is not a legal entity and so is not supported).</p>
     *
     * @param input  the {@code String} to escape, may be null
     * @return a new escaped {@code String}, {@code null} if null string input
     *
     * @see <a href="http://hotwired.lycos.com/webmonkey/reference/special_characters/">ISO Entities</a>
     * @see <a href="http://www.w3.org/TR/REC-html32#latin1">HTML 3.2 Character Entities for ISO Latin-1</a>
     * @see <a href="http://www.w3.org/TR/REC-html40/sgml/entities.html">HTML 4.0 Character entity references</a>
     * @see <a href="http://www.w3.org/TR/html401/charset.html#h-5.3">HTML 4.01 Character References</a>
     * @see <a href="http://www.w3.org/TR/html401/charset.html#code-position">HTML 4.01 Code positions</a>
     */
    public static String escapeHtml4(final String input) {
        return ESCAPE_HTML4.translate(input);
    }

    /**
     * Escapes the characters in a {@code String} using XML entities.
     *
     * <p>For example: {@code "bread" & "butter"} =&gt;
     * {@code &quot;bread&quot; &amp; &quot;butter&quot;}.
     * </p>
     *
     * <p>XML 1.1 can represent certain control characters, but it cannot represent
     * the null byte or unpaired Unicode surrogate code points, even after escaping.
     * {@code escapeXml11} will remove characters that do not fit in the following
     * ranges:</p>
     *
     * <p>{@code [#x1-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]}</p>
     *
     * <p>{@code escapeXml11} will escape characters in the following ranges:</p>
     *
     * <p>{@code [#x1-#x8] | [#xB-#xC] | [#xE-#x1F] | [#x7F-#x84] | [#x86-#x9F]}</p>
     *
     * <p>The returned string can be inserted into a valid XML 1.1 document. Do not
     * use it for XML 1.0 documents.</p>
     *
     * @param input  the {@code String} to escape, may be null
     * @return a new escaped {@code String}, {@code null} if null string input
     * @see #unescapeXml(java.lang.String)
     */
    public static String escapeXml11(final String input) {
        return ESCAPE_XML11.translate(input);
    }

    /**
     * Unescapes a string containing entity escapes to a string
     * containing the actual Unicode characters corresponding to the
     * escapes. Supports HTML 4.0 entities.
     *
     * <p>For example, the string {@code "&lt;Fran&ccedil;ais&gt;"}
     * will become {@code "<Fran�ais>"}</p>
     *
     * <p>If an entity is unrecognized, it is left alone, and inserted
     * verbatim into the result string. e.g. {@code "&gt;&zzzz;x"} will
     * become {@code ">&zzzz;x"}.</p>
     *
     * @param input  the {@code String} to unescape, may be null
     * @return a new unescaped {@code String}, {@code null} if null string input
     */
    public static String unescapeHtml4(final String input) {
        return UNESCAPE_HTML4.translate(input);
    }

    /**
     * Unescapes a string containing XML entity escapes to a string
     * containing the actual Unicode characters corresponding to the
     * escapes.
     *
     * <p>Supports only the five basic XML entities (gt, lt, quot, amp, apos).
     * Does not support DTDs or external entities.</p>
     *
     * <p>Note that numerical \\u Unicode codes are unescaped to their respective
     *    Unicode characters. This may change in future releases.</p>
     *
     * @param input  the {@code String} to unescape, may be null
     * @return a new unescaped {@code String}, {@code null} if null string input
     * @see #escapeXml11(String)
     */
    public static String unescapeXml(final String input) {
        return UNESCAPE_XML.translate(input);
    }

}

package com.octomix.josson.commons;

/**
 * org.apache.commons:commons-text:1.9
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
     * Translator object for unescaping escaped XML.
     *
     * While {@link #unescapeXml(String)} is the expected method of use, this
     * object allows the XML unescaping functionality to be used
     * as the foundation for a custom translator.
     */
    public static final CharSequenceTranslator UNESCAPE_XML =
            new AggregateTranslator(
                    new LookupTranslator(EntityArrays.BASIC_UNESCAPE),
                    new LookupTranslator(EntityArrays.APOS_UNESCAPE),
                    new NumericEntityUnescaper()
            );

    //-----------------------------------------------------------------------
    /**
     * <p>Unescapes a string containing XML entity escapes to a string
     * containing the actual Unicode characters corresponding to the
     * escapes.</p>
     *
     * <p>Supports only the five basic XML entities (gt, lt, quot, amp, apos).
     * Does not support DTDs or external entities.</p>
     *
     * <p>Note that numerical \\u Unicode codes are unescaped to their respective
     *    Unicode characters. This may change in future releases.</p>
     *
     * @param input  the {@code String} to unescape, may be null
     * @return a new unescaped {@code String}, {@code null} if null string input
     */
    public static String unescapeXml(final String input) {
        return UNESCAPE_XML.translate(input);
    }

}

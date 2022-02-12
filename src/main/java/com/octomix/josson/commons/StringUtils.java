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

import java.util.*;

/**
 * <p>From org.apache.commons.lang3:3.12.0</p>
 *
 * <p>Operations on {@link java.lang.String} that are
 * {@code null} safe.</p>
 *
 * <p>The {@code StringUtils} class defines certain words related to
 * String handling.</p>
 *
 * <p>{@code StringUtils} handles {@code null} input Strings quietly.
 * That is to say that a {@code null} input will return {@code null}.
 * Where a {@code boolean} or {@code int} is being returned
 * details vary by method.</p>
 *
 * <p>A side effect of the {@code null} handling is that a
 * {@code NullPointerException} should be considered a bug in
 * {@code StringUtils}.</p>
 *
 * <p>Methods in this class include sample code in their Javadoc comments to explain their operation.
 * The symbol {@code *} is used to indicate any input including {@code null}.</p>
 *
 * <p>#ThreadSafe#</p>
 * @see java.lang.String
 * @since 1.0
 */
public class StringUtils {

    /**
     * A String for a space character.
     *
     * @since 3.2
     */
    public static final String SPACE = " ";

    /**
     * The empty String {@code ""}.
     * @since 2.0
     */
    public static final String EMPTY = "";

    /**
     * Represents a failed index search.
     * @since 2.1
     */
    public static final int INDEX_NOT_FOUND = -1;

    /**
     * <p>The maximum size to which the padding constant(s) can expand.</p>
     */
    private static final int PAD_LIMIT = 8192;

    /**
     * <p>Abbreviates a String using ellipses. This will turn
     * "Now is the time for all good men" into "...is the time for..."</p>
     *
     * <p>Works like {@code abbreviate(String, int)}, but allows you to specify
     * a "left edge" offset.  Note that this left edge is not necessarily going to
     * be the leftmost character in the result, or the first character following the
     * ellipses, but it will appear somewhere in the result.
     *
     * <p>In no case will it return a String of length greater than
     * {@code maxWidth}.</p>
     *
     * <pre>
     * StringUtils.abbreviate(null, *, *)                = null
     * StringUtils.abbreviate("", 0, 4)                  = ""
     * StringUtils.abbreviate("abcdefghijklmno", -1, 10) = "abcdefg..."
     * StringUtils.abbreviate("abcdefghijklmno", 0, 10)  = "abcdefg..."
     * StringUtils.abbreviate("abcdefghijklmno", 1, 10)  = "abcdefg..."
     * StringUtils.abbreviate("abcdefghijklmno", 4, 10)  = "abcdefg..."
     * StringUtils.abbreviate("abcdefghijklmno", 5, 10)  = "...fghi..."
     * StringUtils.abbreviate("abcdefghijklmno", 6, 10)  = "...ghij..."
     * StringUtils.abbreviate("abcdefghijklmno", 8, 10)  = "...ijklmno"
     * StringUtils.abbreviate("abcdefghijklmno", 10, 10) = "...ijklmno"
     * StringUtils.abbreviate("abcdefghijklmno", 12, 10) = "...ijklmno"
     * StringUtils.abbreviate("abcdefghij", 0, 3)        = IllegalArgumentException
     * StringUtils.abbreviate("abcdefghij", 5, 6)        = IllegalArgumentException
     * </pre>
     *
     * @param str  the String to check, may be null
     * @param offset  left edge of source String
     * @param maxWidth  maximum length of result String, must be at least 4
     * @return abbreviated String, {@code null} if null String input
     * @throws IllegalArgumentException if the width is too small
     * @since 2.0
     */
    public static String abbreviate(final String str, final int offset, final int maxWidth) {
        return abbreviate(str, "...", offset, maxWidth);
    }

    /**
     * <p>Abbreviates a String using another given String as replacement marker. This will turn
     * "Now is the time for all good men" into "Now is the time for..." if "..." was defined
     * as the replacement marker.</p>
     *
     * <p>Specifically:</p>
     * <ul>
     *   <li>If the number of characters in {@code str} is less than or equal to
     *       {@code maxWidth}, return {@code str}.</li>
     *   <li>Else abbreviate it to {@code (substring(str, 0, max-abbrevMarker.length) + abbrevMarker)}.</li>
     *   <li>If {@code maxWidth} is less than {@code abbrevMarker.length + 1}, throw an
     *       {@code IllegalArgumentException}.</li>
     *   <li>In no case will it return a String of length greater than
     *       {@code maxWidth}.</li>
     * </ul>
     *
     * <pre>
     * StringUtils.abbreviate(null, "...", *)      = null
     * StringUtils.abbreviate("abcdefg", null, *)  = "abcdefg"
     * StringUtils.abbreviate("", "...", 4)        = ""
     * StringUtils.abbreviate("abcdefg", ".", 5)   = "abcd."
     * StringUtils.abbreviate("abcdefg", ".", 7)   = "abcdefg"
     * StringUtils.abbreviate("abcdefg", ".", 8)   = "abcdefg"
     * StringUtils.abbreviate("abcdefg", "..", 4)  = "ab.."
     * StringUtils.abbreviate("abcdefg", "..", 3)  = "a.."
     * StringUtils.abbreviate("abcdefg", "..", 2)  = IllegalArgumentException
     * StringUtils.abbreviate("abcdefg", "...", 3) = IllegalArgumentException
     * </pre>
     *
     * @param str  the String to check, may be null
     * @param abbrevMarker  the String used as replacement marker
     * @param maxWidth  maximum length of result String, must be at least {@code abbrevMarker.length + 1}
     * @return abbreviated String, {@code null} if null String input
     * @throws IllegalArgumentException if the width is too small
     * @since 3.6
     */
    public static String abbreviate(final String str, final String abbrevMarker, final int maxWidth) {
        return abbreviate(str, abbrevMarker, 0, maxWidth);
    }

    /**
     * <p>Abbreviates a String using a given replacement marker. This will turn
     * "Now is the time for all good men" into "...is the time for..." if "..." was defined
     * as the replacement marker.</p>
     *
     * <p>Works like {@code abbreviate(String, String, int)}, but allows you to specify
     * a "left edge" offset.  Note that this left edge is not necessarily going to
     * be the leftmost character in the result, or the first character following the
     * replacement marker, but it will appear somewhere in the result.
     *
     * <p>In no case will it return a String of length greater than {@code maxWidth}.</p>
     *
     * <pre>
     * StringUtils.abbreviate(null, null, *, *)                 = null
     * StringUtils.abbreviate("abcdefghijklmno", null, *, *)    = "abcdefghijklmno"
     * StringUtils.abbreviate("", "...", 0, 4)                  = ""
     * StringUtils.abbreviate("abcdefghijklmno", "---", -1, 10) = "abcdefg---"
     * StringUtils.abbreviate("abcdefghijklmno", ",", 0, 10)    = "abcdefghi,"
     * StringUtils.abbreviate("abcdefghijklmno", ",", 1, 10)    = "abcdefghi,"
     * StringUtils.abbreviate("abcdefghijklmno", ",", 2, 10)    = "abcdefghi,"
     * StringUtils.abbreviate("abcdefghijklmno", "::", 4, 10)   = "::efghij::"
     * StringUtils.abbreviate("abcdefghijklmno", "...", 6, 10)  = "...ghij..."
     * StringUtils.abbreviate("abcdefghijklmno", "*", 9, 10)    = "*ghijklmno"
     * StringUtils.abbreviate("abcdefghijklmno", "'", 10, 10)   = "'ghijklmno"
     * StringUtils.abbreviate("abcdefghijklmno", "!", 12, 10)   = "!ghijklmno"
     * StringUtils.abbreviate("abcdefghij", "abra", 0, 4)       = IllegalArgumentException
     * StringUtils.abbreviate("abcdefghij", "...", 5, 6)        = IllegalArgumentException
     * </pre>
     *
     * @param str  the String to check, may be null
     * @param abbrevMarker  the String used as replacement marker
     * @param offset  left edge of source String
     * @param maxWidth  maximum length of result String, must be at least 4
     * @return abbreviated String, {@code null} if null String input
     * @throws IllegalArgumentException if the width is too small
     * @since 3.6
     */
    public static String abbreviate(final String str, final String abbrevMarker, int offset, final int maxWidth) {
        if (isNotEmpty(str) && EMPTY.equals(abbrevMarker) && maxWidth > 0) {
            return substring(str, 0, maxWidth);
        } else if (isAnyEmpty(str, abbrevMarker)) {
            return str;
        }
        final int abbrevMarkerLength = abbrevMarker.length();
        final int minAbbrevWidth = abbrevMarkerLength + 1;
        final int minAbbrevWidthOffset = abbrevMarkerLength + abbrevMarkerLength + 1;

        if (maxWidth < minAbbrevWidth) {
            throw new IllegalArgumentException(String.format("Minimum abbreviation width is %d", minAbbrevWidth));
        }
        final int strLen = str.length();
        if (strLen <= maxWidth) {
            return str;
        }
        if (offset > strLen) {
            offset = strLen;
        }
        if (strLen - offset < maxWidth - abbrevMarkerLength) {
            offset = strLen - (maxWidth - abbrevMarkerLength);
        }
        if (offset <= abbrevMarkerLength+1) {
            return str.substring(0, maxWidth - abbrevMarkerLength) + abbrevMarker;
        }
        if (maxWidth < minAbbrevWidthOffset) {
            throw new IllegalArgumentException(String.format("Minimum abbreviation width with offset is %d", minAbbrevWidthOffset));
        }
        if (offset + maxWidth - abbrevMarkerLength < strLen) {
            return abbrevMarker + abbreviate(str.substring(offset), abbrevMarker, maxWidth - abbrevMarkerLength);
        }
        return abbrevMarker + str.substring(strLen - (maxWidth - abbrevMarkerLength));
    }

    /**
     * Appends the suffix to the end of the string if the string does not
     * already end with the suffix.
     *
     * @param str The string.
     * @param suffix The suffix to append to the end of the string.
     * @param ignoreCase Indicates whether the compare should ignore case.
     * @param suffixes Additional suffixes that are valid terminators (optional).
     *
     * @return A new String if suffix was appended, the same string otherwise.
     */
    private static String appendIfMissing(final String str, final CharSequence suffix, final boolean ignoreCase, final CharSequence... suffixes) {
        if (str == null || isEmpty(suffix) || endsWith(str, suffix, ignoreCase)) {
            return str;
        }
        if (ArrayUtils.isNotEmpty(suffixes)) {
            for (final CharSequence s : suffixes) {
                if (endsWith(str, s, ignoreCase)) {
                    return str;
                }
            }
        }
        return str + suffix;
    }

    /**
     * Appends the suffix to the end of the string if the string does not
     * already end with any of the suffixes.
     *
     * <pre>
     * StringUtils.appendIfMissing(null, null) = null
     * StringUtils.appendIfMissing("abc", null) = "abc"
     * StringUtils.appendIfMissing("", "xyz") = "xyz"
     * StringUtils.appendIfMissing("abc", "xyz") = "abcxyz"
     * StringUtils.appendIfMissing("abcxyz", "xyz") = "abcxyz"
     * StringUtils.appendIfMissing("abcXYZ", "xyz") = "abcXYZxyz"
     * </pre>
     * <p>With additional suffixes,</p>
     * <pre>
     * StringUtils.appendIfMissing(null, null, null) = null
     * StringUtils.appendIfMissing("abc", null, null) = "abc"
     * StringUtils.appendIfMissing("", "xyz", null) = "xyz"
     * StringUtils.appendIfMissing("abc", "xyz", new CharSequence[]{null}) = "abcxyz"
     * StringUtils.appendIfMissing("abc", "xyz", "") = "abc"
     * StringUtils.appendIfMissing("abc", "xyz", "mno") = "abcxyz"
     * StringUtils.appendIfMissing("abcxyz", "xyz", "mno") = "abcxyz"
     * StringUtils.appendIfMissing("abcmno", "xyz", "mno") = "abcmno"
     * StringUtils.appendIfMissing("abcXYZ", "xyz", "mno") = "abcXYZxyz"
     * StringUtils.appendIfMissing("abcMNO", "xyz", "mno") = "abcMNOxyz"
     * </pre>
     *
     * @param str The string.
     * @param suffix The suffix to append to the end of the string.
     * @param suffixes Additional suffixes that are valid terminators.
     *
     * @return A new String if suffix was appended, the same string otherwise.
     *
     * @since 3.2
     */
    public static String appendIfMissing(final String str, final CharSequence suffix, final CharSequence... suffixes) {
        return appendIfMissing(str, suffix, false, suffixes);
    }

    /**
     * Appends the suffix to the end of the string if the string does not
     * already end, case insensitive, with any of the suffixes.
     *
     * <pre>
     * StringUtils.appendIfMissingIgnoreCase(null, null) = null
     * StringUtils.appendIfMissingIgnoreCase("abc", null) = "abc"
     * StringUtils.appendIfMissingIgnoreCase("", "xyz") = "xyz"
     * StringUtils.appendIfMissingIgnoreCase("abc", "xyz") = "abcxyz"
     * StringUtils.appendIfMissingIgnoreCase("abcxyz", "xyz") = "abcxyz"
     * StringUtils.appendIfMissingIgnoreCase("abcXYZ", "xyz") = "abcXYZ"
     * </pre>
     * <p>With additional suffixes,</p>
     * <pre>
     * StringUtils.appendIfMissingIgnoreCase(null, null, null) = null
     * StringUtils.appendIfMissingIgnoreCase("abc", null, null) = "abc"
     * StringUtils.appendIfMissingIgnoreCase("", "xyz", null) = "xyz"
     * StringUtils.appendIfMissingIgnoreCase("abc", "xyz", new CharSequence[]{null}) = "abcxyz"
     * StringUtils.appendIfMissingIgnoreCase("abc", "xyz", "") = "abc"
     * StringUtils.appendIfMissingIgnoreCase("abc", "xyz", "mno") = "abcxyz"
     * StringUtils.appendIfMissingIgnoreCase("abcxyz", "xyz", "mno") = "abcxyz"
     * StringUtils.appendIfMissingIgnoreCase("abcmno", "xyz", "mno") = "abcmno"
     * StringUtils.appendIfMissingIgnoreCase("abcXYZ", "xyz", "mno") = "abcXYZ"
     * StringUtils.appendIfMissingIgnoreCase("abcMNO", "xyz", "mno") = "abcMNO"
     * </pre>
     *
     * @param str The string.
     * @param suffix The suffix to append to the end of the string.
     * @param suffixes Additional suffixes that are valid terminators.
     *
     * @return A new String if suffix was appended, the same string otherwise.
     *
     * @since 3.2
     */
    public static String appendIfMissingIgnoreCase(final String str, final CharSequence suffix, final CharSequence... suffixes) {
        return appendIfMissing(str, suffix, true, suffixes);
    }

    /**
     * <p>Capitalizes a String changing the first character to title case as
     * per {@link Character#toTitleCase(int)}. No other characters are changed.</p>
     *
     * <pre>
     * StringUtils.capitalize(null)  = null
     * StringUtils.capitalize("")    = ""
     * StringUtils.capitalize("cat") = "Cat"
     * StringUtils.capitalize("cAt") = "CAt"
     * StringUtils.capitalize("'cat'") = "'cat'"
     * </pre>
     *
     * @param str the String to capitalize, may be null
     * @return the capitalized String, {@code null} if null String input
     * @see #uncapitalize(String)
     * @since 2.0
     */
    public static String capitalize(final String str) {
        final int strLen = length(str);
        if (strLen == 0) {
            return str;
        }

        final int firstCodepoint = str.codePointAt(0);
        final int newCodePoint = Character.toTitleCase(firstCodepoint);
        if (firstCodepoint == newCodePoint) {
            // already capitalized
            return str;
        }

        final int[] newCodePoints = new int[strLen]; // cannot be longer than the char array
        int outOffset = 0;
        newCodePoints[outOffset++] = newCodePoint; // copy the first codepoint
        for (int inOffset = Character.charCount(firstCodepoint); inOffset < strLen; ) {
            final int codepoint = str.codePointAt(inOffset);
            newCodePoints[outOffset++] = codepoint; // copy the remaining ones
            inOffset += Character.charCount(codepoint);
        }
        return new String(newCodePoints, 0, outOffset);
    }

    /**
     * <p>Centers a String in a larger String of size {@code size}.
     * Uses a supplied String as the value to pad the String with.</p>
     *
     * <p>If the size is less than the String length, the String is returned.
     * A {@code null} String returns {@code null}.
     * A negative size is treated as zero.</p>
     *
     * <pre>
     * StringUtils.center(null, *, *)     = null
     * StringUtils.center("", 4, " ")     = "    "
     * StringUtils.center("ab", -1, " ")  = "ab"
     * StringUtils.center("ab", 4, " ")   = " ab "
     * StringUtils.center("abcd", 2, " ") = "abcd"
     * StringUtils.center("a", 4, " ")    = " a  "
     * StringUtils.center("a", 4, "yz")   = "yayz"
     * StringUtils.center("abc", 7, null) = "  abc  "
     * StringUtils.center("abc", 7, "")   = "  abc  "
     * </pre>
     *
     * @param str  the String to center, may be null
     * @param size  the int size of new String, negative treated as zero
     * @param padStr  the String to pad the new String with, must not be null or empty
     * @return centered String, {@code null} if null String input
     * @throws IllegalArgumentException if padStr is {@code null} or empty
     */
    public static String center(String str, final int size, String padStr) {
        if (str == null || size <= 0) {
            return str;
        }
        if (isEmpty(padStr)) {
            padStr = SPACE;
        }
        final int strLen = str.length();
        final int pads = size - strLen;
        if (pads <= 0) {
            return str;
        }
        str = leftPad(str, strLen + pads / 2, padStr);
        str = rightPad(str, size, padStr);
        return str;
    }

    /**
     * <p>Checks if CharSequence contains a search CharSequence, handling {@code null}.
     * This method uses {@link String#indexOf(String)} if possible.</p>
     *
     * <p>A {@code null} CharSequence will return {@code false}.</p>
     *
     * <pre>
     * StringUtils.contains(null, *)     = false
     * StringUtils.contains(*, null)     = false
     * StringUtils.contains("", "")      = true
     * StringUtils.contains("abc", "")   = true
     * StringUtils.contains("abc", "a")  = true
     * StringUtils.contains("abc", "z")  = false
     * </pre>
     *
     * @param seq  the CharSequence to check, may be null
     * @param searchSeq  the CharSequence to find, may be null
     * @return true if the CharSequence contains the search CharSequence,
     *  false if not or {@code null} string input
     * @since 2.0
     * @since 3.0 Changed signature from contains(String, String) to contains(CharSequence, CharSequence)
     */
    public static boolean contains(final CharSequence seq, final CharSequence searchSeq) {
        if (seq == null || searchSeq == null) {
            return false;
        }
        return CharSequenceUtils.indexOf(seq, searchSeq, 0) >= 0;
    }

    /**
     * <p>Checks if CharSequence contains a search CharSequence irrespective of case,
     * handling {@code null}. Case-insensitivity is defined as by
     * {@link String#equalsIgnoreCase(String)}.
     *
     * <p>A {@code null} CharSequence will return {@code false}.</p>
     *
     * <pre>
     * StringUtils.containsIgnoreCase(null, *) = false
     * StringUtils.containsIgnoreCase(*, null) = false
     * StringUtils.containsIgnoreCase("", "") = true
     * StringUtils.containsIgnoreCase("abc", "") = true
     * StringUtils.containsIgnoreCase("abc", "a") = true
     * StringUtils.containsIgnoreCase("abc", "z") = false
     * StringUtils.containsIgnoreCase("abc", "A") = true
     * StringUtils.containsIgnoreCase("abc", "Z") = false
     * </pre>
     *
     * @param str  the CharSequence to check, may be null
     * @param searchStr  the CharSequence to find, may be null
     * @return true if the CharSequence contains the search CharSequence irrespective of
     * case or false if not or {@code null} string input
     * @since 3.0 Changed signature from containsIgnoreCase(String, String) to containsIgnoreCase(CharSequence, CharSequence)
     */
    public static boolean containsIgnoreCase(final CharSequence str, final CharSequence searchStr) {
        if (str == null || searchStr == null) {
            return false;
        }
        final int len = searchStr.length();
        final int max = str.length() - len;
        for (int i = 0; i <= max; i++) {
            if (CharSequenceUtils.regionMatches(str, true, i, searchStr, 0, len)) {
                return true;
            }
        }
        return false;
    }

    /**
     * <p>Check if a CharSequence ends with a specified suffix.</p>
     *
     * <p>{@code null}s are handled without exceptions. Two {@code null}
     * references are considered to be equal. The comparison is case sensitive.</p>
     *
     * <pre>
     * StringUtils.endsWith(null, null)      = true
     * StringUtils.endsWith(null, "def")     = false
     * StringUtils.endsWith("abcdef", null)  = false
     * StringUtils.endsWith("abcdef", "def") = true
     * StringUtils.endsWith("ABCDEF", "def") = false
     * StringUtils.endsWith("ABCDEF", "cde") = false
     * StringUtils.endsWith("ABCDEF", "")    = true
     * </pre>
     *
     * @see java.lang.String#endsWith(String)
     * @param str  the CharSequence to check, may be null
     * @param suffix the suffix to find, may be null
     * @return {@code true} if the CharSequence ends with the suffix, case sensitive, or
     *  both {@code null}
     * @since 2.4
     * @since 3.0 Changed signature from endsWith(String, String) to endsWith(CharSequence, CharSequence)
     */
    public static boolean endsWith(final CharSequence str, final CharSequence suffix) {
        return endsWith(str, suffix, false);
    }

    /**
     * <p>Check if a CharSequence ends with a specified suffix (optionally case insensitive).</p>
     *
     * @see java.lang.String#endsWith(String)
     * @param str  the CharSequence to check, may be null
     * @param suffix the suffix to find, may be null
     * @param ignoreCase indicates whether the compare should ignore case
     *  (case insensitive) or not.
     * @return {@code true} if the CharSequence starts with the prefix or
     *  both {@code null}
     */
    private static boolean endsWith(final CharSequence str, final CharSequence suffix, final boolean ignoreCase) {
        if (str == null || suffix == null) {
            return str == suffix;
        }
        if (suffix.length() > str.length()) {
            return false;
        }
        final int strOffset = str.length() - suffix.length();
        return CharSequenceUtils.regionMatches(str, ignoreCase, strOffset, suffix, 0, suffix.length());
    }

    /**
     * <p>Case insensitive check if a CharSequence ends with a specified suffix.</p>
     *
     * <p>{@code null}s are handled without exceptions. Two {@code null}
     * references are considered to be equal. The comparison is case insensitive.</p>
     *
     * <pre>
     * StringUtils.endsWithIgnoreCase(null, null)      = true
     * StringUtils.endsWithIgnoreCase(null, "def")     = false
     * StringUtils.endsWithIgnoreCase("abcdef", null)  = false
     * StringUtils.endsWithIgnoreCase("abcdef", "def") = true
     * StringUtils.endsWithIgnoreCase("ABCDEF", "def") = true
     * StringUtils.endsWithIgnoreCase("ABCDEF", "cde") = false
     * </pre>
     *
     * @see java.lang.String#endsWith(String)
     * @param str  the CharSequence to check, may be null
     * @param suffix the suffix to find, may be null
     * @return {@code true} if the CharSequence ends with the suffix, case insensitive, or
     *  both {@code null}
     * @since 2.4
     * @since 3.0 Changed signature from endsWithIgnoreCase(String, String) to endsWithIgnoreCase(CharSequence, CharSequence)
     */
    public static boolean endsWithIgnoreCase(final CharSequence str, final CharSequence suffix) {
        return endsWith(str, suffix, true);
    }

    /**
     * <p>Compares two CharSequences, returning {@code true} if they represent
     * equal sequences of characters.</p>
     *
     * <p>{@code null}s are handled without exceptions. Two {@code null}
     * references are considered to be equal. The comparison is <strong>case sensitive</strong>.</p>
     *
     * <pre>
     * StringUtils.equals(null, null)   = true
     * StringUtils.equals(null, "abc")  = false
     * StringUtils.equals("abc", null)  = false
     * StringUtils.equals("abc", "abc") = true
     * StringUtils.equals("abc", "ABC") = false
     * </pre>
     *
     * @param cs1  the first CharSequence, may be {@code null}
     * @param cs2  the second CharSequence, may be {@code null}
     * @return {@code true} if the CharSequences are equal (case-sensitive), or both {@code null}
     * @since 3.0 Changed signature from equals(String, String) to equals(CharSequence, CharSequence)
     * @see Object#equals(Object)
     * @see #equalsIgnoreCase(CharSequence, CharSequence)
     */
    public static boolean equals(final CharSequence cs1, final CharSequence cs2) {
        if (cs1 == cs2) {
            return true;
        }
        if (cs1 == null || cs2 == null) {
            return false;
        }
        if (cs1.length() != cs2.length()) {
            return false;
        }
        if (cs1 instanceof String && cs2 instanceof String) {
            return cs1.equals(cs2);
        }
        // Step-wise comparison
        final int length = cs1.length();
        for (int i = 0; i < length; i++) {
            if (cs1.charAt(i) != cs2.charAt(i)) {
                return false;
            }
        }
        return true;
    }

    /**
     * <p>Compares two CharSequences, returning {@code true} if they represent
     * equal sequences of characters, ignoring case.</p>
     *
     * <p>{@code null}s are handled without exceptions. Two {@code null}
     * references are considered equal. The comparison is <strong>case insensitive</strong>.</p>
     *
     * <pre>
     * StringUtils.equalsIgnoreCase(null, null)   = true
     * StringUtils.equalsIgnoreCase(null, "abc")  = false
     * StringUtils.equalsIgnoreCase("abc", null)  = false
     * StringUtils.equalsIgnoreCase("abc", "abc") = true
     * StringUtils.equalsIgnoreCase("abc", "ABC") = true
     * </pre>
     *
     * @param cs1  the first CharSequence, may be {@code null}
     * @param cs2  the second CharSequence, may be {@code null}
     * @return {@code true} if the CharSequences are equal (case-insensitive), or both {@code null}
     * @since 3.0 Changed signature from equalsIgnoreCase(String, String) to equalsIgnoreCase(CharSequence, CharSequence)
     * @see #equals(CharSequence, CharSequence)
     */
    public static boolean equalsIgnoreCase(final CharSequence cs1, final CharSequence cs2) {
        if (cs1 == cs2) {
            return true;
        }
        if (cs1 == null || cs2 == null) {
            return false;
        }
        if (cs1.length() != cs2.length()) {
            return false;
        }
        return CharSequenceUtils.regionMatches(cs1, true, 0, cs2, 0, cs1.length());
    }

    /**
     * <p>Finds the first index within a CharSequence, handling {@code null}.
     * This method uses {@link String#indexOf(String, int)} if possible.</p>
     *
     * <p>A {@code null} CharSequence will return {@code -1}.</p>
     *
     * <pre>
     * StringUtils.indexOf(null, *)          = -1
     * StringUtils.indexOf(*, null)          = -1
     * StringUtils.indexOf("", "")           = 0
     * StringUtils.indexOf("", *)            = -1 (except when * = "")
     * StringUtils.indexOf("aabaabaa", "a")  = 0
     * StringUtils.indexOf("aabaabaa", "b")  = 2
     * StringUtils.indexOf("aabaabaa", "ab") = 1
     * StringUtils.indexOf("aabaabaa", "")   = 0
     * </pre>
     *
     * @param seq  the CharSequence to check, may be null
     * @param searchSeq  the CharSequence to find, may be null
     * @return the first index of the search CharSequence,
     *  -1 if no match or {@code null} string input
     * @since 2.0
     * @since 3.0 Changed signature from indexOf(String, String) to indexOf(CharSequence, CharSequence)
     */
    public static int indexOf(final CharSequence seq, final CharSequence searchSeq) {
        if (seq == null || searchSeq == null) {
            return INDEX_NOT_FOUND;
        }
        return CharSequenceUtils.indexOf(seq, searchSeq, 0);
    }

    /**
     * <p>Finds the first index within a CharSequence, handling {@code null}.
     * This method uses {@link String#indexOf(String, int)} if possible.</p>
     *
     * <p>A {@code null} CharSequence will return {@code -1}.
     * A negative start position is treated as zero.
     * An empty ("") search CharSequence always matches.
     * A start position greater than the string length only matches
     * an empty search CharSequence.</p>
     *
     * <pre>
     * StringUtils.indexOf(null, *, *)          = -1
     * StringUtils.indexOf(*, null, *)          = -1
     * StringUtils.indexOf("", "", 0)           = 0
     * StringUtils.indexOf("", *, 0)            = -1 (except when * = "")
     * StringUtils.indexOf("aabaabaa", "a", 0)  = 0
     * StringUtils.indexOf("aabaabaa", "b", 0)  = 2
     * StringUtils.indexOf("aabaabaa", "ab", 0) = 1
     * StringUtils.indexOf("aabaabaa", "b", 3)  = 5
     * StringUtils.indexOf("aabaabaa", "b", 9)  = -1
     * StringUtils.indexOf("aabaabaa", "b", -1) = 2
     * StringUtils.indexOf("aabaabaa", "", 2)   = 2
     * StringUtils.indexOf("abc", "", 9)        = 3
     * </pre>
     *
     * @param seq  the CharSequence to check, may be null
     * @param searchSeq  the CharSequence to find, may be null
     * @param startPos  the start position, negative treated as zero
     * @return the first index of the search CharSequence (always &ge; startPos),
     *  -1 if no match or {@code null} string input
     * @since 2.0
     * @since 3.0 Changed signature from indexOf(String, String, int) to indexOf(CharSequence, CharSequence, int)
     */
    public static int indexOf(final CharSequence seq, final CharSequence searchSeq, final int startPos) {
        if (seq == null || searchSeq == null) {
            return INDEX_NOT_FOUND;
        }
        return CharSequenceUtils.indexOf(seq, searchSeq, startPos);
    }

    /**
     * <p>Case in-sensitive find of the first index within a CharSequence.</p>
     *
     * <p>A {@code null} CharSequence will return {@code -1}.
     * A negative start position is treated as zero.
     * An empty ("") search CharSequence always matches.
     * A start position greater than the string length only matches
     * an empty search CharSequence.</p>
     *
     * <pre>
     * StringUtils.indexOfIgnoreCase(null, *)          = -1
     * StringUtils.indexOfIgnoreCase(*, null)          = -1
     * StringUtils.indexOfIgnoreCase("", "")           = 0
     * StringUtils.indexOfIgnoreCase("aabaabaa", "a")  = 0
     * StringUtils.indexOfIgnoreCase("aabaabaa", "b")  = 2
     * StringUtils.indexOfIgnoreCase("aabaabaa", "ab") = 1
     * </pre>
     *
     * @param str  the CharSequence to check, may be null
     * @param searchStr  the CharSequence to find, may be null
     * @return the first index of the search CharSequence,
     *  -1 if no match or {@code null} string input
     * @since 2.5
     * @since 3.0 Changed signature from indexOfIgnoreCase(String, String) to indexOfIgnoreCase(CharSequence, CharSequence)
     */
    public static int indexOfIgnoreCase(final CharSequence str, final CharSequence searchStr) {
        return indexOfIgnoreCase(str, searchStr, 0);
    }

    /**
     * <p>Case in-sensitive find of the first index within a CharSequence
     * from the specified position.</p>
     *
     * <p>A {@code null} CharSequence will return {@code -1}.
     * A negative start position is treated as zero.
     * An empty ("") search CharSequence always matches.
     * A start position greater than the string length only matches
     * an empty search CharSequence.</p>
     *
     * <pre>
     * StringUtils.indexOfIgnoreCase(null, *, *)          = -1
     * StringUtils.indexOfIgnoreCase(*, null, *)          = -1
     * StringUtils.indexOfIgnoreCase("", "", 0)           = 0
     * StringUtils.indexOfIgnoreCase("aabaabaa", "A", 0)  = 0
     * StringUtils.indexOfIgnoreCase("aabaabaa", "B", 0)  = 2
     * StringUtils.indexOfIgnoreCase("aabaabaa", "AB", 0) = 1
     * StringUtils.indexOfIgnoreCase("aabaabaa", "B", 3)  = 5
     * StringUtils.indexOfIgnoreCase("aabaabaa", "B", 9)  = -1
     * StringUtils.indexOfIgnoreCase("aabaabaa", "B", -1) = 2
     * StringUtils.indexOfIgnoreCase("aabaabaa", "", 2)   = 2
     * StringUtils.indexOfIgnoreCase("abc", "", 9)        = -1
     * </pre>
     *
     * @param str  the CharSequence to check, may be null
     * @param searchStr  the CharSequence to find, may be null
     * @param startPos  the start position, negative treated as zero
     * @return the first index of the search CharSequence (always &ge; startPos),
     *  -1 if no match or {@code null} string input
     * @since 2.5
     * @since 3.0 Changed signature from indexOfIgnoreCase(String, String, int) to indexOfIgnoreCase(CharSequence, CharSequence, int)
     */
    public static int indexOfIgnoreCase(final CharSequence str, final CharSequence searchStr, int startPos) {
        if (str == null || searchStr == null) {
            return INDEX_NOT_FOUND;
        }
        if (startPos < 0) {
            startPos = 0;
        }
        final int endLimit = str.length() - searchStr.length() + 1;
        if (startPos > endLimit) {
            return INDEX_NOT_FOUND;
        }
        if (searchStr.length() == 0) {
            return startPos;
        }
        for (int i = startPos; i < endLimit; i++) {
            if (CharSequenceUtils.regionMatches(str, true, i, searchStr, 0, searchStr.length())) {
                return i;
            }
        }
        return INDEX_NOT_FOUND;
    }

    /**
     * <p>Checks if any of the CharSequences are empty ("") or null.</p>
     *
     * <pre>
     * StringUtils.isAnyEmpty((String) null)    = true
     * StringUtils.isAnyEmpty((String[]) null)  = false
     * StringUtils.isAnyEmpty(null, "foo")      = true
     * StringUtils.isAnyEmpty("", "bar")        = true
     * StringUtils.isAnyEmpty("bob", "")        = true
     * StringUtils.isAnyEmpty("  bob  ", null)  = true
     * StringUtils.isAnyEmpty(" ", "bar")       = false
     * StringUtils.isAnyEmpty("foo", "bar")     = false
     * StringUtils.isAnyEmpty(new String[]{})   = false
     * StringUtils.isAnyEmpty(new String[]{""}) = true
     * </pre>
     *
     * @param css  the CharSequences to check, may be null or empty
     * @return {@code true} if any of the CharSequences are empty or null
     * @since 3.2
     */
    public static boolean isAnyEmpty(final CharSequence... css) {
        if (ArrayUtils.isEmpty(css)) {
            return false;
        }
        for (final CharSequence cs : css) {
            if (isEmpty(cs)) {
                return true;
            }
        }
        return false;
    }

    /**
     * <p>Checks if a CharSequence is empty (""), null or whitespace only.</p>
     *
     * <p>Whitespace is defined by {@link Character#isWhitespace(char)}.</p>
     *
     * <pre>
     * StringUtils.isBlank(null)      = true
     * StringUtils.isBlank("")        = true
     * StringUtils.isBlank(" ")       = true
     * StringUtils.isBlank("bob")     = false
     * StringUtils.isBlank("  bob  ") = false
     * </pre>
     *
     * @param cs  the CharSequence to check, may be null
     * @return {@code true} if the CharSequence is null, empty or whitespace only
     * @since 2.0
     * @since 3.0 Changed signature from isBlank(String) to isBlank(CharSequence)
     */
    public static boolean isBlank(final CharSequence cs) {
        final int strLen = length(cs);
        if (strLen == 0) {
            return true;
        }
        for (int i = 0; i < strLen; i++) {
            if (!Character.isWhitespace(cs.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    /**
     * <p>Checks if a CharSequence is empty ("") or null.</p>
     *
     * <pre>
     * StringUtils.isEmpty(null)      = true
     * StringUtils.isEmpty("")        = true
     * StringUtils.isEmpty(" ")       = false
     * StringUtils.isEmpty("bob")     = false
     * StringUtils.isEmpty("  bob  ") = false
     * </pre>
     *
     * <p>NOTE: This method changed in Lang version 2.0.
     * It no longer trims the CharSequence.
     * That functionality is available in isBlank().</p>
     *
     * @param cs  the CharSequence to check, may be null
     * @return {@code true} if the CharSequence is empty or null
     * @since 3.0 Changed signature from isEmpty(String) to isEmpty(CharSequence)
     */
    public static boolean isEmpty(final CharSequence cs) {
        return cs == null || cs.length() == 0;
    }

    /**
     * <p>Checks if a CharSequence is not empty (""), not null and not whitespace only.</p>
     *
     * <p>Whitespace is defined by {@link Character#isWhitespace(char)}.</p>
     *
     * <pre>
     * StringUtils.isNotBlank(null)      = false
     * StringUtils.isNotBlank("")        = false
     * StringUtils.isNotBlank(" ")       = false
     * StringUtils.isNotBlank("bob")     = true
     * StringUtils.isNotBlank("  bob  ") = true
     * </pre>
     *
     * @param cs  the CharSequence to check, may be null
     * @return {@code true} if the CharSequence is
     *  not empty and not null and not whitespace only
     * @since 2.0
     * @since 3.0 Changed signature from isNotBlank(String) to isNotBlank(CharSequence)
     */
    public static boolean isNotBlank(final CharSequence cs) {
        return !isBlank(cs);
    }

    /**
     * <p>Checks if a CharSequence is not empty ("") and not null.</p>
     *
     * <pre>
     * StringUtils.isNotEmpty(null)      = false
     * StringUtils.isNotEmpty("")        = false
     * StringUtils.isNotEmpty(" ")       = true
     * StringUtils.isNotEmpty("bob")     = true
     * StringUtils.isNotEmpty("  bob  ") = true
     * </pre>
     *
     * @param cs  the CharSequence to check, may be null
     * @return {@code true} if the CharSequence is not empty and not null
     * @since 3.0 Changed signature from isNotEmpty(String) to isNotEmpty(CharSequence)
     */
    public static boolean isNotEmpty(final CharSequence cs) {
        return !isEmpty(cs);
    }

    /**
     * <p>Checks if the CharSequence contains only Unicode digits.
     * A decimal point is not a Unicode digit and returns false.</p>
     *
     * <p>{@code null} will return {@code false}.
     * An empty CharSequence (length()=0) will return {@code false}.</p>
     *
     * <p>Note that the method does not allow for a leading sign, either positive or negative.
     * Also, if a String passes the numeric test, it may still generate a NumberFormatException
     * when parsed by Integer.parseInt or Long.parseLong, e.g. if the value is outside the range
     * for int or long respectively.</p>
     *
     * <pre>
     * StringUtils.isNumeric(null)   = false
     * StringUtils.isNumeric("")     = false
     * StringUtils.isNumeric("  ")   = false
     * StringUtils.isNumeric("123")  = true
     * StringUtils.isNumeric("\u0967\u0968\u0969")  = true
     * StringUtils.isNumeric("12 3") = false
     * StringUtils.isNumeric("ab2c") = false
     * StringUtils.isNumeric("12-3") = false
     * StringUtils.isNumeric("12.3") = false
     * StringUtils.isNumeric("-123") = false
     * StringUtils.isNumeric("+123") = false
     * </pre>
     *
     * @param cs  the CharSequence to check, may be null
     * @return {@code true} if only contains digits, and is non-null
     * @since 3.0 Changed signature from isNumeric(String) to isNumeric(CharSequence)
     * @since 3.0 Changed "" to return false and not true
     */
    public static boolean isNumeric(final CharSequence cs) {
        if (isEmpty(cs)) {
            return false;
        }
        final int sz = cs.length();
        for (int i = 0; i < sz; i++) {
            if (!Character.isDigit(cs.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    /**
     * <p>Joins the elements of the provided array into a single String
     * containing the provided list of elements.</p>
     *
     * <p>No delimiter is added before or after the list.
     * A {@code null} separator is the same as an empty String ("").
     * Null objects or empty strings within the array are represented by
     * empty strings.</p>
     *
     * <pre>
     * StringUtils.join(null, *)                = null
     * StringUtils.join([], *)                  = ""
     * StringUtils.join([null], *)              = ""
     * StringUtils.join(["a", "b", "c"], "--")  = "a--b--c"
     * StringUtils.join(["a", "b", "c"], null)  = "abc"
     * StringUtils.join(["a", "b", "c"], "")    = "abc"
     * StringUtils.join([null, "", "a"], ',')   = ",,a"
     * </pre>
     *
     * @param array  the array of values to join together, may be null
     * @param delimiter  the separator character to use, null treated as ""
     * @return the joined String, {@code null} if null array input
     */
    public static String join(final Object[] array, final String delimiter) {
        if (array == null) {
            return null;
        }
        return join(array, delimiter, 0, array.length);
    }

    /**
     * <p>Joins the elements of the provided array into a single String
     * containing the provided list of elements.</p>
     *
     * <p>No delimiter is added before or after the list.
     * A {@code null} separator is the same as an empty String ("").
     * Null objects or empty strings within the array are represented by
     * empty strings.</p>
     *
     * <pre>
     * StringUtils.join(null, *, *, *)                = null
     * StringUtils.join([], *, *, *)                  = ""
     * StringUtils.join([null], *, *, *)              = ""
     * StringUtils.join(["a", "b", "c"], "--", 0, 3)  = "a--b--c"
     * StringUtils.join(["a", "b", "c"], "--", 1, 3)  = "b--c"
     * StringUtils.join(["a", "b", "c"], "--", 2, 3)  = "c"
     * StringUtils.join(["a", "b", "c"], "--", 2, 2)  = ""
     * StringUtils.join(["a", "b", "c"], null, 0, 3)  = "abc"
     * StringUtils.join(["a", "b", "c"], "", 0, 3)    = "abc"
     * StringUtils.join([null, "", "a"], ',', 0, 3)   = ",,a"
     * </pre>
     *
     * @param array  the array of values to join together, may be null
     * @param delimiter  the separator character to use, null treated as ""
     * @param startIndex the first index to start joining from.
     * @param endIndex the index to stop joining from (exclusive).
     * @return the joined String, {@code null} if null array input; or the empty string
     * if {@code endIndex - startIndex <= 0}. The number of joined entries is given by
     * {@code endIndex - startIndex}
     * @throws ArrayIndexOutOfBoundsException ife<br>
     * {@code startIndex < 0} or <br>
     * {@code startIndex >= array.length()} or <br>
     * {@code endIndex < 0} or <br>
     * {@code endIndex > array.length()}
     */
    public static String join(final Object[] array, final String delimiter, final int startIndex, final int endIndex) {
        if (array == null) {
            return null;
        }
        if (endIndex - startIndex <= 0) {
            return EMPTY;
        }
        final StringJoiner joiner = new StringJoiner(toStringOrEmpty(delimiter));
        for (int i = startIndex; i < endIndex; i++) {
            joiner.add(toStringOrEmpty(array[i]));
        }
        return joiner.toString();
    }

    /**
     * <p>Finds the last index within a CharSequence, handling {@code null}.
     * This method uses {@link String#lastIndexOf(String)} if possible.</p>
     *
     * <p>A {@code null} CharSequence will return {@code -1}.</p>
     *
     * <pre>
     * StringUtils.lastIndexOf(null, *)          = -1
     * StringUtils.lastIndexOf(*, null)          = -1
     * StringUtils.lastIndexOf("", "")           = 0
     * StringUtils.lastIndexOf("aabaabaa", "a")  = 7
     * StringUtils.lastIndexOf("aabaabaa", "b")  = 5
     * StringUtils.lastIndexOf("aabaabaa", "ab") = 4
     * StringUtils.lastIndexOf("aabaabaa", "")   = 8
     * </pre>
     *
     * @param seq  the CharSequence to check, may be null
     * @param searchSeq  the CharSequence to find, may be null
     * @return the last index of the search String,
     *  -1 if no match or {@code null} string input
     * @since 2.0
     * @since 3.0 Changed signature from lastIndexOf(String, String) to lastIndexOf(CharSequence, CharSequence)
     */
    public static int lastIndexOf(final CharSequence seq, final CharSequence searchSeq) {
        if (seq == null) {
            return INDEX_NOT_FOUND;
        }
        return CharSequenceUtils.lastIndexOf(seq, searchSeq, seq.length());
    }

    /**
     * <p>Case in-sensitive find of the last index within a CharSequence.</p>
     *
     * <p>A {@code null} CharSequence will return {@code -1}.
     * A negative start position returns {@code -1}.
     * An empty ("") search CharSequence always matches unless the start position is negative.
     * A start position greater than the string length searches the whole string.</p>
     *
     * <pre>
     * StringUtils.lastIndexOfIgnoreCase(null, *)          = -1
     * StringUtils.lastIndexOfIgnoreCase(*, null)          = -1
     * StringUtils.lastIndexOfIgnoreCase("aabaabaa", "A")  = 7
     * StringUtils.lastIndexOfIgnoreCase("aabaabaa", "B")  = 5
     * StringUtils.lastIndexOfIgnoreCase("aabaabaa", "AB") = 4
     * </pre>
     *
     * @param str  the CharSequence to check, may be null
     * @param searchStr  the CharSequence to find, may be null
     * @return the first index of the search CharSequence,
     *  -1 if no match or {@code null} string input
     * @since 2.5
     * @since 3.0 Changed signature from lastIndexOfIgnoreCase(String, String) to lastIndexOfIgnoreCase(CharSequence, CharSequence)
     */
    public static int lastIndexOfIgnoreCase(final CharSequence str, final CharSequence searchStr) {
        if (str == null || searchStr == null) {
            return INDEX_NOT_FOUND;
        }
        return lastIndexOfIgnoreCase(str, searchStr, str.length());
    }

    /**
     * <p>Case in-sensitive find of the last index within a CharSequence
     * from the specified position.</p>
     *
     * <p>A {@code null} CharSequence will return {@code -1}.
     * A negative start position returns {@code -1}.
     * An empty ("") search CharSequence always matches unless the start position is negative.
     * A start position greater than the string length searches the whole string.
     * The search starts at the startPos and works backwards; matches starting after the start
     * position are ignored.
     * </p>
     *
     * <pre>
     * StringUtils.lastIndexOfIgnoreCase(null, *, *)          = -1
     * StringUtils.lastIndexOfIgnoreCase(*, null, *)          = -1
     * StringUtils.lastIndexOfIgnoreCase("aabaabaa", "A", 8)  = 7
     * StringUtils.lastIndexOfIgnoreCase("aabaabaa", "B", 8)  = 5
     * StringUtils.lastIndexOfIgnoreCase("aabaabaa", "AB", 8) = 4
     * StringUtils.lastIndexOfIgnoreCase("aabaabaa", "B", 9)  = 5
     * StringUtils.lastIndexOfIgnoreCase("aabaabaa", "B", -1) = -1
     * StringUtils.lastIndexOfIgnoreCase("aabaabaa", "A", 0)  = 0
     * StringUtils.lastIndexOfIgnoreCase("aabaabaa", "B", 0)  = -1
     * </pre>
     *
     * @param str  the CharSequence to check, may be null
     * @param searchStr  the CharSequence to find, may be null
     * @param startPos  the start position
     * @return the last index of the search CharSequence (always &le; startPos),
     *  -1 if no match or {@code null} input
     * @since 2.5
     * @since 3.0 Changed signature from lastIndexOfIgnoreCase(String, String, int) to lastIndexOfIgnoreCase(CharSequence, CharSequence, int)
     */
    public static int lastIndexOfIgnoreCase(final CharSequence str, final CharSequence searchStr, int startPos) {
        if (str == null || searchStr == null) {
            return INDEX_NOT_FOUND;
        }
        final int searchStrLength = searchStr.length();
        final int strLength = str.length();
        if (startPos > strLength - searchStrLength) {
            startPos = strLength - searchStrLength;
        }
        if (startPos < 0) {
            return INDEX_NOT_FOUND;
        }
        if (searchStrLength == 0) {
            return startPos;
        }

        for (int i = startPos; i >= 0; i--) {
            if (CharSequenceUtils.regionMatches(str, true, i, searchStr, 0, searchStrLength)) {
                return i;
            }
        }
        return INDEX_NOT_FOUND;
    }

    /**
     * <p>Left pad a String with a specified character.</p>
     *
     * <p>Pad to a size of {@code size}.</p>
     *
     * <pre>
     * StringUtils.leftPad(null, *, *)     = null
     * StringUtils.leftPad("", 3, 'z')     = "zzz"
     * StringUtils.leftPad("bat", 3, 'z')  = "bat"
     * StringUtils.leftPad("bat", 5, 'z')  = "zzbat"
     * StringUtils.leftPad("bat", 1, 'z')  = "bat"
     * StringUtils.leftPad("bat", -1, 'z') = "bat"
     * </pre>
     *
     * @param str  the String to pad out, may be null
     * @param size  the size to pad to
     * @param padChar  the character to pad with
     * @return left padded String or original String if no padding is necessary,
     *  {@code null} if null String input
     * @since 2.0
     */
    public static String leftPad(final String str, final int size, final char padChar) {
        if (str == null) {
            return null;
        }
        final int pads = size - str.length();
        if (pads <= 0) {
            return str; // returns original String when possible
        }
        if (pads > PAD_LIMIT) {
            return leftPad(str, size, String.valueOf(padChar));
        }
        return repeat(padChar, pads).concat(str);
    }

    /**
     * <p>Left pad a String with a specified String.</p>
     *
     * <p>Pad to a size of {@code size}.</p>
     *
     * <pre>
     * StringUtils.leftPad(null, *, *)      = null
     * StringUtils.leftPad("", 3, "z")      = "zzz"
     * StringUtils.leftPad("bat", 3, "yz")  = "bat"
     * StringUtils.leftPad("bat", 5, "yz")  = "yzbat"
     * StringUtils.leftPad("bat", 8, "yz")  = "yzyzybat"
     * StringUtils.leftPad("bat", 1, "yz")  = "bat"
     * StringUtils.leftPad("bat", -1, "yz") = "bat"
     * StringUtils.leftPad("bat", 5, null)  = "  bat"
     * StringUtils.leftPad("bat", 5, "")    = "  bat"
     * </pre>
     *
     * @param str  the String to pad out, may be null
     * @param size  the size to pad to
     * @param padStr  the String to pad with, null or empty treated as single space
     * @return left padded String or original String if no padding is necessary,
     *  {@code null} if null String input
     */
    public static String leftPad(final String str, final int size, String padStr) {
        if (str == null) {
            return null;
        }
        if (isEmpty(padStr)) {
            padStr = SPACE;
        }
        final int padLen = padStr.length();
        final int strLen = str.length();
        final int pads = size - strLen;
        if (pads <= 0) {
            return str; // returns original String when possible
        }
        if (padLen == 1 && pads <= PAD_LIMIT) {
            return leftPad(str, size, padStr.charAt(0));
        }

        if (pads == padLen) {
            return padStr.concat(str);
        } else if (pads < padLen) {
            return padStr.substring(0, pads).concat(str);
        } else {
            final char[] padding = new char[pads];
            final char[] padChars = padStr.toCharArray();
            for (int i = 0; i < pads; i++) {
                padding[i] = padChars[i % padLen];
            }
            return new String(padding).concat(str);
        }
    }

    /**
     * Gets a CharSequence length or {@code 0} if the CharSequence is
     * {@code null}.
     *
     * @param cs
     *            a CharSequence or {@code null}
     * @return CharSequence length or {@code 0} if the CharSequence is
     *         {@code null}.
     * @since 2.4
     * @since 3.0 Changed signature from length(String) to length(CharSequence)
     */
    public static int length(final CharSequence cs) {
        return cs == null ? 0 : cs.length();
    }

    /**
     * <p>Converts a String to lower case as per {@link String#toLowerCase()}.</p>
     *
     * <p>A {@code null} input String returns {@code null}.</p>
     *
     * <pre>
     * StringUtils.lowerCase(null)  = null
     * StringUtils.lowerCase("")    = ""
     * StringUtils.lowerCase("aBc") = "abc"
     * </pre>
     *
     * <p><strong>Note:</strong> As described in the documentation for {@link String#toLowerCase()},
     * the result of this method is affected by the current locale.
     *
     * @param str  the String to lower case, may be null
     * @return the lower cased String, {@code null} if null String input
     */
    public static String lowerCase(final String str) {
        if (str == null) {
            return null;
        }
        return str.toLowerCase();
    }

    /**
     * Prepends the prefix to the start of the string if the string does not
     * already start with any of the prefixes.
     *
     * @param str The string.
     * @param prefix The prefix to prepend to the start of the string.
     * @param ignoreCase Indicates whether the compare should ignore case.
     * @param prefixes Additional prefixes that are valid (optional).
     *
     * @return A new String if prefix was prepended, the same string otherwise.
     */
    private static String prependIfMissing(final String str, final CharSequence prefix, final boolean ignoreCase, final CharSequence... prefixes) {
        if (str == null || isEmpty(prefix) || startsWith(str, prefix, ignoreCase)) {
            return str;
        }
        if (ArrayUtils.isNotEmpty(prefixes)) {
            for (final CharSequence p : prefixes) {
                if (startsWith(str, p, ignoreCase)) {
                    return str;
                }
            }
        }
        return prefix + str;
    }

    /**
     * Prepends the prefix to the start of the string if the string does not
     * already start with any of the prefixes.
     *
     * <pre>
     * StringUtils.prependIfMissing(null, null) = null
     * StringUtils.prependIfMissing("abc", null) = "abc"
     * StringUtils.prependIfMissing("", "xyz") = "xyz"
     * StringUtils.prependIfMissing("abc", "xyz") = "xyzabc"
     * StringUtils.prependIfMissing("xyzabc", "xyz") = "xyzabc"
     * StringUtils.prependIfMissing("XYZabc", "xyz") = "xyzXYZabc"
     * </pre>
     * <p>With additional prefixes,</p>
     * <pre>
     * StringUtils.prependIfMissing(null, null, null) = null
     * StringUtils.prependIfMissing("abc", null, null) = "abc"
     * StringUtils.prependIfMissing("", "xyz", null) = "xyz"
     * StringUtils.prependIfMissing("abc", "xyz", new CharSequence[]{null}) = "xyzabc"
     * StringUtils.prependIfMissing("abc", "xyz", "") = "abc"
     * StringUtils.prependIfMissing("abc", "xyz", "mno") = "xyzabc"
     * StringUtils.prependIfMissing("xyzabc", "xyz", "mno") = "xyzabc"
     * StringUtils.prependIfMissing("mnoabc", "xyz", "mno") = "mnoabc"
     * StringUtils.prependIfMissing("XYZabc", "xyz", "mno") = "xyzXYZabc"
     * StringUtils.prependIfMissing("MNOabc", "xyz", "mno") = "xyzMNOabc"
     * </pre>
     *
     * @param str The string.
     * @param prefix The prefix to prepend to the start of the string.
     * @param prefixes Additional prefixes that are valid.
     *
     * @return A new String if prefix was prepended, the same string otherwise.
     *
     * @since 3.2
     */
    public static String prependIfMissing(final String str, final CharSequence prefix, final CharSequence... prefixes) {
        return prependIfMissing(str, prefix, false, prefixes);
    }

    /**
     * Prepends the prefix to the start of the string if the string does not
     * already start, case insensitive, with any of the prefixes.
     *
     * <pre>
     * StringUtils.prependIfMissingIgnoreCase(null, null) = null
     * StringUtils.prependIfMissingIgnoreCase("abc", null) = "abc"
     * StringUtils.prependIfMissingIgnoreCase("", "xyz") = "xyz"
     * StringUtils.prependIfMissingIgnoreCase("abc", "xyz") = "xyzabc"
     * StringUtils.prependIfMissingIgnoreCase("xyzabc", "xyz") = "xyzabc"
     * StringUtils.prependIfMissingIgnoreCase("XYZabc", "xyz") = "XYZabc"
     * </pre>
     * <p>With additional prefixes,</p>
     * <pre>
     * StringUtils.prependIfMissingIgnoreCase(null, null, null) = null
     * StringUtils.prependIfMissingIgnoreCase("abc", null, null) = "abc"
     * StringUtils.prependIfMissingIgnoreCase("", "xyz", null) = "xyz"
     * StringUtils.prependIfMissingIgnoreCase("abc", "xyz", new CharSequence[]{null}) = "xyzabc"
     * StringUtils.prependIfMissingIgnoreCase("abc", "xyz", "") = "abc"
     * StringUtils.prependIfMissingIgnoreCase("abc", "xyz", "mno") = "xyzabc"
     * StringUtils.prependIfMissingIgnoreCase("xyzabc", "xyz", "mno") = "xyzabc"
     * StringUtils.prependIfMissingIgnoreCase("mnoabc", "xyz", "mno") = "mnoabc"
     * StringUtils.prependIfMissingIgnoreCase("XYZabc", "xyz", "mno") = "XYZabc"
     * StringUtils.prependIfMissingIgnoreCase("MNOabc", "xyz", "mno") = "MNOabc"
     * </pre>
     *
     * @param str The string.
     * @param prefix The prefix to prepend to the start of the string.
     * @param prefixes Additional prefixes that are valid (optional).
     *
     * @return A new String if prefix was prepended, the same string otherwise.
     *
     * @since 3.2
     */
    public static String prependIfMissingIgnoreCase(final String str, final CharSequence prefix, final CharSequence... prefixes) {
        return prependIfMissing(str, prefix, true, prefixes);
    }

    /**
     * <p>Removes a substring only if it is at the end of a source string,
     * otherwise returns the source string.</p>
     *
     * <p>A {@code null} source string will return {@code null}.
     * An empty ("") source string will return the empty string.
     * A {@code null} search string will return the source string.</p>
     *
     * <pre>
     * StringUtils.removeEnd(null, *)      = null
     * StringUtils.removeEnd("", *)        = ""
     * StringUtils.removeEnd(*, null)      = *
     * StringUtils.removeEnd("www.domain.com", ".com.")  = "www.domain.com"
     * StringUtils.removeEnd("www.domain.com", ".com")   = "www.domain"
     * StringUtils.removeEnd("www.domain.com", "domain") = "www.domain.com"
     * StringUtils.removeEnd("abc", "")    = "abc"
     * </pre>
     *
     * @param str  the source String to search, may be null
     * @param remove  the String to search for and remove, may be null
     * @return the substring with the string removed if found,
     *  {@code null} if null String input
     * @since 2.1
     */
    public static String removeEnd(final String str, final String remove) {
        if (isEmpty(str) || isEmpty(remove)) {
            return str;
        }
        if (str.endsWith(remove)) {
            return str.substring(0, str.length() - remove.length());
        }
        return str;
    }

    /**
     * <p>Case insensitive removal of a substring if it is at the end of a source string,
     * otherwise returns the source string.</p>
     *
     * <p>A {@code null} source string will return {@code null}.
     * An empty ("") source string will return the empty string.
     * A {@code null} search string will return the source string.</p>
     *
     * <pre>
     * StringUtils.removeEndIgnoreCase(null, *)      = null
     * StringUtils.removeEndIgnoreCase("", *)        = ""
     * StringUtils.removeEndIgnoreCase(*, null)      = *
     * StringUtils.removeEndIgnoreCase("www.domain.com", ".com.")  = "www.domain.com"
     * StringUtils.removeEndIgnoreCase("www.domain.com", ".com")   = "www.domain"
     * StringUtils.removeEndIgnoreCase("www.domain.com", "domain") = "www.domain.com"
     * StringUtils.removeEndIgnoreCase("abc", "")    = "abc"
     * StringUtils.removeEndIgnoreCase("www.domain.com", ".COM") = "www.domain")
     * StringUtils.removeEndIgnoreCase("www.domain.COM", ".com") = "www.domain")
     * </pre>
     *
     * @param str  the source String to search, may be null
     * @param remove  the String to search for (case insensitive) and remove, may be null
     * @return the substring with the string removed if found,
     *  {@code null} if null String input
     * @since 2.4
     */
    public static String removeEndIgnoreCase(final String str, final String remove) {
        if (isEmpty(str) || isEmpty(remove)) {
            return str;
        }
        if (endsWithIgnoreCase(str, remove)) {
            return str.substring(0, str.length() - remove.length());
        }
        return str;
    }

    /**
     * <p>Removes a substring only if it is at the beginning of a source string,
     * otherwise returns the source string.</p>
     *
     * <p>A {@code null} source string will return {@code null}.
     * An empty ("") source string will return the empty string.
     * A {@code null} search string will return the source string.</p>
     *
     * <pre>
     * StringUtils.removeStart(null, *)      = null
     * StringUtils.removeStart("", *)        = ""
     * StringUtils.removeStart(*, null)      = *
     * StringUtils.removeStart("www.domain.com", "www.")   = "domain.com"
     * StringUtils.removeStart("domain.com", "www.")       = "domain.com"
     * StringUtils.removeStart("www.domain.com", "domain") = "www.domain.com"
     * StringUtils.removeStart("abc", "")    = "abc"
     * </pre>
     *
     * @param str  the source String to search, may be null
     * @param remove  the String to search for and remove, may be null
     * @return the substring with the string removed if found,
     *  {@code null} if null String input
     * @since 2.1
     */
    public static String removeStart(final String str, final String remove) {
        if (isEmpty(str) || isEmpty(remove)) {
            return str;
        }
        if (str.startsWith(remove)) {
            return str.substring(remove.length());
        }
        return str;
    }

    /**
     * <p>Case insensitive removal of a substring if it is at the beginning of a source string,
     * otherwise returns the source string.</p>
     *
     * <p>A {@code null} source string will return {@code null}.
     * An empty ("") source string will return the empty string.
     * A {@code null} search string will return the source string.</p>
     *
     * <pre>
     * StringUtils.removeStartIgnoreCase(null, *)      = null
     * StringUtils.removeStartIgnoreCase("", *)        = ""
     * StringUtils.removeStartIgnoreCase(*, null)      = *
     * StringUtils.removeStartIgnoreCase("www.domain.com", "www.")   = "domain.com"
     * StringUtils.removeStartIgnoreCase("www.domain.com", "WWW.")   = "domain.com"
     * StringUtils.removeStartIgnoreCase("domain.com", "www.")       = "domain.com"
     * StringUtils.removeStartIgnoreCase("www.domain.com", "domain") = "www.domain.com"
     * StringUtils.removeStartIgnoreCase("abc", "")    = "abc"
     * </pre>
     *
     * @param str  the source String to search, may be null
     * @param remove  the String to search for (case insensitive) and remove, may be null
     * @return the substring with the string removed if found,
     *  {@code null} if null String input
     * @since 2.4
     */
    public static String removeStartIgnoreCase(final String str, final String remove) {
        if (str != null && startsWithIgnoreCase(str, remove)) {
            return str.substring(length(remove));
        }
        return str;
    }

    /**
     * <p>Returns padding using the specified delimiter repeated
     * to a given length.</p>
     *
     * <pre>
     * StringUtils.repeat('e', 0)  = ""
     * StringUtils.repeat('e', 3)  = "eee"
     * StringUtils.repeat('e', -2) = ""
     * </pre>
     *
     * <p>Note: this method does not support padding with
     * <a href="http://www.unicode.org/glossary/#supplementary_character">Unicode Supplementary Characters</a>
     * as they require a pair of {@code char}s to be represented.
     * If you are needing to support full I18N of your applications
     * consider using {@link #repeat(String, int)} instead.
     * </p>
     *
     * @param ch  character to repeat
     * @param repeat  number of times to repeat char, negative treated as zero
     * @return String with repeated character
     * @see #repeat(String, int)
     */
    public static String repeat(final char ch, final int repeat) {
        if (repeat <= 0) {
            return EMPTY;
        }
        final char[] buf = new char[repeat];
        Arrays.fill(buf, ch);
        return new String(buf);
    }

    /**
     * <p>Repeat a String {@code repeat} times to form a
     * new String.</p>
     *
     * <pre>
     * StringUtils.repeat(null, 2) = null
     * StringUtils.repeat("", 0)   = ""
     * StringUtils.repeat("", 2)   = ""
     * StringUtils.repeat("a", 3)  = "aaa"
     * StringUtils.repeat("ab", 2) = "abab"
     * StringUtils.repeat("a", -2) = ""
     * </pre>
     *
     * @param str  the String to repeat, may be null
     * @param repeat  number of times to repeat str, negative treated as zero
     * @return a new String consisting of the original String repeated,
     *  {@code null} if null String input
     */
    public static String repeat(final String str, final int repeat) {
        // Performance tuned for 2.0 (JDK1.4)
        if (str == null) {
            return null;
        }
        if (repeat <= 0) {
            return EMPTY;
        }
        final int inputLength = str.length();
        if (repeat == 1 || inputLength == 0) {
            return str;
        }
        if (inputLength == 1 && repeat <= PAD_LIMIT) {
            return repeat(str.charAt(0), repeat);
        }

        final int outputLength = inputLength * repeat;
        switch (inputLength) {
            case 1 :
                return repeat(str.charAt(0), repeat);
            case 2 :
                final char ch0 = str.charAt(0);
                final char ch1 = str.charAt(1);
                final char[] output2 = new char[outputLength];
                for (int i = repeat * 2 - 2; i >= 0; i--, i--) {
                    output2[i] = ch0;
                    output2[i + 1] = ch1;
                }
                return new String(output2);
            default :
                final StringBuilder buf = new StringBuilder(outputLength);
                for (int i = 0; i < repeat; i++) {
                    buf.append(str);
                }
                return buf.toString();
        }
    }

    /**
     * <p>Replaces a String with another String inside a larger String,
     * for the first {@code max} values of the search String,
     * case sensitively/insensitively based on {@code ignoreCase} value.</p>
     *
     * <p>A {@code null} reference passed to this method is a no-op.</p>
     *
     * <pre>
     * StringUtils.replace(null, *, *, *, false)         = null
     * StringUtils.replace("", *, *, *, false)           = ""
     * StringUtils.replace("any", null, *, *, false)     = "any"
     * StringUtils.replace("any", *, null, *, false)     = "any"
     * StringUtils.replace("any", "", *, *, false)       = "any"
     * StringUtils.replace("any", *, *, 0, false)        = "any"
     * StringUtils.replace("abaa", "a", null, -1, false) = "abaa"
     * StringUtils.replace("abaa", "a", "", -1, false)   = "b"
     * StringUtils.replace("abaa", "a", "z", 0, false)   = "abaa"
     * StringUtils.replace("abaa", "A", "z", 1, false)   = "abaa"
     * StringUtils.replace("abaa", "A", "z", 1, true)   = "zbaa"
     * StringUtils.replace("abAa", "a", "z", 2, true)   = "zbza"
     * StringUtils.replace("abAa", "a", "z", -1, true)  = "zbzz"
     * </pre>
     *
     * @param text  text to search and replace in, may be null
     * @param searchString  the String to search for (case insensitive), may be null
     * @param replacement  the String to replace it with, may be null
     * @param max  maximum number of values to replace, or {@code -1} if no maximum
     * @param ignoreCase if true replace is case insensitive, otherwise case sensitive
     * @return the text with any replacements processed,
     *  {@code null} if null String input
     */
    public static String replace(final String text, String searchString, final String replacement, int max, final boolean ignoreCase) {
        if (isEmpty(text) || isEmpty(searchString) || replacement == null || max == 0) {
            return text;
        }
        if (ignoreCase) {
            searchString = searchString.toLowerCase();
        }
        int start = 0;
        int end = ignoreCase ? indexOfIgnoreCase(text, searchString, start) : indexOf(text, searchString, start);
        if (end == INDEX_NOT_FOUND) {
            return text;
        }
        final int replLength = searchString.length();
        int increase = Math.max(replacement.length() - replLength, 0);
        increase *= max < 0 ? 16 : Math.min(max, 64);
        final StringBuilder buf = new StringBuilder(text.length() + increase);
        while (end != INDEX_NOT_FOUND) {
            buf.append(text, start, end).append(replacement);
            start = end + replLength;
            if (--max == 0) {
                break;
            }
            end = ignoreCase ? indexOfIgnoreCase(text, searchString, start) : indexOf(text, searchString, start);
        }
        buf.append(text, start, text.length());
        return buf.toString();
    }

    /**
     * <p>Right pad a String with a specified character.</p>
     *
     * <p>The String is padded to the size of {@code size}.</p>
     *
     * <pre>
     * StringUtils.rightPad(null, *, *)     = null
     * StringUtils.rightPad("", 3, 'z')     = "zzz"
     * StringUtils.rightPad("bat", 3, 'z')  = "bat"
     * StringUtils.rightPad("bat", 5, 'z')  = "batzz"
     * StringUtils.rightPad("bat", 1, 'z')  = "bat"
     * StringUtils.rightPad("bat", -1, 'z') = "bat"
     * </pre>
     *
     * @param str  the String to pad out, may be null
     * @param size  the size to pad to
     * @param padChar  the character to pad with
     * @return right padded String or original String if no padding is necessary,
     *  {@code null} if null String input
     * @since 2.0
     */
    public static String rightPad(final String str, final int size, final char padChar) {
        if (str == null) {
            return null;
        }
        final int pads = size - str.length();
        if (pads <= 0) {
            return str; // returns original String when possible
        }
        if (pads > PAD_LIMIT) {
            return rightPad(str, size, String.valueOf(padChar));
        }
        return str.concat(repeat(padChar, pads));
    }

    /**
     * <p>Right pad a String with a specified String.</p>
     *
     * <p>The String is padded to the size of {@code size}.</p>
     *
     * <pre>
     * StringUtils.rightPad(null, *, *)      = null
     * StringUtils.rightPad("", 3, "z")      = "zzz"
     * StringUtils.rightPad("bat", 3, "yz")  = "bat"
     * StringUtils.rightPad("bat", 5, "yz")  = "batyz"
     * StringUtils.rightPad("bat", 8, "yz")  = "batyzyzy"
     * StringUtils.rightPad("bat", 1, "yz")  = "bat"
     * StringUtils.rightPad("bat", -1, "yz") = "bat"
     * StringUtils.rightPad("bat", 5, null)  = "bat  "
     * StringUtils.rightPad("bat", 5, "")    = "bat  "
     * </pre>
     *
     * @param str  the String to pad out, may be null
     * @param size  the size to pad to
     * @param padStr  the String to pad with, null or empty treated as single space
     * @return right padded String or original String if no padding is necessary,
     *  {@code null} if null String input
     */
    public static String rightPad(final String str, final int size, String padStr) {
        if (str == null) {
            return null;
        }
        if (isEmpty(padStr)) {
            padStr = SPACE;
        }
        final int padLen = padStr.length();
        final int strLen = str.length();
        final int pads = size - strLen;
        if (pads <= 0) {
            return str; // returns original String when possible
        }
        if (padLen == 1 && pads <= PAD_LIMIT) {
            return rightPad(str, size, padStr.charAt(0));
        }

        if (pads == padLen) {
            return str.concat(padStr);
        } else if (pads < padLen) {
            return str.concat(padStr.substring(0, pads));
        } else {
            final char[] padding = new char[pads];
            final char[] padChars = padStr.toCharArray();
            for (int i = 0; i < pads; i++) {
                padding[i] = padChars[i % padLen];
            }
            return str.concat(new String(padding));
        }
    }

    /**
     * <p>Splits the provided text into an array, separators specified.
     * This is an alternative to using StringTokenizer.</p>
     *
     * <p>The separator is not included in the returned String array.
     * Adjacent separators are treated as one separator.
     * For more control over the split use the StrTokenizer class.</p>
     *
     * <p>A {@code null} input String returns {@code null}.
     * A {@code null} separatorChars splits on whitespace.</p>
     *
     * <pre>
     * StringUtils.split(null, *)         = null
     * StringUtils.split("", *)           = []
     * StringUtils.split("abc def", null) = ["abc", "def"]
     * StringUtils.split("abc def", " ")  = ["abc", "def"]
     * StringUtils.split("abc  def", " ") = ["abc", "def"]
     * StringUtils.split("ab:cd:ef", ":") = ["ab", "cd", "ef"]
     * </pre>
     *
     * @param str  the String to parse, may be null
     * @param separatorChars  the characters used as the delimiters,
     *  {@code null} splits on whitespace
     * @return an array of parsed Strings, {@code null} if null String input
     */
    public static String[] split(final String str, final String separatorChars) {
        return splitWorker(str, separatorChars, -1, false);
    }

    /**
     * Performs the logic for the {@code split} and
     * {@code splitPreserveAllTokens} methods that return a maximum array
     * length.
     *
     * @param str  the String to parse, may be {@code null}
     * @param separatorChars the separate character
     * @param max  the maximum number of elements to include in the
     *  array. A zero or negative value implies no limit.
     * @param preserveAllTokens if {@code true}, adjacent separators are
     * treated as empty token separators; if {@code false}, adjacent
     * separators are treated as one separator.
     * @return an array of parsed Strings, {@code null} if null String input
     */
    private static String[] splitWorker(final String str, final String separatorChars, final int max, final boolean preserveAllTokens) {
        // Performance tuned for 2.0 (JDK1.4)
        // Direct code is quicker than StringTokenizer.
        // Also, StringTokenizer uses isSpace() not isWhitespace()

        if (str == null) {
            return null;
        }
        final int len = str.length();
        if (len == 0) {
            return ArrayUtils.EMPTY_STRING_ARRAY;
        }
        final List<String> list = new ArrayList<>();
        int sizePlus1 = 1;
        int i = 0;
        int start = 0;
        boolean match = false;
        boolean lastMatch = false;
        if (separatorChars == null) {
            // Null separator means use whitespace
            while (i < len) {
                if (Character.isWhitespace(str.charAt(i))) {
                    if (match || preserveAllTokens) {
                        lastMatch = true;
                        if (sizePlus1++ == max) {
                            i = len;
                            lastMatch = false;
                        }
                        list.add(str.substring(start, i));
                        match = false;
                    }
                    start = ++i;
                    continue;
                }
                lastMatch = false;
                match = true;
                i++;
            }
        } else if (separatorChars.length() == 1) {
            // Optimise 1 character case
            final char sep = separatorChars.charAt(0);
            while (i < len) {
                if (str.charAt(i) == sep) {
                    if (match || preserveAllTokens) {
                        lastMatch = true;
                        if (sizePlus1++ == max) {
                            i = len;
                            lastMatch = false;
                        }
                        list.add(str.substring(start, i));
                        match = false;
                    }
                    start = ++i;
                    continue;
                }
                lastMatch = false;
                match = true;
                i++;
            }
        } else {
            // standard case
            while (i < len) {
                if (separatorChars.indexOf(str.charAt(i)) >= 0) {
                    if (match || preserveAllTokens) {
                        lastMatch = true;
                        if (sizePlus1++ == max) {
                            i = len;
                            lastMatch = false;
                        }
                        list.add(str.substring(start, i));
                        match = false;
                    }
                    start = ++i;
                    continue;
                }
                lastMatch = false;
                match = true;
                i++;
            }
        }
        if (match || preserveAllTokens && lastMatch) {
            list.add(str.substring(start, i));
        }
        return list.toArray(ArrayUtils.EMPTY_STRING_ARRAY);
    }

    /**
     * <p>Check if a CharSequence starts with a specified prefix.</p>
     *
     * <p>{@code null}s are handled without exceptions. Two {@code null}
     * references are considered to be equal. The comparison is case sensitive.</p>
     *
     * <pre>
     * StringUtils.startsWith(null, null)      = true
     * StringUtils.startsWith(null, "abc")     = false
     * StringUtils.startsWith("abcdef", null)  = false
     * StringUtils.startsWith("abcdef", "abc") = true
     * StringUtils.startsWith("ABCDEF", "abc") = false
     * </pre>
     *
     * @see java.lang.String#startsWith(String)
     * @param str  the CharSequence to check, may be null
     * @param prefix the prefix to find, may be null
     * @return {@code true} if the CharSequence starts with the prefix, case sensitive, or
     *  both {@code null}
     * @since 2.4
     * @since 3.0 Changed signature from startsWith(String, String) to startsWith(CharSequence, CharSequence)
     */
    public static boolean startsWith(final CharSequence str, final CharSequence prefix) {
        return startsWith(str, prefix, false);
    }

    /**
     * <p>Check if a CharSequence starts with a specified prefix (optionally case insensitive).</p>
     *
     * @see java.lang.String#startsWith(String)
     * @param str  the CharSequence to check, may be null
     * @param prefix the prefix to find, may be null
     * @param ignoreCase indicates whether the compare should ignore case
     *  (case insensitive) or not.
     * @return {@code true} if the CharSequence starts with the prefix or
     *  both {@code null}
     */
    private static boolean startsWith(final CharSequence str, final CharSequence prefix, final boolean ignoreCase) {
        if (str == null || prefix == null) {
            return str == prefix;
        }
        // Get length once instead of twice in the unlikely case that it changes.
        final int preLen = prefix.length();
        if (preLen > str.length()) {
            return false;
        }
        return CharSequenceUtils.regionMatches(str, ignoreCase, 0, prefix, 0, preLen);
    }

    /**
     * <p>Case insensitive check if a CharSequence starts with a specified prefix.</p>
     *
     * <p>{@code null}s are handled without exceptions. Two {@code null}
     * references are considered to be equal. The comparison is case insensitive.</p>
     *
     * <pre>
     * StringUtils.startsWithIgnoreCase(null, null)      = true
     * StringUtils.startsWithIgnoreCase(null, "abc")     = false
     * StringUtils.startsWithIgnoreCase("abcdef", null)  = false
     * StringUtils.startsWithIgnoreCase("abcdef", "abc") = true
     * StringUtils.startsWithIgnoreCase("ABCDEF", "abc") = true
     * </pre>
     *
     * @see java.lang.String#startsWith(String)
     * @param str  the CharSequence to check, may be null
     * @param prefix the prefix to find, may be null
     * @return {@code true} if the CharSequence starts with the prefix, case insensitive, or
     *  both {@code null}
     * @since 2.4
     * @since 3.0 Changed signature from startsWithIgnoreCase(String, String) to startsWithIgnoreCase(CharSequence, CharSequence)
     */
    public static boolean startsWithIgnoreCase(final CharSequence str, final CharSequence prefix) {
        return startsWith(str, prefix, true);
    }

    /**
     * <p>Strips whitespace from the start and end of a String.</p>
     *
     * <p>This is similar to {@link #trim(String)} but removes whitespace.
     * Whitespace is defined by {@link Character#isWhitespace(char)}.</p>
     *
     * <p>A {@code null} input String returns {@code null}.</p>
     *
     * <pre>
     * StringUtils.strip(null)     = null
     * StringUtils.strip("")       = ""
     * StringUtils.strip("   ")    = ""
     * StringUtils.strip("abc")    = "abc"
     * StringUtils.strip("  abc")  = "abc"
     * StringUtils.strip("abc  ")  = "abc"
     * StringUtils.strip(" abc ")  = "abc"
     * StringUtils.strip(" ab c ") = "ab c"
     * </pre>
     *
     * @param str  the String to remove whitespace from, may be null
     * @return the stripped String, {@code null} if null String input
     */
    public static String strip(final String str) {
        return strip(str, null);
    }

    /**
     * <p>Strips any of a set of characters from the start and end of a String.
     * This is similar to {@link String#trim()} but allows the characters
     * to be stripped to be controlled.</p>
     *
     * <p>A {@code null} input String returns {@code null}.
     * An empty string ("") input returns the empty string.</p>
     *
     * <p>If the stripChars String is {@code null}, whitespace is
     * stripped as defined by {@link Character#isWhitespace(char)}.
     *
     * <pre>
     * StringUtils.strip(null, *)          = null
     * StringUtils.strip("", *)            = ""
     * StringUtils.strip("abc", null)      = "abc"
     * StringUtils.strip("  abc", null)    = "abc"
     * StringUtils.strip("abc  ", null)    = "abc"
     * StringUtils.strip(" abc ", null)    = "abc"
     * StringUtils.strip("  abcyx", "xyz") = "  abc"
     * </pre>
     *
     * @param str  the String to remove characters from, may be null
     * @param stripChars  the characters to remove, null treated as whitespace
     * @return the stripped String, {@code null} if null String input
     */
    public static String strip(String str, final String stripChars) {
        str = stripStart(str, stripChars);
        return stripEnd(str, stripChars);
    }

    /**
     * <p>Strips any of a set of characters from the end of a String.</p>
     *
     * <p>A {@code null} input String returns {@code null}.
     * An empty string ("") input returns the empty string.</p>
     *
     * <p>If the stripChars String is {@code null}, whitespace is
     * stripped as defined by {@link Character#isWhitespace(char)}.</p>
     *
     * <pre>
     * StringUtils.stripEnd(null, *)          = null
     * StringUtils.stripEnd("", *)            = ""
     * StringUtils.stripEnd("abc", "")        = "abc"
     * StringUtils.stripEnd("abc", null)      = "abc"
     * StringUtils.stripEnd("  abc", null)    = "  abc"
     * StringUtils.stripEnd("abc  ", null)    = "abc"
     * StringUtils.stripEnd(" abc ", null)    = " abc"
     * StringUtils.stripEnd("  abcyx", "xyz") = "  abc"
     * StringUtils.stripEnd("120.00", ".0")   = "12"
     * </pre>
     *
     * @param str  the String to remove characters from, may be null
     * @param stripChars  the set of characters to remove, null treated as whitespace
     * @return the stripped String, {@code null} if null String input
     */
    public static String stripEnd(final String str, final String stripChars) {
        int end = length(str);
        if (end == 0) {
            return str;
        }

        if (stripChars == null) {
            while (end != 0 && Character.isWhitespace(str.charAt(end - 1))) {
                end--;
            }
        } else if (stripChars.isEmpty()) {
            return str;
        } else {
            while (end != 0 && stripChars.indexOf(str.charAt(end - 1)) != INDEX_NOT_FOUND) {
                end--;
            }
        }
        return str.substring(0, end);
    }

    /**
     * <p>Strips any of a set of characters from the start of a String.</p>
     *
     * <p>A {@code null} input String returns {@code null}.
     * An empty string ("") input returns the empty string.</p>
     *
     * <p>If the stripChars String is {@code null}, whitespace is
     * stripped as defined by {@link Character#isWhitespace(char)}.</p>
     *
     * <pre>
     * StringUtils.stripStart(null, *)          = null
     * StringUtils.stripStart("", *)            = ""
     * StringUtils.stripStart("abc", "")        = "abc"
     * StringUtils.stripStart("abc", null)      = "abc"
     * StringUtils.stripStart("  abc", null)    = "abc"
     * StringUtils.stripStart("abc  ", null)    = "abc  "
     * StringUtils.stripStart(" abc ", null)    = "abc "
     * StringUtils.stripStart("yxabc  ", "xyz") = "abc  "
     * </pre>
     *
     * @param str  the String to remove characters from, may be null
     * @param stripChars  the characters to remove, null treated as whitespace
     * @return the stripped String, {@code null} if null String input
     */
    public static String stripStart(final String str, final String stripChars) {
        final int strLen = length(str);
        if (strLen == 0) {
            return str;
        }
        int start = 0;
        if (stripChars == null) {
            while (start != strLen && Character.isWhitespace(str.charAt(start))) {
                start++;
            }
        } else if (stripChars.isEmpty()) {
            return str;
        } else {
            while (start != strLen && stripChars.indexOf(str.charAt(start)) != INDEX_NOT_FOUND) {
                start++;
            }
        }
        return str.substring(start);
    }

    /**
     * <p>Gets a substring from the specified String avoiding exceptions.</p>
     *
     * <p>A negative start position can be used to start/end {@code n}
     * characters from the end of the String.</p>
     *
     * <p>The returned substring starts with the character in the {@code start}
     * position and ends before the {@code end} position. All position counting is
     * zero-based -- i.e., to start at the beginning of the string use
     * {@code start = 0}. Negative start and end positions can be used to
     * specify offsets relative to the end of the String.</p>
     *
     * <p>If {@code start} is not strictly to the left of {@code end}, ""
     * is returned.</p>
     *
     * <pre>
     * StringUtils.substring(null, *, *)    = null
     * StringUtils.substring("", * ,  *)    = "";
     * StringUtils.substring("abc", 0, 2)   = "ab"
     * StringUtils.substring("abc", 2, 0)   = ""
     * StringUtils.substring("abc", 2, 4)   = "c"
     * StringUtils.substring("abc", 4, 6)   = ""
     * StringUtils.substring("abc", 2, 2)   = ""
     * StringUtils.substring("abc", -2, -1) = "b"
     * StringUtils.substring("abc", -4, 2)  = "ab"
     * </pre>
     *
     * @param str  the String to get the substring from, may be null
     * @param start  the position to start from, negative means
     *  count back from the end of the String by this many characters
     * @param end  the position to end at (exclusive), negative means
     *  count back from the end of the String by this many characters
     * @return substring from start position to end position,
     *  {@code null} if null String input
     */
    public static String substring(final String str, int start, int end) {
        if (str == null) {
            return null;
        }

        // handle negatives
        if (end < 0) {
            end = str.length() + end; // remember end is negative
        }
        if (start < 0) {
            start = str.length() + start; // remember start is negative
        }

        // check length next
        if (end > str.length()) {
            end = str.length();
        }

        // if start is greater than end, return ""
        if (start > end) {
            return EMPTY;
        }

        if (start < 0) {
            start = 0;
        }
        if (end < 0) {
            end = 0;
        }

        return str.substring(start, end);
    }

    private static String toStringOrEmpty(final Object obj) {
        return Objects.toString(obj, EMPTY);
    }

    /**
     * <p>Removes control characters (char &lt;= 32) from both
     * ends of this String, handling {@code null} by returning
     * {@code null}.</p>
     *
     * <p>The String is trimmed using {@link String#trim()}.
     * Trim removes start and end characters &lt;= 32.</p>
     *
     * <p>To trim your choice of characters, use the
     * {@link #strip(String, String)} methods.</p>
     *
     * <pre>
     * StringUtils.trim(null)          = null
     * StringUtils.trim("")            = ""
     * StringUtils.trim("     ")       = ""
     * StringUtils.trim("abc")         = "abc"
     * StringUtils.trim("    abc    ") = "abc"
     * </pre>
     *
     * @param str  the String to be trimmed, may be null
     * @return the trimmed string, {@code null} if null String input
     */
    public static String trim(final String str) {
        return str == null ? null : str.trim();
    }

    /**
     * <p>Uncapitalizes a String, changing the first character to lower case as
     * per {@link Character#toLowerCase(int)}. No other characters are changed.</p>
     *
     * <pre>
     * StringUtils.uncapitalize(null)  = null
     * StringUtils.uncapitalize("")    = ""
     * StringUtils.uncapitalize("cat") = "cat"
     * StringUtils.uncapitalize("Cat") = "cat"
     * StringUtils.uncapitalize("CAT") = "cAT"
     * </pre>
     *
     * @param str the String to uncapitalize, may be null
     * @return the uncapitalized String, {@code null} if null String input
     * @see #capitalize(String)
     * @since 2.0
     */
    public static String uncapitalize(final String str) {
        final int strLen = length(str);
        if (strLen == 0) {
            return str;
        }

        final int firstCodepoint = str.codePointAt(0);
        final int newCodePoint = Character.toLowerCase(firstCodepoint);
        if (firstCodepoint == newCodePoint) {
            // already capitalized
            return str;
        }

        final int[] newCodePoints = new int[strLen]; // cannot be longer than the char array
        int outOffset = 0;
        newCodePoints[outOffset++] = newCodePoint; // copy the first codepoint
        for (int inOffset = Character.charCount(firstCodepoint); inOffset < strLen; ) {
            final int codepoint = str.codePointAt(inOffset);
            newCodePoints[outOffset++] = codepoint; // copy the remaining ones
            inOffset += Character.charCount(codepoint);
        }
        return new String(newCodePoints, 0, outOffset);
    }

    /**
     * <p>Converts a String to upper case as per {@link String#toUpperCase()}.</p>
     *
     * <p>A {@code null} input String returns {@code null}.</p>
     *
     * <pre>
     * StringUtils.upperCase(null)  = null
     * StringUtils.upperCase("")    = ""
     * StringUtils.upperCase("aBc") = "ABC"
     * </pre>
     *
     * <p><strong>Note:</strong> As described in the documentation for {@link String#toUpperCase()},
     * the result of this method is affected by the current locale.
     *
     * @param str  the String to upper case, may be null
     * @return the upper cased String, {@code null} if null String input
     */
    public static String upperCase(final String str) {
        if (str == null) {
            return null;
        }
        return str.toUpperCase();
    }

}

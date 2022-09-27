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

import java.util.ArrayList;
import java.util.List;

import static com.octomix.josson.commons.StringEscapeUtils.*;

/**
 * To unescape placeholders and escape the fill-in text for different markup language.
 * {@link #NONE}
 * {@link #XML}
 * {@link #HTML}
 */
public enum MarkupLanguage {

    /**
     * Plain text, escaping operation is not required.
     */
    NONE(' ', ' '),

    /**
     * XML.
     */
    XML('<', '>'),

    /**
     * HTML.
     */
    HTML('<', '>');

    final char tagOpen;

    final char tagClose;

    MarkupLanguage(char tagOpen, char tagClose) {
        this.tagOpen = tagOpen;
        this.tagClose = tagClose;
    }

    /**
     * <p>Whether escaping is applicable.</p>
     *
     * @return {@code false} for {@code NONE}, otherwise returns {@code true}
     */
    public boolean isEscapingApplicable() {
        return this != NONE;
    }

    /**
     * <p>Returns the escaped content.</p>
     *
     * @param content the content to be escaped
     * @return The escaped content
     */
    public String escape(String content) {
        switch (this) {
            case XML:
                return escapeXml11(content);
            case HTML:
                return escapeHtml4(content);
            default:
                return content;
        }
    }

    /**
     * <p>Returns the unescaped content.</p>
     *
     * @param content the content to be unescaped
     * @return The unescaped content
     */
    public String unescape(String content) {
        switch (this) {
            case XML:
                return unescapeXml(content);
            case HTML:
                return unescapeHtml4(content);
            default:
                return content;
        }
    }

    public List<String> separateTagAndText(final String content) {
        final List<String> tokens = new ArrayList<>();
        final int len = content.length();
        int text = 0;
        int tag = -1;
        for (int pos = 0; pos < len; pos++) {
            final char tagMark = content.charAt(pos);
            if (tagMark == tagOpen) {
                if (text >= 0) {
                    if (text < pos) {
                        tokens.add(content.substring(text, pos));
                    }
                    text = -1;
                    tag = pos;
                }
            } else if (tagMark == tagClose && tag >= 0) {
                pos++;
                if (pos == len || content.charAt(pos) != tagOpen) {
                    tokens.add(content.substring(tag, pos));
                    tag = -1;
                    text = pos;
                }
            }
        }
        if (text >= 0 && text < len) {
            tokens.add(content.substring(text, len));
        }
        return tokens;
    }
}

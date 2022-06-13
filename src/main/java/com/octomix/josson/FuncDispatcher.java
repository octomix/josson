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
import com.octomix.josson.commons.StringUtils;

import java.time.temporal.ChronoField;
import java.time.temporal.ChronoUnit;
import java.util.Base64;

import static com.octomix.josson.FuncArithmetic.*;
import static com.octomix.josson.FuncArray.*;
import static com.octomix.josson.FuncDate.*;
import static com.octomix.josson.FuncFormat.*;
import static com.octomix.josson.FuncLogical.*;
import static com.octomix.josson.FuncString.*;
import static com.octomix.josson.FuncStructural.*;

/**
 * Function dispatcher.
 */
class FuncDispatcher {

    private final String funcName;

    private final String params;

    FuncDispatcher(final String funcName, final String params) {
        this.funcName = funcName;
        this.params = params;
    }

    private class UnsupportedFunctionException extends UnsupportedOperationException {
        UnsupportedFunctionException() {
            super(String.format("Unsupported function %s()", funcName));
        }
    }

    JsonNode apply(final JsonNode node) {
        try {
            String func = funcName.toLowerCase();
            switch (func.charAt(0)) {
                case 'a': return applyA(node, func);
                case 'b': return applyB(node, func);
                case 'c': return applyC(node, func);
                case 'd': return applyD(node, func);
                case 'e': return applyE(node, func);
                case 'f': return applyF(node, func);
                case 'g': return applyG(node, func);
                case 'h': return applyH(node, func);
                case 'i': return applyI(node, func);
                case 'j': return applyJ(node, func);
                case 'k': return applyK(node, func);
                case 'l': return applyL(node, func);
                case 'm': return applyM(node, func);
                case 'n': return applyN(node, func);
                case 'o': return applyO(node, func);
                case 'p': return applyP(node, func);
                case 'q': return applyQ(node, func);
                case 'r': return applyR(node, func);
                case 's': return applyS(node, func);
                case 't': return applyT(node, func);
                case 'u': return applyU(node, func);
                case 'w': return applyW(node, func);
                case 'y': return applyY(node, func);
            }
        } catch (IllegalArgumentException e) {
            throw new IllegalArgumentException(
                    String.format("Invalid function call %s() : %s", funcName, e.getMessage()));
        }
        throw new UnsupportedFunctionException();
    }

    private JsonNode applyA(final JsonNode node, final String func) {
        switch (func) {
            // Arithmetic
            case "abs":
                return funcAbs(node, params);
            // Array
            case "avg":
                return funcAggregate(node, func, params);
            // Date
            case "ampmofday":
                return funcAmPmOfDay(node, params);
            // String
            case "abbreviate":
                return funcAbbreviate(node, params);
            case "append":
                return funcAppend(node, params);
            case "appendifmissing":
                return funcAppendIfMissing(node, params, false);
            case "appendifmissingignorecase":
                return funcAppendIfMissing(node, params, true);
        }
        throw new UnsupportedFunctionException();
    }

    private JsonNode applyB(final JsonNode node, final String func) {
        switch (func) {
            // Format
            case "b64decode":
                return funcB64Decode(node, params, Base64.getDecoder());
            case "b64mimedecode":
                return funcB64Decode(node, params, Base64.getMimeDecoder());
            case "b64urldecode":
                return funcB64Decode(node, params, Base64.getUrlDecoder());
            case "b64encode":
                return funcB64Encode(node, params, Base64.getEncoder());
            case "b64encodenopadding":
                return funcB64Encode(node, params, Base64.getEncoder().withoutPadding());
            case "b64mimeencode":
                return funcB64Encode(node, params, Base64.getMimeEncoder());
            case "b64mimeencodenopadding":
                return funcB64Encode(node, params, Base64.getMimeEncoder().withoutPadding());
            case "b64urlencode":
                return funcB64Encode(node, params, Base64.getUrlEncoder());
            case "b64urlencodenopadding":
                return funcB64Encode(node, params, Base64.getUrlEncoder().withoutPadding());
        }
        throw new UnsupportedFunctionException();
    }

    private JsonNode applyC(final JsonNode node, final String func) {
        switch (func) {
            // Arithmetic
            case "calc":
                return funcCalc(node, params);
            case "ceil":
                return funcCeil(node, params);
            // Array
            case "count":
                return funcAggregate(node, func, params);
            // Format
            case "casevalue":
                return funcCaseValue(node, params);
            case "csv":
                return funcCsv(node, params, false);
            case "csvshownull":
                return funcCsv(node, params, true);
            case "cyclevalue":
                return funcCycleValue(node, params);
            // Logical
            case "contains":
                return funcContains(node, params, false, false);
            case "containsignorecase":
                return funcContains(node, params, true, false);
            // String
            case "capitalize":
                return funcCapitalize(node, params);
            case "center":
                return funcPadding(node, params, 0);
            case "concat":
                return funcConcat(node, params, true);
            case "concatfree":
                return funcConcat(node, params, false);
            // Structural
            case "coalesce":
                return funcCoalesce(node, params);
        }
        throw new UnsupportedFunctionException();
    }

    private JsonNode applyD(final JsonNode node, final String func) {
        switch (func) {
            // Array
            case "distinct":
                return funcDistinct(node, params);
            // Date
            case "dayofweek":
                return funcChronometry(node, params, ChronoField.DAY_OF_WEEK);
            case "day":
                return funcChronometry(node, params, ChronoField.DAY_OF_MONTH);
            case "dayofyear":
                return funcChronometry(node, params, ChronoField.DAY_OF_YEAR);
            case "dayend":
                return funcDayEnd(node, params);
            // Format
            case "default":
                return funcDefault(node, params);
            // String
            case "doublequote":
                return funcDoubleQuote(node, params);
        }
        throw new UnsupportedFunctionException();
    }

    private JsonNode applyE(final JsonNode node, final String func) {
        switch (func) {
            // Logical
            case "endswith":
                return funcEndsWith(node, params, false, false);
            case "endswithignorecase":
                return funcEndsWith(node, params, true, false);
            case "equals":
                return funcEquals(node, params, false, false);
            case "equalsignorecase":
                return funcEquals(node, params, true, false);
            // Structural
            case "entries":
                return funcEntries(node, params);
        }
        throw new UnsupportedFunctionException();
    }

    private JsonNode applyF(final JsonNode node, final String func) {
        switch (func) {
            // Arithmetic
            case "floor":
                return funcFloor(node, params);
            // Array
            case "findbymax":
                return funcFindByMaxMin(node, params, true, 0);
            case "findbymaxornull":
                return funcFindByMaxMin(node, params, true, -1);
            case "findbymin":
                return funcFindByMaxMin(node, params, false, 0);
            case "findbyminornull":
                return funcFindByMaxMin(node, params, false, -1);
            case "findbynullormax":
                return funcFindByMaxMin(node, params, true, 1);
            case "findbynullormin":
                return funcFindByMaxMin(node, params, false, 1);
            case "first":
                return funcFirst(node, params);
            // Format
            case "formatdate":
                return funcFormatDate(node, params);
            case "formatnumber":
                return funcFormatNumber(node, params);
            case "formattext":
                return funcFormatText(node, params);
            case "formattexts":
                return funcFormatTexts(node, params);
            // Structural
            case "field":
                return funcField(node, params);
            case "flatten":
                return funcFlatten(node, params);
        }
        throw new UnsupportedFunctionException();
    }

    private JsonNode applyG(final JsonNode node, final String func) {
        // Structural
        if ("group".equals(func)) {
            return funcGroup(node, params);
        }
        throw new UnsupportedFunctionException();
    }

    private JsonNode applyH(final JsonNode node, final String func) {
        switch (func) {
            // Date
            case "hourofampm":
                return funcChronometry(node, params, ChronoField.HOUR_OF_AMPM);
            case "hour":
                return funcChronometry(node, params, ChronoField.HOUR_OF_DAY);
        }
        throw new UnsupportedFunctionException();
    }

    private JsonNode applyI(final JsonNode node, final String func) {
        switch (func) {
            // Array
            case "indexof":
                return funcIndexOf(node, params, 1);
            // Format
            case "if":
                return funcIf(node, params);
            case "indexedvalue":
                return funcIndexedValue(node, params);
            // Logical
            case "in":
                return funcIn(node, params, false, false);
            case "inignorecase":
                return funcIn(node, params, true, false);
            case "isblank":
                return funcIsBlank(node, params, false);
            case "isnotblank":
                return funcIsBlank(node, params, true);
            case "isboolean":
                return funcIsBoolean(node, params);
            case "isempty":
                return funcIsEmpty(node, params, false);
            case "isnotempty":
                return funcIsEmpty(node, params, true);
            case "iseven":
                return funcIsEven(node, params);
            case "isnull":
                return funcIsNull(node, params, false);
            case "isnotnull":
                return funcIsNull(node, params, true);
            case "isnumber":
                return funcIsNumber(node, params);
            case "isodd":
                return funcIsOdd(node, params);
            case "istext":
                return funcIsText(node, params);
            case "isweekday":
                return funcIsWeekday(node, params);
            case "isweekend":
                return funcIsWeekend(node, params);
            case "isleapyear":
                return funcIsLeapYear(node, params);
        }
        throw new UnsupportedFunctionException();
    }

    private JsonNode applyJ(final JsonNode node, final String func) {
        switch (func) {
            // Array
            case "join":
                return funcJoin(node, params);
            // Structural
            case "json":
                return funcJson(node, params);
        }
        throw new UnsupportedFunctionException();
    }

    private JsonNode applyK(final JsonNode node, final String func) {
        switch (func) {
            // String
            case "keepafter":
                return funcKeep(node, params, false, true, false);
            case "keepafterignorecase":
                return funcKeep(node, params, true, true, false);
            case "keepafterlast":
                return funcKeep(node, params, false, true, true);
            case "keepafterlastignorecase":
                return funcKeep(node, params, true, true, true);
            case "keepbefore":
                return funcKeep(node, params, false, false, false);
            case "keepbeforeignorecase":
                return funcKeep(node, params, true, false, false);
            case "keepbeforelast":
                return funcKeep(node, params, false, false, true);
            case "keepbeforelastignorecase":
                return funcKeep(node, params, true, false, true);
            // Structural
            case "keys":
                return funcKeys(node, params);
        }
        throw new UnsupportedFunctionException();
    }

    private JsonNode applyL(final JsonNode node, final String func) {
        switch (func) {
            // Array
            case "last":
                return funcLast(node, params);
            case "lastindex":
                return funcLastIndex(node, params);
            case "lastindexof":
                return funcIndexOf(node, params, -1);
            // Date
            case "lengthofmonth":
                return funcLengthOfMonth(node, params);
            case "lengthofyear":
                return funcLengthOfYear(node, params);
            case "localtooffsetdate":
                return funcLocalToOffsetDate(node, params);
            // String
            case "leftpad":
                return funcPadding(node, params, -1);
            case "length":
                return funcLength(node, params);
            case "lowercase":
                return funcLowerCase(node, params);
        }
        throw new UnsupportedFunctionException();
    }

    private JsonNode applyM(final JsonNode node, final String func) {
        switch (func) {
            // Arithmetic
            case "max":
                return funcMaxMin(node, params, true);
            case "min":
                return funcMaxMin(node, params, false);
            case "mod":
                return funcMod(node, params);
            // Date
            case "minute":
                return funcChronometry(node, params, ChronoField.MINUTE_OF_HOUR);
            case "minuteofday":
                return funcChronometry(node, params, ChronoField.MINUTE_OF_DAY);
            case "month":
                return funcChronometry(node, params, ChronoField.MONTH_OF_YEAR);
            case "minusseconds":
                return funcDateMinus(node, params, ChronoUnit.SECONDS);
            case "minusminutes":
                return funcDateMinus(node, params, ChronoUnit.MINUTES);
            case "minushours":
                return funcDateMinus(node, params, ChronoUnit.HOURS);
            case "minusdays":
                return funcDateMinus(node, params, ChronoUnit.DAYS);
            case "minusweeks":
                return funcDateMinus(node, params, ChronoUnit.WEEKS);
            case "minusmonths":
                return funcDateMinus(node, params, ChronoUnit.MONTHS);
            case "minusyears":
                return funcDateMinus(node, params, ChronoUnit.YEARS);
            case "monthend":
                return funcMonthEnd(node, params);
            // Logical
            case "matches":
                return funcMatches(node, params, false);
            // Structural
            case "map":
                return funcMap(node, params);
        }
        throw new UnsupportedFunctionException();
    }

    private JsonNode applyN(final JsonNode node, final String func) {
        switch (func) {
            // Logical
            case "notcontains":
                return funcContains(node, params, false, true);
            case "notcontainsignorecase":
                return funcContains(node, params, true, true);
            case "notendswith":
                return funcEndsWith(node, params, false, true);
            case "notendswithignorecase":
                return funcEndsWith(node, params, true, true);
            case "notequals":
                return funcEquals(node, params, false, true);
            case "notequalsignorecase":
                return funcEquals(node, params, true, true);
            case "notin":
                return funcIn(node, params, false, true);
            case "notinignorecase":
                return funcIn(node, params, true, true);
            case "not":
                return funcNot(node, params);
            case "notmatches":
                return funcMatches(node, params, true);
            case "notstartswith":
                return funcStartsWith(node, params, false, true);
            case "notstartswithignorecase":
                return funcStartsWith(node, params, true, true);
            // String
            case "notblank":
                return funcNotBlankOrEmpty(node, params, StringUtils::isNotBlank);
            case "notempty":
                return funcNotBlankOrEmpty(node, params, StringUtils::isNotEmpty);
        }
        throw new UnsupportedFunctionException();
    }

    private JsonNode applyO(final JsonNode node, final String func) {
        // Date
        if ("offsettolocaldate".equals(func)) {
            return funcOffsetToLocalDate(node, params);
        }
        throw new UnsupportedFunctionException();
    }

    private JsonNode applyP(final JsonNode node, final String func) {
        switch (func) {
            // Date
            case "plusseconds":
                return funcDatePlus(node, params, ChronoUnit.SECONDS);
            case "plusminutes":
                return funcDatePlus(node, params, ChronoUnit.MINUTES);
            case "plushours":
                return funcDatePlus(node, params, ChronoUnit.HOURS);
            case "plusdays":
                return funcDatePlus(node, params, ChronoUnit.DAYS);
            case "plusweeks":
                return funcDatePlus(node, params, ChronoUnit.WEEKS);
            case "plusmonths":
                return funcDatePlus(node, params, ChronoUnit.MONTHS);
            case "plusyears":
                return funcDatePlus(node, params, ChronoUnit.YEARS);
            // String
            case "prepend":
                return funcPrepend(node, params);
            case "prependifmissing":
                return funcPrependIfMissing(node, params, false);
            case "prependifmissingignorecase":
                return funcPrependIfMissing(node, params, true);
        }
        throw new UnsupportedFunctionException();
    }

    private JsonNode applyQ(final JsonNode node, final String func) {
        // String
        if (func.equals("quote")) {
            return funcSingleQuote(node, params);
        }
        throw new UnsupportedFunctionException();
    }

    private JsonNode applyR(final JsonNode node, final String func) {
        switch (func) {
            // Arithmetic
            case "round":
                return funcRound(node, params);
            // Array
            case "reverse":
                return funcReverse(node, params);
            // String
            case "removeend":
                return funcRemoveEnd(node, params, false);
            case "removeendignorecase":
                return funcRemoveEnd(node, params, true);
            case "removestart":
                return funcRemoveStart(node, params, false);
            case "removestartignorecase":
                return funcRemoveStart(node, params, true);
            case "repeat":
                return funcRepeat(node, params);
            case "replace":
                return funcReplace(node, params, false);
            case "replaceignorecase":
                return funcReplace(node, params, true);
            case "rightpad":
                return funcPadding(node, params, 1);
        }
        throw new UnsupportedFunctionException();
    }

    private JsonNode applyS(final JsonNode node, final String func) {
        switch (func) {
            // Array
            case "size":
                return funcSize(node, params);
            case "slice":
                return funcSlice(node, params);
            case "sort":
                return funcSort(node, params);
            case "sum":
                return funcAggregate(node, func, params);
            // Date
            case "second":
                return funcChronometry(node, params, ChronoField.SECOND_OF_MINUTE);
            case "secondofday":
                return funcChronometry(node, params, ChronoField.SECOND_OF_DAY);
            // Logical
            case "startswith":
                return funcStartsWith(node, params, false, false);
            case "startswithignorecase":
                return funcStartsWith(node, params, true, false);
            // String
            case "singlequote":
                return funcSingleQuote(node, params);
            case "split":
                return funcSplit(node, params);
            case "strip":
                return funcStrip(node, params);
            case "stripend":
                return funcStripEnd(node, params);
            case "stripstart":
                return funcStripStart(node, params);
            case "substr":
                return funcSubstr(node, params);
        }
        throw new UnsupportedFunctionException();
    }

    private JsonNode applyT(final JsonNode node, final String func) {
        switch (func) {
            // Date
            case "truncatetomicro":
                return funcDateTruncateTo(node, params, ChronoUnit.MICROS);
            case "truncatetomilli":
                return funcDateTruncateTo(node, params, ChronoUnit.MILLIS);
            case "truncatetosecond":
                return funcDateTruncateTo(node, params, ChronoUnit.SECONDS);
            case "truncatetominute":
                return funcDateTruncateTo(node, params, ChronoUnit.MINUTES);
            case "truncatetohour":
                return funcDateTruncateTo(node, params, ChronoUnit.HOURS);
            case "truncatetoday":
                return funcDateTruncateTo(node, params, ChronoUnit.DAYS);
            case "truncatetomonth":
                return funcDateTruncateToMonth(node, params);
            case "truncatetoyear":
                return funcDateTruncateToYear(node, params);
            // Format
            case "tonumber":
                return funcToNumber(node, params);
            case "tostring":
                return funcToString(node, params);
            case "totext":
                return funcToText(node, params);
            // String
            case "trim":
                return funcTrim(node, params);
            // Structural
            case "toarray":
                return funcToArray(node, params);
        }
        throw new UnsupportedFunctionException();
    }

    private JsonNode applyU(final JsonNode node, final String func) {
        switch (func) {
            // Date
            case "untilinsecond":
                return funcUntil(node, params, ChronoUnit.SECONDS);
            case "untilinminute":
                return funcUntil(node, params, ChronoUnit.MINUTES);
            case "untilinhour":
                return funcUntil(node, params, ChronoUnit.HOURS);
            case "untilinday":
                return funcUntil(node, params, ChronoUnit.DAYS);
            case "untilinmonth":
                return funcUntil(node, params, ChronoUnit.MONTHS);
            case "untilinyear":
                return funcUntil(node, params, ChronoUnit.YEARS);
            // Format
            case "urldecode":
                return funcUrlDecode(node, params);
            case "urlencode":
                return funcUrlEncode(node, params);
            // String
            case "uncapitalize":
                return funcUncapitalize(node, params);
            case "uppercase":
                return funcUpperCase(node, params);
        }
        throw new UnsupportedFunctionException();
    }

    private JsonNode applyW(final JsonNode node, final String func) {
        switch (func) {
            // Date
            case "withnano":
                return funcDateWith(node, params, ChronoField.NANO_OF_SECOND);
            case "withmicro":
                return funcDateWith(node, params, ChronoField.MICRO_OF_SECOND);
            case "withmilli":
                return funcDateWith(node, params, ChronoField.MILLI_OF_SECOND);
            case "withsecond":
                return funcDateWith(node, params, ChronoField.SECOND_OF_MINUTE);
            case "withminute":
                return funcDateWith(node, params, ChronoField.MINUTE_OF_HOUR);
            case "withhour":
                return funcDateWith(node, params, ChronoField.HOUR_OF_DAY);
            case "withday":
                return funcDateWith(node, params, ChronoField.DAY_OF_MONTH);
            case "withdayofyear":
                return funcDateWith(node, params, ChronoField.DAY_OF_YEAR);
            case "withmonth":
                return funcDateWith(node, params, ChronoField.MONTH_OF_YEAR);
            case "withyear":
                return funcDateWith(node, params, ChronoField.YEAR);
        }
        throw new UnsupportedFunctionException();
    }

    private JsonNode applyY(final JsonNode node, final String func) {
        switch (func) {
            // Date
            case "year":
                return funcChronometry(node, params, ChronoField.YEAR);
            case "yearend":
                return funcYearEnd(node, params);
        }
        throw new UnsupportedFunctionException();
    }
}

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

import com.fasterxml.jackson.databind.JsonNode;

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

class FuncDispatcher {
    static JsonNode dispatch(JsonNode node, String funcId, String params) {
        try {
            switch (funcId.toLowerCase()) {

                // Arithmetic
                case "sum":
                case "avg":
                case "count":
                    return funcAggregate(node, funcId, params);
                case "abs":
                    return funcAbs(node, params);
                case "calc":
                    return funcCalc(node, params);
                case "ceil":
                    return funcCeil(node, params);
                case "floor":
                    return funcFloor(node, params);
                case "mod":
                    return funcMod(node, params);
                case "round":
                    return funcRound(node, params);

                // Array
                case "distinctvalue":
                    return funcDistinctValue(node, params);
                case "first":
                    return funcFirst(node, params);
                case "indexof":
                    return funcIndexOf(node, params);
                case "last":
                    return funcLast(node, params);
                case "lastindex":
                    return funcLastIndex(node, params);
                case "lastindexof":
                    return funcLastIndexOf(node, params);
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
                case "max":
                    return funcMaxMin(node, params, true);
                case "min":
                    return funcMaxMin(node, params, false);
                case "reverse":
                    return funcReverse(node, params);
                case "slice":
                    return funcSlice(node, params);
                case "sort":
                    return funcSort(node, params);

                // Date
                case "ampmofday":
                    return funcAmPmOfDay(node, params);
                case "second":
                    return funcChronometry(node, params, ChronoField.SECOND_OF_MINUTE);
                case "secondofday":
                    return funcChronometry(node, params, ChronoField.SECOND_OF_DAY);
                case "minute":
                    return funcChronometry(node, params, ChronoField.MINUTE_OF_HOUR);
                case "minuteofday":
                    return funcChronometry(node, params, ChronoField.MINUTE_OF_DAY);
                case "hourofampm":
                    return funcChronometry(node, params, ChronoField.HOUR_OF_AMPM);
                case "hour":
                    return funcChronometry(node, params, ChronoField.HOUR_OF_DAY);
                case "dayofweek":
                    return funcChronometry(node, params, ChronoField.DAY_OF_WEEK);
                case "day":
                    return funcChronometry(node, params, ChronoField.DAY_OF_MONTH);
                case "dayofyear":
                    return funcChronometry(node, params, ChronoField.DAY_OF_YEAR);
                case "month":
                    return funcChronometry(node, params, ChronoField.MONTH_OF_YEAR);
                case "year":
                    return funcChronometry(node, params, ChronoField.YEAR);
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
                case "dayend":
                    return funcDayEnd(node, params);
                case "monthend":
                    return funcMonthEnd(node, params);
                case "yearend":
                    return funcYearEnd(node, params);
                case "lengthofmonth":
                    return funcLengthOfMonth(node, params);
                case "lengthofyear":
                    return funcLengthOfYear(node, params);
                case "localtooffsetdate":
                    return funcLocalToOffsetDate(node, params);
                case "offsettolocaldate":
                    return funcOffsetToLocalDate(node, params);

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
                case "casevalue":
                    return funcCaseValue(node, params);
                case "cyclevalue":
                    return funcCycleValue(node, params);
                case "formatdate":
                    return funcFormatDate(node, params);
                case "formatnumber":
                    return funcFormatNumber(node, params);
                case "formattext":
                    return funcFormatText(node, params);
                case "formattexts":
                    return funcFormatTexts(node, params);
                case "indexedvalue":
                    return funcIndexedValue(node, params);
                case "tonumber":
                    return funcToNumber(node, params);
                case "totext":
                    return funcToText(node, params);
                case "urldecode":
                    return funcUrlDecode(node, params);
                case "urlencode":
                    return funcUrlEncode(node, params);

                    // Logical
                case "contains":
                    return funcContains(node, params, false, false);
                case "containsignorecase":
                    return funcContains(node, params, true, false);
                case "notcontains":
                    return funcContains(node, params, false, true);
                case "notcontainsignorecase":
                    return funcContains(node, params, true, true);
                case "endswith":
                    return funcEndsWith(node, params, false, false);
                case "endswithignorecase":
                    return funcEndsWith(node, params, true, false);
                case "notendswith":
                    return funcEndsWith(node, params, false, true);
                case "notendswithignorecase":
                    return funcEndsWith(node, params, true, true);
                case "equals":
                    return funcEquals(node, params, false, false);
                case "equalsignorecase":
                    return funcEquals(node, params, true, false);
                case "notequals":
                    return funcEquals(node, params, false, true);
                case "notequalsignorecase":
                    return funcEquals(node, params, true, true);
                case "in":
                    return funcIn(node, params, false, false);
                case "inignorecase":
                    return funcIn(node, params, true, false);
                case "notin":
                    return funcIn(node, params, false, true);
                case "notinignorecase":
                    return funcIn(node, params, true, true);
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
                case "not":
                    return funcNot(node, params);
                case "startswith":
                    return funcStartsWith(node, params, false, false);
                case "startswithignorecase":
                    return funcStartsWith(node, params, true, false);
                case "notstartswith":
                    return funcStartsWith(node, params, false, true);
                case "notstartswithignorecase":
                    return funcStartsWith(node, params, true, true);
                case "isweekday":
                    return funcIsWeekDay(node, params);
                case "isweekend":
                    return funcIsWeekEnd(node, params);
                case "isleapyear":
                    return funcIsLeapYear(node, params);

                // String
                case "abbreviate":
                    return funcAbbreviate(node, params);
                case "appendifmissing":
                    return funcAppendIfMissing(node, params, false);
                case "appendifmissingignorecase":
                    return funcAppendIfMissing(node, params, true);
                case "capitalize":
                    return funcCapitalize(node, params);
                case "center":
                    return funcCenter(node, params);
                case "concat":
                    return funcConcat(node, params);
                case "csv":
                    return funcCsv(node, params);
                case "join":
                    return funcJoin(node, params);
                case "keepafter":
                    return funcKeepAfter(node, params, false, false);
                case "keepafterignorecase":
                    return funcKeepAfter(node, params, true, false);
                case "keepafterlast":
                    return funcKeepAfter(node, params, false, true);
                case "keepafterlastignorecase":
                    return funcKeepAfter(node, params, true, true);
                case "keepbefore":
                    return funcKeepBefore(node, params, false, false);
                case "keepbeforeignorecase":
                    return funcKeepBefore(node, params, true, false);
                case "keepbeforelast":
                    return funcKeepBefore(node, params, false, true);
                case "keepbeforelastignorecase":
                    return funcKeepBefore(node, params, true, true);
                case "leftpad":
                    return funcLeftPad(node, params);
                case "length":
                    return funcLength(node, params);
                case "lowercase":
                    return funcLowerCase(node, params);
                case "notblank":
                    return funcNotBlank(node, params);
                case "notempty":
                    return funcNotEmpty(node, params);
                case "prependifmissing":
                    return funcPrependIfMissing(node, params, false);
                case "prependifmissingignorecase":
                    return funcPrependIfMissing(node, params, true);
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
                    return funcRightPad(node, params);
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
                case "trim":
                    return funcTrim(node, params);
                case "uncapitalize":
                    return funcUncapitalize(node, params);
                case "uppercase":
                    return funcUpperCase(node, params);

                // Structural
                case "coalesce":
                    return funcCoalesce(node, params);
                case "flatten":
                    return funcFlatten(node, params);
                case "map":
                    return funcMap(node, params);
                case "size":
                    return funcSize(node, params);
                case "toarray":
                    return funcToArray(node, params);
            }
        } catch (IllegalArgumentException e) {
            throw new IllegalArgumentException("Invalid function call " + funcId + "() : " + e.getMessage());
        }
        throw new UnsupportedOperationException("Unsupported function " + funcId + "()");
    }
}

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

import com.octomix.josson.commons.CaseUtils;
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

    PathTrace apply(final PathTrace path) {
        try {
            String func = funcName.toLowerCase();
            switch (func.charAt(0)) {
                case 'a': return applyA(path, func);
                case 'b': return applyB(path, func);
                case 'c': return applyC(path, func);
                case 'd': return applyD(path, func);
                case 'e': return applyE(path, func);
                case 'f': return applyF(path, func);
                case 'g': return applyG(path, func);
                case 'h': return applyH(path, func);
                case 'i': return applyI(path, func);
                case 'j': return applyJ(path, func);
                case 'k': return applyK(path, func);
                case 'l': return applyL(path, func);
                case 'm': return applyM(path, func);
                case 'n': return applyN(path, func);
                case 'o': return applyO(path, func);
                case 'p': return applyP(path, func);
                case 'q': return applyQ(path, func);
                case 'r': return applyR(path, func);
                case 's': return applyS(path, func);
                case 't': return applyT(path, func);
                case 'u': return applyU(path, func);
                case 'w': return applyW(path, func);
                case 'y': return applyY(path, func);
            }
        } catch (IllegalArgumentException e) {
            throw new IllegalArgumentException(
                    String.format("Invalid function call %s() : %s", funcName, e.getMessage()));
        }
        throw new UnsupportedFunctionException();
    }

    private PathTrace applyA(final PathTrace path, final String func) {
        switch (func) {
            // Arithmetic
            case "abs":
                return funcAbs(path, params);
            // Array
            case "avg":
                return funcNumericAggregate(path, func, params);
            // Date
            case "ampmofday":
                return funcAmPmOfDay(path, params);
            // String
            case "abbreviate":
                return funcAbbreviate(path, params);
            case "append":
                return funcAppend(path, params);
            case "appendifmissing":
                return funcAppendIfMissing(path, params, false);
            case "appendifmissingignorecase":
                return funcAppendIfMissing(path, params, true);
            // Structural
            case "assort":
                return funcAssort(path, params);
        }
        throw new UnsupportedFunctionException();
    }

    private PathTrace applyB(final PathTrace path, final String func) {
        switch (func) {
            // Array
            case "bottomn":
                return funcTopBottomN(path, params, false);
            // Format
            case "b64decode":
                return funcB64Decode(path, params, Base64.getDecoder());
            case "b64mimedecode":
                return funcB64Decode(path, params, Base64.getMimeDecoder());
            case "b64urldecode":
                return funcB64Decode(path, params, Base64.getUrlDecoder());
            case "b64encode":
                return funcB64Encode(path, params, Base64.getEncoder());
            case "b64encodenopadding":
                return funcB64Encode(path, params, Base64.getEncoder().withoutPadding());
            case "b64mimeencode":
                return funcB64Encode(path, params, Base64.getMimeEncoder());
            case "b64mimeencodenopadding":
                return funcB64Encode(path, params, Base64.getMimeEncoder().withoutPadding());
            case "b64urlencode":
                return funcB64Encode(path, params, Base64.getUrlEncoder());
            case "b64urlencodenopadding":
                return funcB64Encode(path, params, Base64.getUrlEncoder().withoutPadding());
        }
        throw new UnsupportedFunctionException();
    }

    private PathTrace applyC(final PathTrace path, final String func) {
        switch (func) {
            // Arithmetic
            case "calc":
                return funcCalc(path, params);
            case "ceil":
                return funcCeil(path, params);
            // Array
            case "count":
                return funcNumericAggregate(path, func, params);
            // Format
            case "casevalue":
                return funcCaseValue(path, params, false);
            case "casevalueignorecase":
                return funcCaseValue(path, params, true);
            case "coalesce":
                return funcCoalesce(path, params);
            case "csv":
                return funcCsv(path, params, false, false);
            case "csvshownull":
                return funcCsv(path, params, true, false);
            case "csvparams":
                return funcCsv(path, params, true, true);
            case "cyclevalue":
                return funcCycleValue(path, params);
            // Logical
            case "contains":
                return funcContains(path, params, false, false);
            case "containsignorecase":
                return funcContains(path, params, true, false);
            // String
            case "camelcase":
                return funcCamelCase(path, params, false);
            case "camelsnakecase":
                return funcSnakeCase(path, params, CaseUtils.Type.CAMEL);
            case "capitalize":
                return funcCapitalize(path, params);
            case "center":
                return funcPadding(path, params, 0);
            case "concat":
                return funcConcat(path, params, true);
            case "concatfree":
                return funcConcat(path, params, false);
            // Structural
            case "collect":
                return funcCollect(path, params);
            case "cumulatecollect":
                return funcCumulateCollect(path, params);
        }
        throw new UnsupportedFunctionException();
    }

    private PathTrace applyD(final PathTrace path, final String func) {
        switch (func) {
            // Array
            case "distinct":
                return funcDistinct(path, params);
            // Date
            case "dayofweek":
                return funcChronometry(path, params, ChronoField.DAY_OF_WEEK);
            case "day":
                return funcChronometry(path, params, ChronoField.DAY_OF_MONTH);
            case "dayofyear":
                return funcChronometry(path, params, ChronoField.DAY_OF_YEAR);
            case "dayend":
                return funcDayEnd(path, params);
            // Format
            case "default":
                return funcDefault(path, params);
            // String
            case "doublequote":
                return funcDoubleQuote(path, params);
            // Structural
            case "depthlimit":
                return funcDepthLimit(path, params);
        }
        throw new UnsupportedFunctionException();
    }

    private PathTrace applyE(final PathTrace path, final String func) {
        switch (func) {
            // Format
            case "escapehtml":
                return funcMarkupEscape(path, params, MarkupLanguage.HTML);
            case "escapexml":
                return funcMarkupEscape(path, params, MarkupLanguage.XML);
            // Logical
            case "endswith":
                return funcEndsWith(path, params, false, false);
            case "endswithignorecase":
                return funcEndsWith(path, params, true, false);
            case "equals":
                return funcEquals(path, params, false, false);
            case "equalsignorecase":
                return funcEquals(path, params, true, false);
            // Structural
            case "entries":
                return funcEntries(path, params);
        }
        throw new UnsupportedFunctionException();
    }

    private PathTrace applyF(final PathTrace path, final String func) {
        switch (func) {
            // Arithmetic
            case "floor":
                return funcFloor(path, params);
            // Array
            case "findbymax":
                return funcFindByMaxMin(path, params, true, 0);
            case "findbymaxornull":
                return funcFindByMaxMin(path, params, true, -1);
            case "findbymin":
                return funcFindByMaxMin(path, params, false, 0);
            case "findbyminornull":
                return funcFindByMaxMin(path, params, false, -1);
            case "findbynullormax":
                return funcFindByMaxMin(path, params, true, 1);
            case "findbynullormin":
                return funcFindByMaxMin(path, params, false, 1);
            case "first":
                return funcFirst(path, params);
            // Format
            case "formatdate":
                return funcFormatDate(path, params);
            case "formatnumber":
                return funcFormatNumber(path, params);
            case "formattext":
                return funcFormatText(path, params);
            case "formattexts":
                return funcFormatTexts(path, params);
            // Structural
            case "field":
                return funcField(path, params);
            case "flatten":
                return funcFlatten(path, params);
        }
        throw new UnsupportedFunctionException();
    }

    private PathTrace applyG(final PathTrace path, final String func) {
        // Structural
        if ("group".equals(func)) {
            return funcGroup(path, params);
        }
        throw new UnsupportedFunctionException();
    }

    private PathTrace applyH(final PathTrace path, final String func) {
        switch (func) {
            // Date
            case "hourofampm":
                return funcChronometry(path, params, ChronoField.HOUR_OF_AMPM);
            case "hour":
                return funcChronometry(path, params, ChronoField.HOUR_OF_DAY);
        }
        throw new UnsupportedFunctionException();
    }

    private PathTrace applyI(final PathTrace path, final String func) {
        switch (func) {
            // Array
            case "indexof":
                return funcIndexOf(path, params, 1);
            // Format
            case "if":
                return funcIf(path, params, false);
            case "ifnot":
                return funcIf(path, params, true);
            case "indexedvalue":
                return funcIndexedValue(path, params);
            // Logical
            case "in":
                return funcIn(path, params, false, false);
            case "inignorecase":
                return funcIn(path, params, true, false);
            case "isarray":
                return funcIsArray(path, params);
            case "isblank":
                return funcIsBlank(path, params, false);
            case "isnotblank":
                return funcIsBlank(path, params, true);
            case "isboolean":
                return funcIsBoolean(path, params);
            case "isempty":
                return funcIsEmpty(path, params, false);
            case "isnotempty":
                return funcIsEmpty(path, params, true);
            case "isemptyarray":
                return funcIsEmptyArray(path, params);
            case "isemptyobject":
                return funcIsEmptyObject(path, params);
            case "iseven":
                return funcIsEvenOdd(path, params, 0);
            case "isnull":
                return funcIsNull(path, params, false);
            case "isnotnull":
                return funcIsNull(path, params, true);
            case "isnumber":
                return funcIsNumber(path, params);
            case "isobject":
                return funcIsObject(path, params);
            case "isodd":
                return funcIsEvenOdd(path, params, 1);
            case "istext":
                return funcIsText(path, params);
            case "isweekday":
                return funcIsWeekday(path, params);
            case "isweekend":
                return funcIsWeekend(path, params);
            case "isleapyear":
                return funcIsLeapYear(path, params);
        }
        throw new UnsupportedFunctionException();
    }

    private PathTrace applyJ(final PathTrace path, final String func) {
        switch (func) {
            // Array
            case "join":
                return funcJoin(path, params);
            // Structural
            case "json":
                return funcJson(path, params);
        }
        throw new UnsupportedFunctionException();
    }

    private PathTrace applyK(final PathTrace path, final String func) {
        switch (func) {
            // String
            case "keepafter":
                return funcKeep(path, params, false, true, false);
            case "keepafterignorecase":
                return funcKeep(path, params, true, true, false);
            case "keepafterlast":
                return funcKeep(path, params, false, true, true);
            case "keepafterlastignorecase":
                return funcKeep(path, params, true, true, true);
            case "keepbefore":
                return funcKeep(path, params, false, false, false);
            case "keepbeforeignorecase":
                return funcKeep(path, params, true, false, false);
            case "keepbeforelast":
                return funcKeep(path, params, false, false, true);
            case "keepbeforelastignorecase":
                return funcKeep(path, params, true, false, true);
            // Structural
            case "keys":
                return funcKeys(path, params);
        }
        throw new UnsupportedFunctionException();
    }

    private PathTrace applyL(final PathTrace path, final String func) {
        switch (func) {
            // Array
            case "last":
                return funcLast(path, params);
            case "lastindex":
                return funcLastIndex(path, params);
            case "lastindexof":
                return funcIndexOf(path, params, -1);
            // Date
            case "lengthofmonth":
                return funcLengthOfMonth(path, params);
            case "lengthofyear":
                return funcLengthOfYear(path, params);
            case "localtooffsetdate":
                return funcLocalToOffsetDate(path, params);
            case "localdatetomillis":
                return funcLocalDateToEpochMilli(path, params);
            case "localdatetoseconds":
                return funcLocalDateToEpochSecond(path, params);
            // String
            case "leftpad":
                return funcPadding(path, params, -1);
            case "length":
                return funcLength(path, params);
            case "lowersnakecase":
                return funcSnakeCase(path, params, CaseUtils.Type.LOWER);
            case "lowercase":
                return funcLowerCase(path, params);
            // Structural
            case "level":
                return funcLevel(path, params);
        }
        throw new UnsupportedFunctionException();
    }

    private PathTrace applyM(final PathTrace path, final String func) {
        switch (func) {
            // Arithmetic
            case "max":
                return funcMaxMin(path, params, true);
            case "min":
                return funcMaxMin(path, params, false);
            case "mod":
                return funcMod(path, params);
            // Date
            case "millistolocaldate":
                return funcEpochMilliToLocalDate(path, params);
            case "millistooffsetdate":
                return funcEpochMilliToOffsetDate(path, params);
            case "minute":
                return funcChronometry(path, params, ChronoField.MINUTE_OF_HOUR);
            case "minuteofday":
                return funcChronometry(path, params, ChronoField.MINUTE_OF_DAY);
            case "month":
                return funcChronometry(path, params, ChronoField.MONTH_OF_YEAR);
            case "minusseconds":
                return funcDateMinus(path, params, ChronoUnit.SECONDS);
            case "minusminutes":
                return funcDateMinus(path, params, ChronoUnit.MINUTES);
            case "minushours":
                return funcDateMinus(path, params, ChronoUnit.HOURS);
            case "minusdays":
                return funcDateMinus(path, params, ChronoUnit.DAYS);
            case "minusweeks":
                return funcDateMinus(path, params, ChronoUnit.WEEKS);
            case "minusmonths":
                return funcDateMinus(path, params, ChronoUnit.MONTHS);
            case "minusyears":
                return funcDateMinus(path, params, ChronoUnit.YEARS);
            case "monthend":
                return funcMonthEnd(path, params);
            // Logical
            case "matches":
                return funcMatches(path, params, false);
            // Structural
            case "map":
                return funcMap(path, params);
            case "mergeobjects":
                return funcMergeObjects(path, params);
        }
        throw new UnsupportedFunctionException();
    }

    private PathTrace applyN(final PathTrace path, final String func) {
        switch (func) {
            // Date
            case "now":
                return funcNow(path, params);
            // Logical
            case "notcontains":
                return funcContains(path, params, false, true);
            case "notcontainsignorecase":
                return funcContains(path, params, true, true);
            case "notendswith":
                return funcEndsWith(path, params, false, true);
            case "notendswithignorecase":
                return funcEndsWith(path, params, true, true);
            case "notequals":
                return funcEquals(path, params, false, true);
            case "notequalsignorecase":
                return funcEquals(path, params, true, true);
            case "notin":
                return funcIn(path, params, false, true);
            case "notinignorecase":
                return funcIn(path, params, true, true);
            case "not":
                return funcNot(path, params);
            case "notmatches":
                return funcMatches(path, params, true);
            case "notstartswith":
                return funcStartsWith(path, params, false, true);
            case "notstartswithignorecase":
                return funcStartsWith(path, params, true, true);
            // String
            case "notblank":
                return funcNotBlankOrEmpty(path, params, StringUtils::isNotBlank);
            case "notempty":
                return funcNotBlankOrEmpty(path, params, StringUtils::isNotEmpty);
        }
        throw new UnsupportedFunctionException();
    }

    private PathTrace applyO(final PathTrace path, final String func) {
        switch (func) {
            // Date
            case "offsettolocaldate":
                return funcOffsetToLocalDate(path, params);
            case "offsetdatetomillis":
                return funcOffsetDateToEpochMilli(path, params);
            case "offsetdatetoseconds":
                return funcOffsetDateToEpochSecond(path, params);
        }
        throw new UnsupportedFunctionException();
    }

    private PathTrace applyP(final PathTrace path, final String func) {
        switch (func) {
            // Date
            case "plusseconds":
                return funcDatePlus(path, params, ChronoUnit.SECONDS);
            case "plusminutes":
                return funcDatePlus(path, params, ChronoUnit.MINUTES);
            case "plushours":
                return funcDatePlus(path, params, ChronoUnit.HOURS);
            case "plusdays":
                return funcDatePlus(path, params, ChronoUnit.DAYS);
            case "plusweeks":
                return funcDatePlus(path, params, ChronoUnit.WEEKS);
            case "plusmonths":
                return funcDatePlus(path, params, ChronoUnit.MONTHS);
            case "plusyears":
                return funcDatePlus(path, params, ChronoUnit.YEARS);
            // String
            case "prepend":
                return funcPrepend(path, params);
            case "prependifmissing":
                return funcPrependIfMissing(path, params, false);
            case "prependifmissingignorecase":
                return funcPrependIfMissing(path, params, true);
        }
        throw new UnsupportedFunctionException();
    }

    private PathTrace applyQ(final PathTrace path, final String func) {
        switch (func) {
            // String
            case "q":
            case "quote":
                return funcSingleQuote(path, params);
            case "qq":
                return funcDoubleQuote(path, params);
        }
        throw new UnsupportedFunctionException();
    }

    private PathTrace applyR(final PathTrace path, final String func) {
        switch (func) {
            // Arithmetic
            case "round":
                return funcRound(path, params);
            // Array
            case "reverse":
                return funcReverse(path, params);
            // String
            case "removeend":
                return funcRemoveEnd(path, params, false);
            case "removeendignorecase":
                return funcRemoveEnd(path, params, true);
            case "removestart":
                return funcRemoveStart(path, params, false);
            case "removestartignorecase":
                return funcRemoveStart(path, params, true);
            case "repeat":
                return funcRepeat(path, params);
            case "replace":
                return funcReplace(path, params, false);
            case "replaceignorecase":
                return funcReplace(path, params, true);
            case "rightpad":
                return funcPadding(path, params, 1);
        }
        throw new UnsupportedFunctionException();
    }

    private PathTrace applyS(final PathTrace path, final String func) {
        switch (func) {
            // Array
            case "size":
                return funcSize(path, params);
            case "slice":
                return funcSlice(path, params);
            case "sort":
                return funcSort(path, params);
            case "sum":
                return funcNumericAggregate(path, func, params);
            // Date
            case "second":
                return funcChronometry(path, params, ChronoField.SECOND_OF_MINUTE);
            case "secondofday":
                return funcChronometry(path, params, ChronoField.SECOND_OF_DAY);
            case "secondstolocaldate":
                return funcEpochSecondToLocalDate(path, params);
            case "secondstooffsetdate":
                return funcEpochSecondToOffsetDate(path, params);
            // Logical
            case "startswith":
                return funcStartsWith(path, params, false, false);
            case "startswithignorecase":
                return funcStartsWith(path, params, true, false);
            // String
            case "separate":
                return funcSplit(path, params, true);
            case "separatemax":
                return funcSplitMax(path, params, true);
            case "singlequote":
                return funcSingleQuote(path, params);
            case "snakecase":
                return funcSnakeCase(path, params, CaseUtils.Type.UNDEFINED);
            case "split":
                return funcSplit(path, params, false);
            case "splitmax":
                return funcSplitMax(path, params, false);
            case "strip":
                return funcStrip(path, params);
            case "stripend":
                return funcStripEnd(path, params);
            case "stripstart":
                return funcStripStart(path, params);
            case "substr":
                return funcSubstr(path, params);
        }
        throw new UnsupportedFunctionException();
    }

    private PathTrace applyT(final PathTrace path, final String func) {
        switch (func) {
            // Array
            case "topn":
                return funcTopBottomN(path, params, true);
            // Date
            case "truncatetomicro":
                return funcDateTruncateTo(path, params, ChronoUnit.MICROS);
            case "truncatetomilli":
                return funcDateTruncateTo(path, params, ChronoUnit.MILLIS);
            case "truncatetosecond":
                return funcDateTruncateTo(path, params, ChronoUnit.SECONDS);
            case "truncatetominute":
                return funcDateTruncateTo(path, params, ChronoUnit.MINUTES);
            case "truncatetohour":
                return funcDateTruncateTo(path, params, ChronoUnit.HOURS);
            case "truncatetoday":
                return funcDateTruncateTo(path, params, ChronoUnit.DAYS);
            case "truncatetomonth":
                return funcDateTruncateToMonth(path, params);
            case "truncatetoyear":
                return funcDateTruncateToYear(path, params);
            // Format
            case "tonumber":
                return funcToNumber(path, params);
            case "tostring":
                return funcToString(path, params);
            case "totext":
                return funcToText(path, params);
            // String
            case "trim":
                return funcTrim(path, params);
            // Structural
            case "toarray":
                return funcToArray(path, params);
            case "toobject":
                return funcToObject(path, params);
        }
        throw new UnsupportedFunctionException();
    }

    private PathTrace applyU(final PathTrace path, final String func) {
        switch (func) {
            // Date
            case "untilinsecond":
                return funcUntil(path, params, ChronoUnit.SECONDS);
            case "untilinminute":
                return funcUntil(path, params, ChronoUnit.MINUTES);
            case "untilinhour":
                return funcUntil(path, params, ChronoUnit.HOURS);
            case "untilinday":
                return funcUntil(path, params, ChronoUnit.DAYS);
            case "untilinmonth":
                return funcUntil(path, params, ChronoUnit.MONTHS);
            case "untilinyear":
                return funcUntil(path, params, ChronoUnit.YEARS);
            // Format
            case "unescapehtml":
                return funcMarkupUnescape(path, params, MarkupLanguage.HTML);
            case "unescapexml":
                return funcMarkupUnescape(path, params, MarkupLanguage.XML);
            case "urldecode":
                return funcUrlDecode(path, params);
            case "urlencode":
                return funcUrlEncode(path, params);
            // String
            case "uncapitalize":
                return funcUncapitalize(path, params);
            case "uppercamelcase":
                return funcCamelCase(path, params, true);
            case "uppersnakecase":
                return funcSnakeCase(path, params, CaseUtils.Type.UPPER);
            case "uppercase":
                return funcUpperCase(path, params);
            // Structural
            case "unflatten":
                return funcUnflatten(path, params);
            case "unwind":
                return funcUnwind(path, params);
        }
        throw new UnsupportedFunctionException();
    }

    private PathTrace applyW(final PathTrace path, final String func) {
        switch (func) {
            // Date
            case "withnano":
                return funcDateWith(path, params, ChronoField.NANO_OF_SECOND);
            case "withmicro":
                return funcDateWith(path, params, ChronoField.MICRO_OF_SECOND);
            case "withmilli":
                return funcDateWith(path, params, ChronoField.MILLI_OF_SECOND);
            case "withsecond":
                return funcDateWith(path, params, ChronoField.SECOND_OF_MINUTE);
            case "withminute":
                return funcDateWith(path, params, ChronoField.MINUTE_OF_HOUR);
            case "withhour":
                return funcDateWith(path, params, ChronoField.HOUR_OF_DAY);
            case "withday":
                return funcDateWith(path, params, ChronoField.DAY_OF_MONTH);
            case "withdayofyear":
                return funcDateWith(path, params, ChronoField.DAY_OF_YEAR);
            case "withmonth":
                return funcDateWith(path, params, ChronoField.MONTH_OF_YEAR);
            case "withyear":
                return funcDateWith(path, params, ChronoField.YEAR);
            // Structural
            case "wrap":
                return funcWrap(path, params);
        }
        throw new UnsupportedFunctionException();
    }

    private PathTrace applyY(final PathTrace path, final String func) {
        switch (func) {
            // Date
            case "year":
                return funcChronometry(path, params, ChronoField.YEAR);
            case "yearend":
                return funcYearEnd(path, params);
        }
        throw new UnsupportedFunctionException();
    }
}

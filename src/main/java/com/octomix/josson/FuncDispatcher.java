package com.octomix.josson;

import com.fasterxml.jackson.databind.JsonNode;

import static com.octomix.josson.FuncArithmetic.*;
import static com.octomix.josson.FuncArray.*;
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
                case "distinct":
                    return funcDistinct(node, params);
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
                case "max":
                    return funcMaxMin(node, params, true, 0);
                case "maxnull":
                    return funcMaxMin(node, params, true, -1);
                case "min":
                    return funcMaxMin(node, params, false, 0);
                case "minnull":
                    return funcMaxMin(node, params, false, -1);
                case "nullmax":
                    return funcMaxMin(node, params, true, 1);
                case "nullmin":
                    return funcMaxMin(node, params, false, 1);
                case "reverse":
                    return funcReverse(node, params);
                case "slice":
                    return funcSlice(node, params);
                case "sort":
                    return funcSort(node, params);

                // Format
                case "casevalue":
                    return funcCaseValue(node, params);
                case "cyclevalue":
                    return funcCycleValue(node, params);
                case "formatdate":
                    return funcFormatDate(node, params);
                case "formatnum":
                    return funcFormatNumber(node, params);
                case "formattext":
                    return funcFormatText(node, params);
                case "indexedvalue":
                    return funcIndexedValue(node, params);

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
                case "iseven":
                    return funcIsEven(node, params);
                case "isnull":
                    return funcIsNull(node, params, false);
                case "isnotnull":
                    return funcIsNull(node, params, true);
                case "isodd":
                    return funcIsOdd(node, params);
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

                // String
                case "abbreviate":
                    return funcAbbreviate(node, params);
                case "appendifmissing":
                    return funcAppendIfMissing(node, params, false);
                case "appendifmissingignorecase":
                    return funcAppendIfMissing(node, params, true);
                case "b64decode":
                    return funcB64Decode(node, params);
                case "b64encode":
                    return funcB64Encode(node, params);
                case "capitalize":
                    return funcCapitalize(node, params);
                case "center":
                    return funcCenter(node, params);
                case "concat":
                    return funcConcat(node, params);
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
                case "lowercase":
                    return funcLowerCase(node, params);
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
                case "replace":
                    return funcReplace(node, params, false);
                case "replaceignorecase":
                    return funcReplace(node, params, true);
                case "rightpad":
                    return funcRightPad(node, params);
                case "split":
                    return funcSplit(node, params);
                case "substr":
                    return funcSubstr(node, params);
                case "trim":
                    return funcTrim(node, params);
                case "uppercase":
                    return funcUpperCase(node, params);

                // Structural
                case "length":
                    return funcLength(node, params);
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

package com.octomix.josson.core;

import com.fasterxml.jackson.databind.JsonNode;

import static com.octomix.josson.core.FuncArray.*;
import static com.octomix.josson.core.FuncFormat.*;
import static com.octomix.josson.core.FuncLogical.*;
import static com.octomix.josson.core.FuncNumeric.*;
import static com.octomix.josson.core.FuncString.*;
import static com.octomix.josson.core.FuncStructural.*;

public class FuncDispatcher {
    static JsonNode funcDispatcher(JsonNode node, String funcId, String params) {
        try {
            switch (funcId.toLowerCase()) {
                case "sum":
                case "max":
                case "min":
                case "avg":
                case "count":
                    return funcAggregate(node, funcId, params);
                case "equals":
                    return funcEquals(node, params, false);
                case "equalsignorecase":
                    return funcEquals(node, params, true);
                case "contains":
                    return funcContains(node, params, false);
                case "containsignorecase":
                    return funcContains(node, params, true);
                case "startswith":
                    return funcStartsWith(node, params, false);
                case "startswithignorecase":
                    return funcStartsWith(node, params, true);
                case "endswith":
                    return funcEndsWith(node, params, false);
                case "endswithignorecase":
                    return funcEndsWith(node, params, true);
                case "join":
                    return funcJoin(node, params);
                case "map":
                    return funcMap(node, params);
                case "distinct":
                    return funcDistinct(node, params);
                case "toarray":
                    return funcToArray(node, params);
                case "sort":
                    return funcSort(node, params);
                case "reverse":
                    return funcReverse(node, params);
                case "slice":
                    return funcSlice(node, params);
                case "concat":
                    return funcConcat(node, params);
                case "substr":
                    return funcSubstr(node, params);
                case "tolower":
                    return funcToLower(node, params);
                case "toupper":
                    return funcToUpper(node, params);
                case "abbreviate":
                    return funcAbbreviate(node, params);
                case "length":
                    return funcLength(node, params);
                case "size":
                    return funcSize(node, params);
                case "formattext":
                    return funcFormatText(node, params);
                case "formatnum":
                    return funcFormatNumber(node, params);
                case "formatdate":
                    return funcFormatDate(node, params);
                case "b64encode":
                    return funcB64Encode(node, params);
                case "b64decode":
                    return funcB64Decode(node, params);
                case "abs":
                    return funcAbs(node, params);
                case "ceil":
                    return funcCeil(node, params);
                case "floor":
                    return funcFloor(node, params);
                default:
                    throw new IllegalArgumentException("Unsupported function " + funcId + "()");
            }
        } catch (UnsupportedOperationException | NumberFormatException e) {
            throw new IllegalArgumentException("Invalid argument of function call " + funcId + "(" + params + ")", e);
        }
    }
}

package com.octomix.josson;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import org.junit.jupiter.api.Test;

import java.time.ZoneId;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;

public class UnitTest {

    @Test
    public void testJosson() throws Exception {
        Josson.setLocale(Locale.ENGLISH);
        Josson.setZoneId(ZoneId.of("Asia/Hong_Kong"));
        Josson.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        Josson josson = Josson.fromJsonString("{\n" +
                "    \"salesOrderId\": \"SO0001\",\n" +
                "    \"salesDate\": \"2022-01-01T10:01:23\",\n" +
                "    \"salesPerson\": \"Raymond\",\n" +
                "    \"customer\": {\n" +
                "        \"customerId\": \"CU0001\",\n" +
                "        \"name\": \"Peggy\",\n" +
                "        \"phone\": \"+852 62000610\"\n" +
                "    },\n" +
                "    \"items\": [\n" +
                "        {\n" +
                "            \"itemCode\": \"B00001\",\n" +
                "            \"name\": \"WinWin TShirt Series A - 2022\",\n" +
                "            \"brand\": \"WinWin\",\n" +
                "            \"property\": {\n" +
                "                \"size\": \"M\",\n" +
                "                \"colors\": [\"WHITE\",\"RED\"]\n" +
                "            },\n" +
                "            \"qty\": 2,\n" +
                "            \"unit\": \"Pcs\",\n" +
                "            \"unitPrice\": 15.0,\n" +
                "            \"tags\": [\"SHIRT\",\"WOMEN\"]\n" +
                "        },\n" +
                "        {\n" +
                "            \"itemCode\": \"A00308\",\n" +
                "            \"name\": \"OctoPlus Tennis Racket - Star\",\n" +
                "            \"brand\": \"OctoPlus\",\n" +
                "            \"property\": {\n" +
                "                \"colors\": [\"BLACK\"]\n" +
                "            },\n" +
                "            \"qty\": 1,\n" +
                "            \"unit\": \"Pcs\",\n" +
                "            \"unitPrice\": 150.0,\n" +
                "            \"unitDiscount\": 10.0,\n" +
                "            \"tags\": [\"TENNIS\",\"SPORT\",\"RACKET\"]\n" +
                "        },\n" +
                "        {\n" +
                "            \"itemCode\": \"A00201\",\n" +
                "            \"name\": \"WinWin Sport Shoe - Super\",\n" +
                "            \"brand\": \"WinWin\",\n" +
                "            \"property\": {\n" +
                "                \"size\": \"35\",\n" +
                "                \"colors\": [\"RED\"]\n" +
                "            },\n" +
                "            \"qty\": 1,\n" +
                "            \"unit\": \"Pair\",\n" +
                "            \"unitPrice\": 110.0,\n" +
                "            \"unitDiscount\": 10.0,\n" +
                "            \"tags\": [\"SHOE\",\"SPORT\",\"WOMEN\"]\n" +
                "        }\n" +
                "    ],\n" +
                "    \"totalAmount\": 270.0\n" +
                "}");
        AtomicInteger count = new AtomicInteger(0);
        BiConsumer<String, String> evaluate = (path, expected) -> {
            JsonNode node = josson.getNode(path);
            String result;
            if (node == null) {
                result = "!unresolvable!";
            } else if (node.isValueNode()) {
                result = node.asText();
                if (result.isEmpty()) {
                    result = "*empty*";
                }
            } else {
                result = node.toPrettyString().replace("\r", "");
            }
            System.out.printf("%d:\n%s = %s\n%s\n%n", count.incrementAndGet(), path, node == null ? null : node.getNodeType(), result);
            assert result.equals(expected);
        };

        // To query a value node.
        //
        // {}->salesPerson ==>""
        //
        evaluate.accept("salesPerson",
                "Raymond");

        // Node name is case-sensitive.  Josson returns null value if the path is unresolvable.
        evaluate.accept("salesperson",
                "!unresolvable!");

        // To query an object node.
        //
        // {}->customer{} ==>{}
        //
        evaluate.accept("customer",
                "{\n" +
                        "  \"customerId\" : \"CU0001\",\n" +
                        "  \"name\" : \"Peggy\",\n" +
                        "  \"phone\" : \"+852 62000610\"\n" +
                        "}");

        // Object parent-child relation is connected by a ".".
        //
        // {}->customer.name ==>""
        //
        evaluate.accept("customer.name",
                "Peggy");

        // Function is constructed by a function name followed by parentheses with optional comma-separated arguments.
        // A function manipulate the current node and produce an output along the path.
        //
        // {}->customer.name->upperCase() ==>""
        //
        evaluate.accept("customer.name.upperCase()",
                "PEGGY");

        // Function name is case-insensitive.
        // A path argument takes the function's current node as its parent.
        //
        // {}->customer{}->UPPERCase($V) ==>""
        //
        evaluate.accept("customer.UPPERCase(name)",
                "PEGGY");

        // If the function is the first path step, it works on the root node.
        //
        // {}->upperCase($V) ==>""
        //
        evaluate.accept("upperCase(customer.name)",
                "PEGGY");

        // Functions can be nested and the parameters have the same parent node.
        //
        // {}->customer{}->concat($V...) ==>""
        //
        evaluate.accept("customer.concat(upperCase(name), ' / ', phone)",
                "PEGGY / +852 62000610");

        // A path start with numbers override the data and produces an integer node.
        //
        // $I ==>$I
        //
        evaluate.accept("123",
                "123");

        // A path start with numbers and has "." produces a double node.
        //
        // $D ==>$D
        //
        evaluate.accept("123.40",
                "123.4");

        // A path start and end with single quote "'" override the data and produces a text string node.
        // If the string literal contains a single quote, it is replaced by two single quotes.
        //
        // "" ==>""
        //
        evaluate.accept("'She said, ''Go ahead''.'",
                "She said, 'Go ahead'.");

        // A path start with true or false override the data and produces a boolean node.
        //
        // $TF->not() ==>$TF
        //
        evaluate.accept("true.not()",
                "false");

        // To query an array node.
        //
        // {}->items* ==>[{}]
        //
        evaluate.accept("items",
                "[ {\n" +
                        "  \"itemCode\" : \"B00001\",\n" +
                        "  \"name\" : \"WinWin TShirt Series A - 2022\",\n" +
                        "  \"brand\" : \"WinWin\",\n" +
                        "  \"property\" : {\n" +
                        "    \"size\" : \"M\",\n" +
                        "    \"colors\" : [ \"WHITE\", \"RED\" ]\n" +
                        "  },\n" +
                        "  \"qty\" : 2,\n" +
                        "  \"unit\" : \"Pcs\",\n" +
                        "  \"unitPrice\" : 15.0,\n" +
                        "  \"tags\" : [ \"SHIRT\", \"WOMEN\" ]\n" +
                        "}, {\n" +
                        "  \"itemCode\" : \"A00308\",\n" +
                        "  \"name\" : \"OctoPlus Tennis Racket - Star\",\n" +
                        "  \"brand\" : \"OctoPlus\",\n" +
                        "  \"property\" : {\n" +
                        "    \"colors\" : [ \"BLACK\" ]\n" +
                        "  },\n" +
                        "  \"qty\" : 1,\n" +
                        "  \"unit\" : \"Pcs\",\n" +
                        "  \"unitPrice\" : 150.0,\n" +
                        "  \"unitDiscount\" : 10.0,\n" +
                        "  \"tags\" : [ \"TENNIS\", \"SPORT\", \"RACKET\" ]\n" +
                        "}, {\n" +
                        "  \"itemCode\" : \"A00201\",\n" +
                        "  \"name\" : \"WinWin Sport Shoe - Super\",\n" +
                        "  \"brand\" : \"WinWin\",\n" +
                        "  \"property\" : {\n" +
                        "    \"size\" : \"35\",\n" +
                        "    \"colors\" : [ \"RED\" ]\n" +
                        "  },\n" +
                        "  \"qty\" : 1,\n" +
                        "  \"unit\" : \"Pair\",\n" +
                        "  \"unitPrice\" : 110.0,\n" +
                        "  \"unitDiscount\" : 10.0,\n" +
                        "  \"tags\" : [ \"SHOE\", \"SPORT\", \"WOMEN\" ]\n" +
                        "} ]");

        // An array filter is enclosed by square brackets.
        // To query an array element by index value.
        //
        // {}->items[0] ==>{}
        //
        evaluate.accept("items[0]",
                "{\n" +
                        "  \"itemCode\" : \"B00001\",\n" +
                        "  \"name\" : \"WinWin TShirt Series A - 2022\",\n" +
                        "  \"brand\" : \"WinWin\",\n" +
                        "  \"property\" : {\n" +
                        "    \"size\" : \"M\",\n" +
                        "    \"colors\" : [ \"WHITE\", \"RED\" ]\n" +
                        "  },\n" +
                        "  \"qty\" : 2,\n" +
                        "  \"unit\" : \"Pcs\",\n" +
                        "  \"unitPrice\" : 15.0,\n" +
                        "  \"tags\" : [ \"SHIRT\", \"WOMEN\" ]\n" +
                        "}");

        // To query a value node in an array element.
        //
        // {}->items[1].name ==>""
        //
        evaluate.accept("items[1].name",
                "OctoPlus Tennis Racket - Star");

        // To query an object node in an array element.
        //
        // {}->items[2].property{} ==>{}
        //
        evaluate.accept("items[2].property",
                "{\n" +
                        "  \"size\" : \"35\",\n" +
                        "  \"colors\" : [ \"RED\" ]\n" +
                        "}");

        // To query all the elements of an array node and output them inside an array node.
        //
        // {}->items*->[qty] ==>[$I]
        //
        evaluate.accept("items.qty",
                "[ 2, 1, 1 ]");

        // A function that manipulates each array element and output all results inside an array node.
        //
        // {}->items*->[{}->concat($V)] ==>[""]
        //
        evaluate.accept("items.concat('Qty=',qty)",
                "[ \"Qty=2\", \"Qty=1\", \"Qty=1\" ]");

        // For function argument, a path step "?" represents the current node.
        //
        // {}->items*->[qty]->[$I->concat(?)] ==>[""]
        //
        evaluate.accept("items.qty.concat('Qty=',?)",
                "[ \"Qty=2\", \"Qty=1\", \"Qty=1\" ]");

        // A function that manipulates an array node and produce a value node.
        //
        // {}->items*->[qty]->sum() ==>$D
        //
        evaluate.accept("items.qty.sum()",
                "4.0");

        // Uses Java standard formatting pattern.
        //
        // {}->items*->[{}]->sum([$V])->formatNumber() ==>""
        //
        evaluate.accept("items.sum(qty).formatNumber('#,##0')",
                "4");

        // Find the first matching element by array filter.
        //
        // {}->items*->[itemCode][] ==>""
        //
        evaluate.accept("items.itemCode[!startsWith('A')]",
                "B00001");

        // Filter using relational operators "=", "!=", ">", ">=", "<" and "<=".
        //
        // {}->items[].name ==>""
        //
        evaluate.accept("items[unitDiscount > 0].name",
                "OctoPlus Tennis Racket - Star");

        // Returns null value if nothing matches the array filter.
        evaluate.accept("items[unitDiscount > 100].name",
                "!unresolvable!");

        // To query all matching elements, add a modifier "*" after the array filter.
        //
        // {}->items[]*->[name] ==>[""]
        //
        evaluate.accept("items[unitDiscount > 0]*.name",
                "[ \"OctoPlus Tennis Racket - Star\", \"WinWin Sport Shoe - Super\" ]");

        // For each path step, a nested array is flattened once.
        //
        // {}->items[]*->[tags[]*->[""]] ==>[""]
        //
        evaluate.accept("items[true]*.tags[true]*",
                "[ \"SHIRT\", \"WOMEN\", \"TENNIS\", \"SPORT\", \"RACKET\", \"SHOE\", \"SPORT\", \"WOMEN\" ]");

        // Path step "array." is the same as "array[true]*.".
        //
        // {}->items*->[tags*->[""]] ==>[""]
        //
        evaluate.accept("items.tags",
                "[ \"SHIRT\", \"WOMEN\", \"TENNIS\", \"SPORT\", \"RACKET\", \"SHOE\", \"SPORT\", \"WOMEN\" ]");

        // The matching criteria supports logical operators and parentheses.
        // "!" = not, "&" = and, "|" = or
        //
        // {}->items[]*->[name] ==>[""]
        //
        evaluate.accept("items[(unitDiscount=null | unitDiscount=0) & !(qty<=1)]*.name",
                "[ \"WinWin TShirt Series A - 2022\" ]");

        // Example of a find-all filter operation with flattened array result.
        //
        // {}->items[]*->[tags*->[""]] ==>[""]
        //
        evaluate.accept("items[tags.contains('SPORT')]*.tags",
                "[ \"TENNIS\", \"SPORT\", \"RACKET\", \"SHOE\", \"SPORT\", \"WOMEN\" ]");

        // An array filter modifier "@" divert each element to separate branch for upcoming manipulation.
        // The final output merges branches into an array.
        //
        //              {}->tags*->[""]
        //             /               \
        // {}->items[]@                 ==>[[""]]
        //             \               /
        //              {}->tags*->[""]
        //
        evaluate.accept("items[tags.containsIgnoreCase('Women')]@.tags",
                "[ [ \"SHIRT\", \"WOMEN\" ], [ \"SHOE\", \"SPORT\", \"WOMEN\" ] ]");

        // Some functions work on an array node and produce a value node.
        //
        // {}->items*->[tags*->[""]]->[""]->join() ==>""
        //
        evaluate.accept("items.tags.join('+')",
                "SHIRT+WOMEN+TENNIS+SPORT+RACKET+SHOE+SPORT+WOMEN");

        // An array node can apply the modifier "@" that divert each element to separate branch.
        //
        //            {}->tags*->[""]->join()->""
        //           /                           \
        // {}->items@                             ==>[""]
        //           \                           /
        //            {}->tags*->[""]->join()->""
        //
        evaluate.accept("items@.tags.join('+')",
                "[ \"SHIRT+WOMEN\", \"TENNIS+SPORT+RACKET\", \"SHOE+SPORT+WOMEN\" ]");

        // Syntax "[]@" diverts each element of the current array node.
        //
        //                            {}->tags*->[""]->join()->""
        //                           /                           \
        // {}->items*->[{}]->join([]@                             =>[""]) ==>""
        //                           \                           /
        //                            {}->tags*->[""]->join()->""
        //
        evaluate.accept("items.join([]@.tags.join('+'),' / ')",
                "SHIRT+WOMEN / TENNIS+SPORT+RACKET / SHOE+SPORT+WOMEN");

        // Modifier "@" before a function name merges all branch results into a single array before manipulation.
        //
        //            {}->tags*->[""]->join()->""
        //           /                           \
        // {}->items@                             @->[""]->join() ==>""
        //           \                           /
        //            {}->tags*->[""]->join()->""
        //
        evaluate.accept("items@.tags.join('+').@join(' / ')",
                "SHIRT+WOMEN / TENNIS+SPORT+RACKET / SHOE+SPORT+WOMEN");

        // Syntax "[]@" can divert the array output of function.
        //
        //                    ""->split()->[""->calc(?)]->[$D->round()]->[$I]->join()->""->concat()
        //                   /                                                                     \
        // ""->split()->[""]@                                                                       @->[""]->join()==>""
        //                   \                                                                     /
        //                    ""->split()->[""->calc(?)]->[$D->round()]->[$I]->join()->""->concat()
        //
        evaluate.accept("'1+2 | 3+4 | 5+6'.split('|').[]@.split('+').calc(?*2).round(0).join('+').concat('(',?,')/2').@join(' | ')",
                "(2+4)/2 | (6+8)/2 | (10+12)/2");

        // Function parameter can be a value node of parent.
        //
        //            {}->repeat($V...)->""
        //           /                     \
        // {}->items@                       @->[""]->join()==>""
        //           \                     /
        //            {}->repeat($V...)->""
        //
        evaluate.accept("items@.repeat(concat('[',brand,'] ',name,'\n'), qty).@join()",
                "[WinWin] WinWin TShirt Series A - 2022\n" +
                        "[WinWin] WinWin TShirt Series A - 2022\n" +
                        "[OctoPlus] OctoPlus Tennis Racket - Star\n" +
                        "[WinWin] WinWin Sport Shoe - Super\n");

        // Functions work on array and produce an array, such as "concat()", manipulate on each element.
        // An argument "#" denotes the zero-based array index.
        //
        // {}->items*->[{}->concat(#, $V...)]->join() ==>""
        //
        evaluate.accept("items.concat('Item ',#,': [',itemCode,'] ',qty,unit,' x ',name,' <',property.colors.join(','),'>').join('\n')",
                "Item 0: [B00001] 2Pcs x WinWin TShirt Series A - 2022 <WHITE,RED>\n" +
                        "Item 1: [A00308] 1Pcs x OctoPlus Tennis Racket - Star <BLACK>\n" +
                        "Item 2: [A00201] 1Pair x WinWin Sport Shoe - Super <RED>");

        // An argument "##" denotes the one-based array index.
        // A function argument path step start with "@" represents the parent array node.
        //                            .----->----.
        //                           /            \
        // {}->items*->[{}]->sort($V)->[{}->concat(@, ##, $V...)]->join() ==>""
        //
        evaluate.accept("items.sort(itemCode).concat('Item ',##,'/',@.size(),': [',itemCode,'] ',qty,unit,' x ',name,' <',property.colors.join(','),'>').join('\n')",
                "Item 1/3: [A00201] 1Pair x WinWin Sport Shoe - Super <RED>\n" +
                        "Item 2/3: [A00308] 1Pcs x OctoPlus Tennis Racket - Star <BLACK>\n" +
                        "Item 3/3: [B00001] 2Pcs x WinWin TShirt Series A - 2022 <WHITE,RED>");

        // An object node with a validation filter.
        evaluate.accept("customer[name='Peggy']",
                "{\n" +
                        "  \"customerId\" : \"CU0001\",\n" +
                        "  \"name\" : \"Peggy\",\n" +
                        "  \"phone\" : \"+852 62000610\"\n" +
                        "}");
        evaluate.accept("customer[name='Raymond']", "!unresolvable!");

        // Function "json" parse a JSON string.
        evaluate.accept("json('[1,2,\"3\"]')",
                "[ 1, 2, \"3\" ]");

        // Relational operator "=" and "!=" support object comparison.
        evaluate.accept("[customer = json('{\"name\":\"Peggy\",\"phone\":\"+852 62000610\",\"customerId\":\"CU0001\"}')].isNotNull()",
                "true");

        // Relational operator "=" and "!=" support root level array values comparison where the position ordering is allowed to be different.
        evaluate.accept("[items[0].property.colors = json('[\"RED\",\"WHITE\"]')].isNotNull()",
                "true");

        // Function "calc" uses MathParser.org-mXparser library <http://mathparser.org/> to perform calculation.
        //
        // {}->items*->[{}->calc($V...)]->[$D->concat(?, ##)] ==>[""]
        //
        evaluate.accept("items.calc(qty * (unitPrice-unitDiscount)).concat(##,'=',?)",
                "[ null, \"2=140.0\", \"3=100.0\" ]");

        // Non-array manipulate functions preserve null element.
        //
        // {}->items*->[{}->calc($V...)]->[$D][]*->[$D->concat(?, ##)] ==>[""]
        //
        evaluate.accept("items.calc(qty * (unitPrice-unitDiscount)).[##<=2]*.concat(##,'=',?)",
                "[ null, \"2=140.0\" ]");

        // An array-to-value transformation function throws away null nodes automatically.
        //
        // {}->items*->[{}->calc($V...)]->[$D->concat(?, ##)]->join() ==>""
        //
        evaluate.accept("items.calc(qty * (unitPrice-unitDiscount)).concat(##,'=',?).join(' / ')",
                "2=140.0 / 3=100.0");

        // Array filter can filter out null nodes.
        //
        // {}->items*->[{}->calc($V...)]->[$D][]*->[$D->concat(?, ##)] ==>[""]
        //
        evaluate.accept("items.calc(qty * (unitPrice-unitDiscount)).[isNotNull()]*.concat(##,'=',?)",
                "[ \"1=140.0\", \"2=100.0\" ]");

        // An argument "#A" denotes the uppercase alphabetic array index.
        //
        // {}->items*->[{}->calc($V...)]->[$D][]*->[$D->concat(?, #A)]->join() ==>""
        //
        evaluate.accept("items.calc(qty * (unitPrice-unitDiscount)).[?!=null]*.concat(#A,'=',?).join(' / ')",
                "A=140.0 / B=100.0");

        // Merge Diverted branches throws away null nodes automatically.
        // An argument "#a" denotes the lowercase alphabetic array index.
        //
        //            {}->calc($V...)->$D
        //           /                   \
        // {}->items@                     @->[$D->concat(?, #a)] ==>[""]
        //           \                   /
        //            {}->calc($V...)->$D
        //
        evaluate.accept("items@.calc(qty * (unitPrice-unitDiscount)).@concat(#a,'=',?)",
                "[ \"a=140.0\", \"b=100.0\" ]");

        // mXparser expression accepts single-level path only.
        // To apply multi-level path, function or filter, append arguments with syntax "newVariable:path".
        evaluate.accept("items.calc(qty * (unitPrice-x), x:coalesce(unitDiscount,0)).formatNumber('US$#,##0.00')",
                "[ \"US$30.00\", \"US$140.00\", \"US$100.00\" ]");

        // An argument "#r" and "#R" denotes the lowercase and uppercase roman numerals array index.
        evaluate.accept("items.unitPrice.calc(? * 2).concat(#r,'=',?)",
                "[ \"i=30.0\", \"ii=300.0\", \"iii=220.0\" ]");

        // Function "entries" returns an array of an object's string-keyed property [key, value] pairs.
        evaluate.accept("items[0].entries()",
                "[ {\n" +
                        "  \"key\" : \"itemCode\",\n" +
                        "  \"value\" : \"B00001\"\n" +
                        "}, {\n" +
                        "  \"key\" : \"name\",\n" +
                        "  \"value\" : \"WinWin TShirt Series A - 2022\"\n" +
                        "}, {\n" +
                        "  \"key\" : \"brand\",\n" +
                        "  \"value\" : \"WinWin\"\n" +
                        "}, {\n" +
                        "  \"key\" : \"property\",\n" +
                        "  \"value\" : {\n" +
                        "    \"size\" : \"M\",\n" +
                        "    \"colors\" : [ \"WHITE\", \"RED\" ]\n" +
                        "  }\n" +
                        "}, {\n" +
                        "  \"key\" : \"qty\",\n" +
                        "  \"value\" : 2\n" +
                        "}, {\n" +
                        "  \"key\" : \"unit\",\n" +
                        "  \"value\" : \"Pcs\"\n" +
                        "}, {\n" +
                        "  \"key\" : \"unitPrice\",\n" +
                        "  \"value\" : 15.0\n" +
                        "}, {\n" +
                        "  \"key\" : \"tags\",\n" +
                        "  \"value\" : [ \"SHIRT\", \"WOMEN\" ]\n" +
                        "} ]");

        // Function "keys" lists an object's key names.
        evaluate.accept("keys()",
                "[ \"salesOrderId\", \"salesDate\", \"salesPerson\", \"customer\", \"items\", \"totalAmount\" ]");

        // "keys()" can retrieve nested child object keys for a given levels.
        evaluate.accept("keys(?, 2)",
                "[ \"salesOrderId\", \"salesDate\", \"salesPerson\", \"customer\", \"customerId\", \"name\", \"phone\", \"items\", \"totalAmount\" ]");

        // Function "toArray" puts an object's values into an array.
        evaluate.accept("customer.toArray()",
                "[ \"CU0001\", \"Peggy\", \"+852 62000610\" ]");

        // Furthermore, function "toArray" puts all arguments (values, object's values, array elements) into a single array.
        evaluate.accept("toArray('Hello',customer,items.itemCode.sort())",
                "[ \"Hello\", \"CU0001\", \"Peggy\", \"+852 62000610\", \"A00201\", \"A00308\", \"B00001\" ]");

        // Function "map" constructs a new object node.
        // For multi-level path, the last element name will become the new element name.
        // To rename an element, use syntax "newFieldName:path".
        evaluate.accept("map(customer.name,date:salesDate,sales:map(items.concat(name,' x ',qty,unit), totalQty:items.sum(qty), totalAmount))",
                "{\n" +
                        "  \"name\" : \"Peggy\",\n" +
                        "  \"date\" : \"2022-01-01T10:01:23\",\n" +
                        "  \"sales\" : {\n" +
                        "    \"items\" : [ \"WinWin TShirt Series A - 2022 x 2Pcs\", \"OctoPlus Tennis Racket - Star x 1Pcs\", \"WinWin Sport Shoe - Super x 1Pair\" ],\n" +
                        "    \"totalQty\" : 4.0,\n" +
                        "    \"totalAmount\" : 270.0\n" +
                        "  }\n" +
                        "}");

        // Function "field" adds, removes and renames field on the current object node.
        // To remove an element, use syntax "fieldName:".
        evaluate.accept("items[0].field(subtotal:calc(qty * (unitPrice-x), x:coalesce(unitDiscount,0)),brand:,property:,tags:)",
                "{\n" +
                        "  \"itemCode\" : \"B00001\",\n" +
                        "  \"name\" : \"WinWin TShirt Series A - 2022\",\n" +
                        "  \"qty\" : 2,\n" +
                        "  \"unit\" : \"Pcs\",\n" +
                        "  \"unitPrice\" : 15.0,\n" +
                        "  \"subtotal\" : 30.0\n" +
                        "}");

        // Function `map` and `field` works on array.
        evaluate.accept("items.field(subtotal:calc(qty * (unitPrice-x), x:coalesce(unitDiscount,0)),brand:,property:,tags:)",
                "[ {\n" +
                        "  \"itemCode\" : \"B00001\",\n" +
                        "  \"name\" : \"WinWin TShirt Series A - 2022\",\n" +
                        "  \"qty\" : 2,\n" +
                        "  \"unit\" : \"Pcs\",\n" +
                        "  \"unitPrice\" : 15.0,\n" +
                        "  \"subtotal\" : 30.0\n" +
                        "}, {\n" +
                        "  \"itemCode\" : \"A00308\",\n" +
                        "  \"name\" : \"OctoPlus Tennis Racket - Star\",\n" +
                        "  \"qty\" : 1,\n" +
                        "  \"unit\" : \"Pcs\",\n" +
                        "  \"unitPrice\" : 150.0,\n" +
                        "  \"unitDiscount\" : 10.0,\n" +
                        "  \"subtotal\" : 140.0\n" +
                        "}, {\n" +
                        "  \"itemCode\" : \"A00201\",\n" +
                        "  \"name\" : \"WinWin Sport Shoe - Super\",\n" +
                        "  \"qty\" : 1,\n" +
                        "  \"unit\" : \"Pair\",\n" +
                        "  \"unitPrice\" : 110.0,\n" +
                        "  \"unitDiscount\" : 10.0,\n" +
                        "  \"subtotal\" : 100.0\n" +
                        "} ]");

        // Function "flatten" flatten an array same as the default path step behavior. But more readable.
        evaluate.accept("items@.tags",
                "[ [ \"SHIRT\", \"WOMEN\" ], [ \"TENNIS\", \"SPORT\", \"RACKET\" ], [ \"SHOE\", \"SPORT\", \"WOMEN\" ] ]");
        evaluate.accept("items@.tags.@",
                "[ [ \"SHIRT\", \"WOMEN\" ], [ \"TENNIS\", \"SPORT\", \"RACKET\" ], [ \"SHOE\", \"SPORT\", \"WOMEN\" ] ]");
        evaluate.accept("items@.tags.@flatten()",
                "[ \"SHIRT\", \"WOMEN\", \"TENNIS\", \"SPORT\", \"RACKET\", \"SHOE\", \"SPORT\", \"WOMEN\" ]");
        evaluate.accept("items@.tags.@[true]*",
                "[ \"SHIRT\", \"WOMEN\", \"TENNIS\", \"SPORT\", \"RACKET\", \"SHOE\", \"SPORT\", \"WOMEN\" ]");

        josson.setJsonString("{\n" +
                "    \"a\": [\n" +
                "        {\n" +
                "            \"b\": [\n" +
                "                {\n" +
                "                    \"c\": [\n" +
                "                        {\n" +
                "                            \"d\": 1\n" +
                "                        },\n" +
                "                        {\n" +
                "                            \"d\": 3\n" +
                "                        }\n" +
                "                    ]\n" +
                "                },\n" +
                "                {\n" +
                "                    \"c\": [\n" +
                "                        {\n" +
                "                            \"d\": 5\n" +
                "                        },\n" +
                "                        {\n" +
                "                            \"d\": 7\n" +
                "                        }\n" +
                "                    ]\n" +
                "                }\n" +
                "            ]\n" +
                "        },\n" +
                "        {\n" +
                "            \"b\": [\n" +
                "                {\n" +
                "                    \"c\": [\n" +
                "                        {\n" +
                "                            \"d\": 13\n" +
                "                        },\n" +
                "                        {\n" +
                "                            \"d\": 15\n" +
                "                        }\n" +
                "                    ]\n" +
                "                },\n" +
                "                {\n" +
                "                    \"c\": [\n" +
                "                        {\n" +
                "                            \"d\": 17\n" +
                "                        },\n" +
                "                        {\n" +
                "                            \"d\": 19\n" +
                "                        }\n" +
                "                    ]\n" +
                "                }\n" +
                "            ]\n" +
                "        }\n" +
                "    ]\n" +
                "}");
        // Demonstrate the effect of array flatten and divert.
        evaluate.accept("a.b.c.sum(d)",
                "80.0");
        evaluate.accept("a.b.c@.sum(d)",
                "[ 1.0, 3.0, 5.0, 7.0, 13.0, 15.0, 17.0, 19.0 ]");
        evaluate.accept("a.b@.c.sum(d)",
                "[ 4.0, 12.0, 28.0, 36.0 ]");
        evaluate.accept("a@.b.c.sum(d)",
                "[ 16.0, 64.0 ]");
        evaluate.accept("a.b@.c@.sum(d)",
                "[ [ 1.0, 3.0 ], [ 5.0, 7.0 ], [ 13.0, 15.0 ], [ 17.0, 19.0 ] ]");
        evaluate.accept("a@.b.c@.sum(d)",
                "[ [ 1.0, 3.0, 5.0, 7.0 ], [ 13.0, 15.0, 17.0, 19.0 ] ]");
        evaluate.accept("a@.b@.c.sum(d)",
                "[ [ 4.0, 12.0 ], [ 28.0, 36.0 ] ]");
        evaluate.accept("a@.b@.c@.sum(d)",
                "[ [ [ 1.0, 3.0 ], [ 5.0, 7.0 ] ], [ [ 13.0, 15.0 ], [ 17.0, 19.0 ] ] ]");

        // Arithmetic functions
        // abs()
        evaluate.accept("-3.14.abs()", "3.14");
        evaluate.accept("abs(3.14)", "3.14");
        // calc()
        evaluate.accept("1.5.calc(? * 2 + ? / 2)", "3.75");
        evaluate.accept("calc(2^8)", "256.0");
        evaluate.accept("calc(sqrt(a^2 + b^2), a:3, b:4)", "5.0");
        // ceil()
        evaluate.accept("3.14.ceil()", "4");
        evaluate.accept("ceil(-3.14)", "-3");
        // floor()
        evaluate.accept("3.14.floor()", "3");
        evaluate.accept("floor(-3.14)", "-4");
        // mod()
        evaluate.accept("8.mod(3)", "2");
        evaluate.accept("8.mod(?, 3)", "2");
        evaluate.accept("mod(-8, 3)", "1");
        evaluate.accept("3.mod(-8, ?)", "1");
        // round()
        evaluate.accept("3.14.round(1)", "3.1");
        evaluate.accept("3.14.round(?, 1)", "3.1");
        evaluate.accept("round(3.56, 0)", "4");

        // String functions
        // abbreviate()
        evaluate.accept("'abcdefghijkl'.abbreviate(9)", "abcdef...");
        evaluate.accept("'abcdefghijkl'.abbreviate(5, 9)", "...fgh...");
        evaluate.accept("'abcdefghijkl'.abbreviate(?, 7, 9)", "...ghijkl");
        evaluate.accept("abbreviate('abcdefghijkl', 0, 9)", "abcdef...");
        evaluate.accept("abbreviate('abcdefghijkl', 1, 9)", "abcdef...");
        evaluate.accept("abbreviate('abcdefghijkl', 4, 9)", "abcdef...");
        evaluate.accept("abbreviate('abcdefghijkl', 5, 9)", "...fgh...");
        evaluate.accept("abbreviate('abcdefghijkl', 6, 9)", "...ghijkl");
        evaluate.accept("abbreviate('abcdefghijkl', 10, 9)", "...ghijkl");
        evaluate.accept("abbreviate('abcdefghijkl', 11, 9)", "...ghijkl");
        // appendIfMissing()
        evaluate.accept("'abc'.appendIfMissing('xyz')", "abcxyz");
        evaluate.accept("'abc'.appendIfMissing(?, 'xyz')", "abcxyz");
        evaluate.accept("appendIfMissing('abcxyz', 'xyz')", "abcxyz");
        evaluate.accept("'xyz'.appendIfMissing('abcXYZ', ?)", "abcXYZxyz");
        // appendIfMissingIgnoreCase()
        evaluate.accept("'abc'.appendIfMissingIgnoreCase('xyz')", "abcxyz");
        evaluate.accept("'abc'.appendIfMissingIgnoreCase(?, 'xyz')", "abcxyz");
        evaluate.accept("appendIfMissingIgnoreCase('abcxyz', 'xyz')", "abcxyz");
        evaluate.accept("'xyz'.appendIfMissingIgnoreCase('abcXYZ', ?)", "abcXYZ");
        // capitalize()
        evaluate.accept("'cat'.capitalize()", "Cat");
        evaluate.accept("capitalize('cAt')", "CAt");
        // center()
        evaluate.accept("'abc'.center(7)", "  abc  ");
        evaluate.accept("'abc'.center(7, 'X')", "XXabcXX");
        evaluate.accept("'abc'.center(?, 7, upperCase(?))", "ABabcAB");
        evaluate.accept("center('abc', 7, '')", "  abc  ");
        evaluate.accept("4.center('a', ?, 'yz')", "yayz");
        // concat()
        evaluate.accept("'Hello'.concat(2022, '... ', ?, ' World!')", "2022... Hello World!");
        // keepAfter()
        evaluate.accept("'abcxmnxyz'.keepAfter('x')", "mnxyz");
        evaluate.accept("'abcxmnxyz'.keepAfter(?, 'X')", "*empty*");
        evaluate.accept("keepAfter('abcxmnxyz', 'mn')", "xyz");
        // keepAfterIgnoreCase()
        evaluate.accept("'abcxmnxyz'.keepAfterIgnoreCase('x')", "mnxyz");
        evaluate.accept("'abcxmnxyz'.keepAfterIgnoreCase(?, 'X')", "mnxyz");
        evaluate.accept("keepAfterIgnoreCase('abcxmnxyz', 'mn')", "xyz");
        // keepAfterLast()
        evaluate.accept("'abcxmnxyz'.keepAfterLast('x')", "yz");
        evaluate.accept("'abcxmnxyz'.keepAfterLast(?, 'X')", "*empty*");
        evaluate.accept("keepAfterLast('abcxmnxyz', 'mn')", "xyz");
        // keepAfterLastIgnoreCase()
        evaluate.accept("'abcxmnxyz'.keepAfterLastIgnoreCase('x')", "yz");
        evaluate.accept("'abcxmnxyz'.keepAfterLastIgnoreCase(?, 'X')", "yz");
        evaluate.accept("keepAfterLastIgnoreCase('abcxmnxyz', 'mn')", "xyz");
        // keepBefore()
        evaluate.accept("'abcxmnxyz'.keepBefore('x')", "abc");
        evaluate.accept("'abcxmnxyz'.keepBefore(?, 'X')", "*empty*");
        evaluate.accept("keepBefore('abcxmnxyz', 'mn')", "abcx");
        // keepBeforeIgnoreCase()
        evaluate.accept("'abcxmnxyz'.keepBeforeIgnoreCase('x')", "abc");
        evaluate.accept("'abcxmnxyz'.keepBeforeIgnoreCase(?, 'X')", "abc");
        evaluate.accept("keepBeforeIgnoreCase('abcxmnxyz', 'mn')", "abcx");
        // keepBeforeLast()
        evaluate.accept("'abcxmnxyz'.keepBeforeLast('x')", "abcxmn");
        evaluate.accept("'abcxmnxyz'.keepBeforeLast(?, 'X')", "*empty*");
        evaluate.accept("keepBeforeLast('abcxmnxyz', 'mn')", "abcx");
        // keepBeforeLastIgnoreCase()
        evaluate.accept("'abcxmnxyz'.keepBeforeLastIgnoreCase('x')", "abcxmn");
        evaluate.accept("'abcxmnxyz'.keepBeforeLastIgnoreCase(?, 'X')", "abcxmn");
        evaluate.accept("keepBeforeLastIgnoreCase('abcxmnxyz', 'mn')", "abcx");
        // leftPad()
        evaluate.accept("'bat'.leftPad(5).concat('>',?,'<')", ">  bat<");
        evaluate.accept("'bat'.leftPad(?, 8, 'yz')", "yzyzybat");
        evaluate.accept("leftPad('bat', 3, 'yz')", "bat");
        evaluate.accept("5.leftPad('bat', ?, '').concat('>',?,'<')", ">  bat<");
        // length()
        evaluate.accept("'Josson'.length()", "6");
        evaluate.accept("length('Josson')", "6");
        evaluate.accept("length(2022)", "4");
        // lowerCase()
        evaluate.accept("'Cat'.lowerCase()", "cat");
        evaluate.accept("lowerCase('cAt')", "cat");
        // notEmpty()
        evaluate.accept("'abc'.notEmpty('xyz')", "abc");
        evaluate.accept("''.notEmpty(null, '', 'xyz')", "xyz");
        evaluate.accept("json('{\"a\":\"\",\"b\":\"\",\"c\":\"abc\"}').notEmpty(a,b,c,'xyz')", "abc");
        // notBlank()
        evaluate.accept("'abc'.notBlank('xyz')", "abc");
        evaluate.accept("' '.notBlank(null, '  ', 'xyz')", "xyz");
        evaluate.accept("json('{\"a\":\" \",\"b\":\" \",\"c\":\"abc\"}').notBlank(a,b,c,'xyz')", "abc");
        // prependIfMissing()
        evaluate.accept("'abc'.prependIfMissing('xyz')", "xyzabc");
        evaluate.accept("'abc'.prependIfMissing(?, 'xyz')", "xyzabc");
        evaluate.accept("prependIfMissing('xyzabc', 'xyz')", "xyzabc");
        evaluate.accept("'xyz'.prependIfMissing('XYZabc', ?)", "xyzXYZabc");
        // prependIfMissingIgnoreCase()
        evaluate.accept("'abc'.prependIfMissingIgnoreCase('xyz')", "xyzabc");
        evaluate.accept("'abc'.prependIfMissingIgnoreCase(?, 'xyz')", "xyzabc");
        evaluate.accept("prependIfMissingIgnoreCase('xyzabc', 'xyz')", "xyzabc");
        evaluate.accept("'xyz'.prependIfMissingIgnoreCase('XYZabc', ?)", "XYZabc");
        // removeEnd()
        evaluate.accept("'www.domain.com'.removeEnd('.com')", "www.domain");
        evaluate.accept("'www.domain.com'.removeEnd(?, '.Com')", "www.domain.com");
        evaluate.accept("removeEnd('www.domain.com', '.com')", "www.domain");
        // removeEndIgnoreCase()
        evaluate.accept("'www.domain.COM'.removeEndIgnoreCase('.com')", "www.domain");
        evaluate.accept("'www.domain.com'.removeEndIgnoreCase(?, '.Com')", "www.domain");
        evaluate.accept("removeEndIgnoreCase('www.domain.com', '.COM')", "www.domain");
        // removeStart()
        evaluate.accept("'www.domain.com'.removeStart('www.')", "domain.com");
        evaluate.accept("'www.domain.com'.removeStart(?, '.Www')", "www.domain.com");
        evaluate.accept("removeStart('www.domain.com', 'www.')", "domain.com");
        // removeStartIgnoreCase()
        evaluate.accept("'WWW.domain.com'.removeStartIgnoreCase('www.')", "domain.com");
        evaluate.accept("'www.domain.com'.removeStartIgnoreCase(?, '.Www')", "www.domain.com");
        evaluate.accept("removeStartIgnoreCase('www.domain.com', 'WWW.')", "domain.com");
        // repeat()
        evaluate.accept("'a'.repeat(3)", "aaa");
        evaluate.accept("'ab'.repeat(?, 2)", "abab");
        evaluate.accept("repeat('abc', 2)", "abcabc");
        evaluate.accept("3.repeat('abc', ?)", "abcabcabc");
        // replace()
        evaluate.accept("'abaa'.replace('a', 'z')", "zbzz");
        evaluate.accept("'abaa'.replace(?, 'a', 'z', -1)", "zbzz");
        evaluate.accept("replace('abaa', 'a', '', -1)", "b");
        evaluate.accept("replace('abaa', 'A', 'z', 1)", "abaa");
        evaluate.accept("'a'.replace('abaa', ?, 'z', 2)", "zbza");
        // replaceIgnoreCase()
        evaluate.accept("'abaa'.replaceIgnoreCase('a', 'z')", "zbzz");
        evaluate.accept("'abaa'.replaceIgnoreCase(?, 'a', 'z', -1)", "zbzz");
        evaluate.accept("replaceIgnoreCase('abaa', 'a', '', -1)", "b");
        evaluate.accept("replaceIgnoreCase('abaa', 'A', 'z', 1)", "zbaa");
        evaluate.accept("'a'.replaceIgnoreCase('abaa', ?, 'z', 2)", "zbza");
        // rightPad()
        evaluate.accept("'bat'.rightPad(5).concat('>',?,'<')", ">bat  <");
        evaluate.accept("'bat'.rightPad(?, 8, 'yz')", "batyzyzy");
        evaluate.accept("rightPad('bat', 3, 'yz')", "bat");
        evaluate.accept("rightPad('bat', 5, '').concat('>',?,'<')", ">bat  <");
        // split()
        evaluate.accept("'abc def'.split()", "[ \"abc\", \"def\" ]");
        evaluate.accept("'abc  def'.split(' ')", "[ \"abc\", \"def\" ]");
        evaluate.accept("' abc  def '.split(?, ' ')", "[ \"abc\", \"def\" ]");
        evaluate.accept("split('ab:cd:ef', ':')", "[ \"ab\", \"cd\", \"ef\" ]");
        // strip()
        evaluate.accept("'  abc  '.strip(' ').concat('>',?,'<')", ">abc<");
        evaluate.accept("'  abcyx'.strip('xyz').concat('>',?,'<')", ">  abc<");
        evaluate.accept("strip('z abcyx', 'xyz').concat('>',?,'<')", "> abc<");
        // stripEnd()
        evaluate.accept("'  abc  '.stripEnd(' ').concat('>',?,'<')", ">  abc<");
        evaluate.accept("'z abcyx'.stripEnd('xyz').concat('>',?,'<')", ">z abc<");
        evaluate.accept("stripEnd('z abcyx', 'xyz').concat('>',?,'<')", ">z abc<");
        // stripStart()
        evaluate.accept("'  abc  '.stripStart(' ').concat('>',?,'<')", ">abc  <");
        evaluate.accept("'z abcyx'.stripStart('xyz').concat('>',?,'<')", "> abcyx<");
        evaluate.accept("stripStart('z abcyx', 'xyz').concat('>',?,'<')", "> abcyx<");
        // substr()
        evaluate.accept("'abc'.substr(1)", "bc");
        evaluate.accept("'abc'.substr(0, 2)", "ab");
        evaluate.accept("'abc'.substr(?, 1, 2)", "b");
        evaluate.accept("substr('abc', -2, -1)", "b");
        evaluate.accept("2.substr('abc', -4, ?)", "ab");
        // trim()
        evaluate.accept("'abc'.trim()", "abc");
        evaluate.accept("trim('  abc  ')", "abc");
        // uncapitalize()
        evaluate.accept("'Cat'.uncapitalize()", "cat");
        evaluate.accept("uncapitalize('CAt')", "cAt");
        // upperCase()
        evaluate.accept("'Cat'.upperCase()", "CAT");
        evaluate.accept("upperCase('cAt')", "CAT");
        // singleQuote()
        evaluate.accept("'Peggy''s cat'.singleQuote()", "'Peggy''s cat'");
        evaluate.accept("123.singleQuote()", "'123'");
        evaluate.accept("singleQuote('Raymond''s dog')", "'Raymond''s dog'");
        evaluate.accept("singleQuote(True)", "'true'");
        // doubleQuote()
        evaluate.accept("'Peggy\"s cat'.doubleQuote()", "\"Peggy\\\"s cat\"");
        evaluate.accept("12.3.doubleQuote()", "\"12.3\"");
        evaluate.accept("doubleQuote('Raymond\"s dog')", "\"Raymond\\\"s dog\"");
        evaluate.accept("doubleQuote(False)", "\"false\"");

        // Date functions
        // amPmOfDay()
        evaluate.accept("'2022-01-02T03:04:05'.amPmOfDay()", "AM");
        evaluate.accept("amPmOfDay('2022-02-04T13:14:15')", "PM");
        // second()
        evaluate.accept("'2022-01-02T03:04:05'.second()", "5");
        evaluate.accept("second('2022-02-04T13:14:15')", "15");
        // secondOfDay()
        evaluate.accept("'2022-01-02T03:04:05'.secondOfDay()", "11045");
        evaluate.accept("secondOfDay('2022-02-04T13:14:15')", "47655");
        // minute()
        evaluate.accept("'2022-01-02T03:04:05'.minute()", "4");
        evaluate.accept("minute('2022-02-04T13:14:15')", "14");
        // minuteOfDay()
        evaluate.accept("'2022-01-02T03:04:05'.minuteOfDay()", "184");
        evaluate.accept("minuteOfDay('2022-02-04T13:14:15')", "794");
        // hourOfAmPm()
        evaluate.accept("'2022-01-02T03:04:05'.hourOfAmPm()", "3");
        evaluate.accept("hourOfAmPm('2022-02-04T13:14:15')", "1");
        // hour()
        evaluate.accept("'2022-01-02T03:04:05'.hour()", "3");
        evaluate.accept("hour('2022-02-04T13:14:15')", "13");
        // dayOfWeek()
        evaluate.accept("'2022-01-02T03:04:05'.dayOfWeek()", "7");
        evaluate.accept("dayOfWeek('2022-02-04T13:14:15')", "5");
        // day()
        evaluate.accept("'2022-01-02T03:04:05'.day()", "2");
        evaluate.accept("day('2022-02-04T13:14:15')", "4");
        // dayOfYear()
        evaluate.accept("'2022-01-02T03:04:05'.dayOfYear()", "2");
        evaluate.accept("dayOfYear('2022-02-04T13:14:15')", "35");
        // month()
        evaluate.accept("'2022-01-02T03:04:05'.month()", "1");
        evaluate.accept("month('2022-02-04T13:14:15')", "2");
        // year()
        evaluate.accept("'2022-01-02T03:04:05'.year()", "2022");
        evaluate.accept("year('2022-02-04T13:14:15')", "2022");
        // plusSeconds()
        evaluate.accept("'2022-01-02T03:04:05'.plusSeconds(9)", "2022-01-02T03:04:14");
        evaluate.accept("'2022-01-02T03:04:05'.plusSeconds(?, 10)", "2022-01-02T03:04:15");
        evaluate.accept("plusSeconds('2022-02-04T13:14:15', 9)", "2022-02-04T13:14:24");
        // plusMinutes()
        evaluate.accept("'2022-01-02T03:04:05'.plusMinutes(9)", "2022-01-02T03:13:05");
        evaluate.accept("'2022-01-02T03:04:05'.plusMinutes(?, 10)", "2022-01-02T03:14:05");
        evaluate.accept("plusMinutes('2022-02-04T13:14:15', 9)", "2022-02-04T13:23:15");
        // plusHours()
        evaluate.accept("'2022-01-02T03:04:05'.plusHours(9)", "2022-01-02T12:04:05");
        evaluate.accept("'2022-01-02T03:04:05'.plusHours(?, 10)", "2022-01-02T13:04:05");
        evaluate.accept("plusHours('2022-02-04T13:14:15', 9)", "2022-02-04T22:14:15");
        // plusDays()
        evaluate.accept("'2022-01-02T03:04:05'.plusDays(9)", "2022-01-11T03:04:05");
        evaluate.accept("'2022-01-02T03:04:05'.plusDays(?, 10)", "2022-01-12T03:04:05");
        evaluate.accept("plusDays('2022-02-04T13:14:15', 9)", "2022-02-13T13:14:15");
        // plusWeeks()
        evaluate.accept("'2022-01-02T03:04:05'.plusWeeks(9)", "2022-03-06T03:04:05");
        evaluate.accept("'2022-01-02T03:04:05'.plusWeeks(?, 10)", "2022-03-13T03:04:05");
        evaluate.accept("plusWeeks('2022-02-04T13:14:15', 9)", "2022-04-08T13:14:15");
        // plusMonths()
        evaluate.accept("'2022-01-02T03:04:05'.plusMonths(9)", "2022-10-02T03:04:05");
        evaluate.accept("'2022-01-02T03:04:05'.plusMonths(?, 10)", "2022-11-02T03:04:05");
        evaluate.accept("plusMonths('2022-02-04T13:14:15', 9)", "2022-11-04T13:14:15");
        // plusYears()
        evaluate.accept("'2022-01-02T03:04:05'.plusYears(9)", "2031-01-02T03:04:05");
        evaluate.accept("'2022-01-02T03:04:05'.plusYears(?, 10)", "2032-01-02T03:04:05");
        evaluate.accept("plusYears('2022-02-04T13:14:15', 9)", "2031-02-04T13:14:15");
        // minusSeconds()
        evaluate.accept("'2022-01-02T03:04:05'.minusSeconds(9)", "2022-01-02T03:03:56");
        evaluate.accept("'2022-01-02T03:04:05'.minusSeconds(?, 10)", "2022-01-02T03:03:55");
        evaluate.accept("minusSeconds('2022-02-04T13:14:15', 9)", "2022-02-04T13:14:06");
        // minusMinutes()
        evaluate.accept("'2022-01-02T03:04:05'.minusMinutes(9)", "2022-01-02T02:55:05");
        evaluate.accept("'2022-01-02T03:04:05'.minusMinutes(?, 10)", "2022-01-02T02:54:05");
        evaluate.accept("minusMinutes('2022-02-04T13:14:15', 9)", "2022-02-04T13:05:15");
        // minusHours()
        evaluate.accept("'2022-01-02T03:04:05'.minusHours(9)", "2022-01-01T18:04:05");
        evaluate.accept("'2022-01-02T03:04:05'.minusHours(?, 10)", "2022-01-01T17:04:05");
        evaluate.accept("minusHours('2022-02-04T13:14:15', 9)", "2022-02-04T04:14:15");
        // minusDays()
        evaluate.accept("'2022-01-02T03:04:05'.minusDays(9)", "2021-12-24T03:04:05");
        evaluate.accept("'2022-01-02T03:04:05'.minusDays(?, 10)", "2021-12-23T03:04:05");
        evaluate.accept("minusDays('2022-02-04T13:14:15', 9)", "2022-01-26T13:14:15");
        // minusWeeks()
        evaluate.accept("'2022-01-02T03:04:05'.minusWeeks(9)", "2021-10-31T03:04:05");
        evaluate.accept("'2022-01-02T03:04:05'.minusWeeks(?, 10)", "2021-10-24T03:04:05");
        evaluate.accept("minusWeeks('2022-02-04T13:14:15', 9)", "2021-12-03T13:14:15");
        // minusMonths()
        evaluate.accept("'2022-01-02T03:04:05'.minusMonths(9)", "2021-04-02T03:04:05");
        evaluate.accept("'2022-01-02T03:04:05'.minusMonths(?, 10)", "2021-03-02T03:04:05");
        evaluate.accept("minusMonths('2022-02-04T13:14:15', 9)", "2021-05-04T13:14:15");
        // minusYears()
        evaluate.accept("'2022-01-02T03:04:05'.minusYears(9)", "2013-01-02T03:04:05");
        evaluate.accept("'2022-01-02T03:04:05'.minusYears(?, 10)", "2012-01-02T03:04:05");
        evaluate.accept("minusYears('2022-02-04T13:14:15', 9)", "2013-02-04T13:14:15");
        // truncateToMicro()
        evaluate.accept("'2022-01-02T03:04:05.229390600'.truncateToMicro()", "2022-01-02T03:04:05.229390");
        evaluate.accept("truncateToMicro('2022-02-04T13:14:15.229390600')", "2022-02-04T13:14:15.229390");
        // truncateToMilli()
        evaluate.accept("'2022-01-02T03:04:05.229390600'.truncateToMilli()", "2022-01-02T03:04:05.229");
        evaluate.accept("truncateToMilli('2022-02-04T13:14:15.229390600')", "2022-02-04T13:14:15.229");
        // truncateToSecond()
        evaluate.accept("'2022-01-02T03:04:05.229390600'.truncateToSecond()", "2022-01-02T03:04:05");
        evaluate.accept("truncateToSecond('2022-02-04T13:14:15.229390600')", "2022-02-04T13:14:15");
        // truncateToMinute()
        evaluate.accept("'2022-01-02T03:04:05.229390600'.truncateToMinute()", "2022-01-02T03:04");
        evaluate.accept("truncateToMinute('2022-02-04T13:14:15.229390600')", "2022-02-04T13:14");
        // truncateToHour()
        evaluate.accept("'2022-01-02T03:04:05.229390600'.truncateToHour()", "2022-01-02T03:00");
        evaluate.accept("truncateToHour('2022-02-04T13:14:15.229390600')", "2022-02-04T13:00");
        // truncateToDay()
        evaluate.accept("'2022-01-02T03:04:05.229390600'.truncateToDay()", "2022-01-02T00:00");
        evaluate.accept("truncateToDay('2022-02-04T13:14:15.229390600')", "2022-02-04T00:00");
        // truncateToMonth()
        evaluate.accept("'2022-01-02T03:04:05.229390600'.truncateToMonth()", "2022-01-01T00:00");
        evaluate.accept("truncateToMonth('2022-02-04T13:14:15.229390600')", "2022-02-01T00:00");
        // truncateToYear()
        evaluate.accept("'2022-01-02T03:04:05.229390600'.truncateToYear()", "2022-01-01T00:00");
        evaluate.accept("truncateToYear('2022-02-04T13:14:15.229390600')", "2022-01-01T00:00");
        // withNano()
        evaluate.accept("'2022-01-02T03:04'.withNano(789)", "2022-01-02T03:04:00.000000789");
        evaluate.accept("'2022-01-02T03:04'.withNano(?, 789)", "2022-01-02T03:04:00.000000789");
        evaluate.accept("withNano('2022-02-04T13:14', 789)", "2022-02-04T13:14:00.000000789");
        // withMicro()
        evaluate.accept("'2022-01-02T03:04'.withMicro(789)", "2022-01-02T03:04:00.000789");
        evaluate.accept("'2022-01-02T03:04'.withMicro(?, 789)", "2022-01-02T03:04:00.000789");
        evaluate.accept("withMicro('2022-02-04T13:14', 789)", "2022-02-04T13:14:00.000789");
        // withMilli()
        evaluate.accept("'2022-01-02T03:04'.withMilli(789)", "2022-01-02T03:04:00.789");
        evaluate.accept("'2022-01-02T03:04'.withMilli(?, 789)", "2022-01-02T03:04:00.789");
        evaluate.accept("withMilli('2022-02-04T13:14', 789)", "2022-02-04T13:14:00.789");
        // withSecond()
        evaluate.accept("'2022-01-02T03:04'.withSecond(35)", "2022-01-02T03:04:35");
        evaluate.accept("'2022-01-02T03:04'.withSecond(?, 35)", "2022-01-02T03:04:35");
        evaluate.accept("withSecond('2022-02-04T13:14', 35)", "2022-02-04T13:14:35");
        // withMinute()
        evaluate.accept("'2022-01-02T03:04'.withMinute(35)", "2022-01-02T03:35");
        evaluate.accept("'2022-01-02T03:04'.withMinute(?, 35)", "2022-01-02T03:35");
        evaluate.accept("withMinute('2022-02-04T13:14', 35)", "2022-02-04T13:35");
        // withHour()
        evaluate.accept("'2022-01-02T03:04'.withHour(16)", "2022-01-02T16:04");
        evaluate.accept("'2022-01-02T03:04'.withHour(?, 16)", "2022-01-02T16:04");
        evaluate.accept("withHour('2022-02-04T13:14', 16)", "2022-02-04T16:14");
        // withDay()
        evaluate.accept("'2022-01-02T03:04'.withDay(25)", "2022-01-25T03:04");
        evaluate.accept("'2022-01-02T03:04'.withDay(?, 25)", "2022-01-25T03:04");
        evaluate.accept("withDay('2022-02-04T13:14', 25)", "2022-02-25T13:14");
        // withDayOfYear()
        evaluate.accept("'2022-01-02T03:04'.withDayOfYear(123)", "2022-05-03T03:04");
        evaluate.accept("'2022-01-02T03:04'.withDayOfYear(?, 123)", "2022-05-03T03:04");
        evaluate.accept("withDayOfYear('2022-02-04T13:14', 123)", "2022-05-03T13:14");
        // withMonth()
        evaluate.accept("'2022-01-02T03:04'.withMonth(7)", "2022-07-02T03:04");
        evaluate.accept("'2022-01-02T03:04'.withMonth(?, 7)", "2022-07-02T03:04");
        evaluate.accept("withMonth('2022-02-04T13:14', 7)", "2022-07-04T13:14");
        // withYear()
        evaluate.accept("'2022-01-02T03:04'.withYear(2047)", "2047-01-02T03:04");
        evaluate.accept("'2022-01-02T03:04'.withYear(?, 2047)", "2047-01-02T03:04");
        evaluate.accept("withYear('2022-02-04T13:14', 2047)", "2047-02-04T13:14");
        // dayEnd()
        evaluate.accept("'2022-01-02T03:04'.dayEnd()", "2022-01-02T23:59:59.999999999");
        evaluate.accept("dayEnd('2022-02-04T13:14')", "2022-02-04T23:59:59.999999999");
        // monthEnd()
        evaluate.accept("'2022-01-02T03:04'.monthEnd()", "2022-01-31T23:59:59.999999999");
        evaluate.accept("monthEnd('2022-02-04T13:14')", "2022-02-28T23:59:59.999999999");
        // yearEnd()
        evaluate.accept("'2022-01-02T03:04'.yearEnd()", "2022-12-31T23:59:59.999999999");
        evaluate.accept("yearEnd('2022-02-04T13:14')", "2022-12-31T23:59:59.999999999");
        // lengthOfMonth()
        evaluate.accept("'2022-01-02T03:04'.lengthOfMonth()", "31");
        evaluate.accept("lengthOfMonth('2022-02-04T13:14')", "28");
        // lengthOfYear()
        evaluate.accept("'2022-01-02T03:04'.lengthOfYear()", "365");
        evaluate.accept("lengthOfYear('2024-02-04T13:14')", "366");
        // localToOffsetDate()
        evaluate.accept("'2022-01-02T03:04:05'.localToOffsetDate()", "2022-01-02T03:04:05+08:00");
        evaluate.accept("localToOffsetDate('2022-02-04T13:14:15')", "2022-02-04T13:14:15+08:00");
        // offsetToLocalDate()
        evaluate.accept("'2022-01-02T03:04:05+08:00'.offsetToLocalDate()", "2022-01-02T03:04:05");
        evaluate.accept("offsetToLocalDate('2022-02-04T13:14:15+08:00')", "2022-02-04T13:14:15");

        // Format functions
        // b64Encode()
        evaluate.accept("'abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ'.b64Encode()",
                "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg==");
        // b64EncodeNoPadding()
        evaluate.accept("b64EncodeNoPadding('abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ')",
                "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg");
        // b64MimeEncode() - Split lines into 76 character wide chunks
        evaluate.accept("'abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ'.b64MimeEncode()",
                "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9Q\r\nUVJTVFVWV1hZWg==");
        // b64MimeEncodeNoPadding()
        evaluate.accept("b64MimeEncodeNoPadding('abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ')",
                "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9Q\r\nUVJTVFVWV1hZWg");
        // b64UrlEncode()
        evaluate.accept("'abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ'.b64UrlEncode()",
                "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp-IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg==");
        // b64UrlEncodeNoPadding()
        evaluate.accept("b64UrlEncodeNoPadding('abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ')",
                "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp-IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg");
        // b64Decode()
        evaluate.accept("'YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg=='.b64Decode()",
                "abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ");
        evaluate.accept("b64Decode('YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg')",
                "abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ");
        // b64MimeDecode()
        evaluate.accept("'YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9Q\nUVJTVFVWV1hZWg=='.b64MimeDecode()",
                "abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ");
        evaluate.accept("b64MimeDecode('YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9Q\r\nUVJTVFVWV1hZWg')",
                "abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ");
        // b64UrlDecode()
        evaluate.accept("'YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp-IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg=='.b64UrlDecode()",
                "abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ");
        evaluate.accept("b64UrlDecode('YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp-IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg')",
                "abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ");
        // urlEncode()
        evaluate.accept("'www.domain.com?a=1+2&b=3+4'.urlEncode()", "www.domain.com%3Fa%3D1%2B2%26b%3D3%2B4");
        evaluate.accept("urlEncode('www.domain.com?a=1+2&b=3+4')", "www.domain.com%3Fa%3D1%2B2%26b%3D3%2B4");
        // urlDecode()
        evaluate.accept("'www.domain.com%3Fa%3D1%2B2%26b%3D3%2B4'.urlDecode()", "www.domain.com?a=1+2&b=3+4");
        evaluate.accept("urlDecode('www.domain.com%3Fa%3D1%2B2%26b%3D3%2B4')", "www.domain.com?a=1+2&b=3+4");
        // caseValue()
        evaluate.accept("'a'.caseValue('c',1,'b',2,'a',3,4)", "3");
        evaluate.accept("'z'.caseValue('c',1,'b',2,'a',3,4)", "4");
        evaluate.accept("'z'.caseValue('c',1,'b',2,'a',3)", "!unresolvable!");
        evaluate.accept("json('[{\"s\":1},{\"s\":null},{\"s\":3}]').s.caseValue(1,'A',null,'B')", "[ \"A\", \"B\", null ]");
        // indexedValue()
        evaluate.accept("0.indexedValue('a','b','c','d')", "a");
        evaluate.accept("1.indexedValue(json('[\"a\",\"b\",\"c\",\"d\"]'))", "b");
        evaluate.accept("'3'.indexedValue('a','b','c','d')", "d");
        evaluate.accept("4.indexedValue('a','b','c','d')", "!unresolvable!");
        evaluate.accept("-1.indexedValue('a','b','c','d')", "!unresolvable!");
        // cycleValue()
        evaluate.accept("0.cycleValue('a','b','c','d')", "a");
        evaluate.accept("1.cycleValue(json('[\"a\",\"b\",\"c\",\"d\"]'))", "b");
        evaluate.accept("'3'.cycleValue('a','b','c','d')", "d");
        evaluate.accept("4.cycleValue('a','b','c','d')", "a");
        evaluate.accept("-1.cycleValue('a','b','c','d')", "d");
        evaluate.accept("-6.cycleValue('a','b','c','d')", "c");
        // formatDate()
        evaluate.accept("'2022-01-02T03:04:05'.formatDate('dd/MM/yyyy HH:mm:ss')", "02/01/2022 03:04:05");
        evaluate.accept("'2022-01-02T03:04:05'.formatDate(?, 'yyyy-MM-dd')", "2022-01-02");
        evaluate.accept("formatDate('2022-01-02T03:04:05', 'EEE, MMM d, yyyy')", "Sun, Jan 2, 2022");
        // formatNumber()
        evaluate.accept("12345.6.formatNumber('HK$#,##0.00')", "HK$12,345.60");
        evaluate.accept("123.formatNumber(?, '#,##0.#')", "123");
        evaluate.accept("formatNumber(123.45, '#,##0.#')", "123.5");
        // formatText()
        evaluate.accept("'Dog'.formatText('[%-5s]')", "[Dog  ]");
        evaluate.accept("123.formatText(?, '[%5d]')", "[  123]");
        evaluate.accept("formatText('Dog', '[%5s]')", "[  Dog]");
        // formatTexts()
        evaluate.accept("formatTexts('1:%s 2:%s 3:%s', 'a', 'b', 'c')", "1:a 2:b 3:c");
        evaluate.accept("'b'.formatTexts('1:%s 2:%s 3:%s', 'a', ?, 'c')", "1:a 2:b 3:c");
        evaluate.accept("json('{\"A\":\"a\",\"B\":\"b\"}').formatTexts('1:%s 2:%s 3:%s', A, B, 'c')", "1:a 2:b 3:c");
        // toNumber()
        evaluate.accept("'123'.toNumber()", "123.0");
        evaluate.accept("toNumber('abc')", "0.0");
        evaluate.accept("toNumber(true)", "1.0");
        evaluate.accept("toNumber(null)", "!unresolvable!");
        evaluate.accept("toNumber(json('{\"a\":1}'))", "!unresolvable!");
        evaluate.accept("toNumber(json('[1,2.0,\"a\",true,null]'))", "[ 1, 2.0, 0.0, 1.0, null ]");
        // toString()
        evaluate.accept("123.toString()", "123");
        evaluate.accept("toString(false)", "false");
        evaluate.accept("toString(null)", "null");
        evaluate.accept("toString(json('{\"a\":1}'))", "{\"a\":1}");
        evaluate.accept("toString(json('[1,2.0,\"a\",true,null]'))", "[1,2.0,\"a\",true,null]");
        // toText()
        evaluate.accept("123.toText()", "123");
        evaluate.accept("toText(false)", "false");
        evaluate.accept("toText(null)", "null");
        evaluate.accept("toText(json('{\"a\":1}'))", "!unresolvable!");
        evaluate.accept("toText(json('[1,2.0,\"a\",true,null]'))", "[ \"1\", \"2.0\", \"a\", \"true\", \"null\" ]");

        // Logical
        // contains()
        evaluate.accept("'abcde'.contains('bc')", "true");
        evaluate.accept("contains('abcde','B')", "false");
        evaluate.accept("json('[1.0,2.8,3.0]').contains(?, '1')", "false");
        evaluate.accept("json('[1.0,2.8,3.0]').contains(1)", "true");
        evaluate.accept("contains(json('[\"1\",\"2\",\"3\"]'), 2.0)", "true");
        evaluate.accept("contains(json('[1.0,2.8,3.00]'), '3.0')", "true");
        evaluate.accept("json('[\"1.0\",\"2.0\",\"3.0\"]').contains(?, '3.0')", "true");
        evaluate.accept("json('[1,2,null,4]').contains(null)", "true");
        evaluate.accept("json('{\"a\":1,\"b\":2,\"c\":3}').contains('a')", "true");
        // containsIgnoreCase()
        evaluate.accept("'abcde'.containsIgnoreCase('bc')", "true");
        evaluate.accept("containsIgnoreCase('abcde','B')", "true");
        evaluate.accept("json('[\"a\",\"b\",\"c\"]').containsIgnoreCase(?, 'B')", "true");
        evaluate.accept("containsIgnoreCase(json('[\"a\",\"b\",\"c\"]'), 'bc')", "false");
        evaluate.accept("json('{\"a\":1,\"b\":2,\"c\":3}').containsIgnoreCase('A')", "true");
        // notContains()
        evaluate.accept("'abcde'.notContains('bc')", "false");
        evaluate.accept("notContains('abcde','B')", "true");
        evaluate.accept("json('[1.0,2.8,3.0]').notContains(?, 1)", "false");
        evaluate.accept("json('[1,2,null,4]').notContains(null)", "false");
        // notContainsIgnoreCase()
        evaluate.accept("'abcde'.notContainsIgnoreCase('bc')", "false");
        evaluate.accept("notContainsIgnoreCase('abcde','B')", "false");
        evaluate.accept("json('[\"a\",\"b\",\"c\"]').notContainsIgnoreCase(?, 'D')", "true");
        // startsWith()
        evaluate.accept("'abcdef'.startsWith('abc')", "true");
        evaluate.accept("'ABCDEF'.startsWith(?,'abc')", "false");
        evaluate.accept("startsWith('ABCDEF','cde')", "false");
        // startsWithIgnoreCase()
        evaluate.accept("'abcdef'.startsWithIgnoreCase('abc')", "true");
        evaluate.accept("'ABCDEF'.startsWithIgnoreCase(?,'abc')", "true");
        evaluate.accept("startsWithIgnoreCase('ABCDEF','cde')", "false");
        // notStartsWith()
        evaluate.accept("'abcdef'.notStartsWith('abc')", "false");
        evaluate.accept("'ABCDEF'.notStartsWith(?,'abc')", "true");
        evaluate.accept("notStartsWith('ABCDEF','cde')", "true");
        // notStartsWithIgnoreCase()
        evaluate.accept("'abcdef'.notStartsWithIgnoreCase('abc')", "false");
        evaluate.accept("'ABCDEF'.notStartsWithIgnoreCase(?,'abc')", "false");
        evaluate.accept("notStartsWithIgnoreCase('ABCDEF','cde')", "true");
        // endsWith()
        evaluate.accept("'abcdef'.endsWith('def')", "true");
        evaluate.accept("'ABCDEF'.endsWith(?,'def')", "false");
        evaluate.accept("endsWith('ABCDEF','cde')", "false");
        // endsWithIgnoreCase()
        evaluate.accept("'abcdef'.endsWithIgnoreCase('def')", "true");
        evaluate.accept("'ABCDEF'.endsWithIgnoreCase(?,'def')", "true");
        evaluate.accept("endsWithIgnoreCase('ABCDEF','cde')", "false");
        // notEndsWith()
        evaluate.accept("'abcdef'.notEndsWith('def')", "false");
        evaluate.accept("'ABCDEF'.notEndsWith(?,'def')", "true");
        evaluate.accept("notEndsWith('ABCDEF','cde')", "true");
        // notEndsWithIgnoreCase()
        evaluate.accept("'abcdef'.notEndsWithIgnoreCase('def')", "false");
        evaluate.accept("'ABCDEF'.notEndsWithIgnoreCase(?,'def')", "false");
        evaluate.accept("notEndsWithIgnoreCase('ABCDEF','cde')", "true");
        // equals()
        evaluate.accept("'abc'.equals('abc')", "true");
        evaluate.accept("'abc'.equals(?, ' abc')", "false");
        evaluate.accept("equals('ABC','abc')", "false");
        // equalsIgnoreCase()
        evaluate.accept("'abc'.equalsIgnoreCase('abc')", "true");
        evaluate.accept("'abc'.equalsIgnoreCase(?, ' abc')", "false");
        evaluate.accept("equalsIgnoreCase('ABC','abc')", "true");
        // notEquals()
        evaluate.accept("'abc'.notEquals('abc')", "false");
        evaluate.accept("'abc'.notEquals(?, ' abc')", "true");
        evaluate.accept("notEquals('ABC','abc')", "true");
        // notEqualsIgnoreCase()
        evaluate.accept("'abc'.notEqualsIgnoreCase('abcd')", "true");
        evaluate.accept("'abc'.notEqualsIgnoreCase(' abc')", "true");
        evaluate.accept("notEqualsIgnoreCase('ABC','abc')", "false");
        // in()
        evaluate.accept("56.in(12,34,56)", "true");
        evaluate.accept("'56'.in(12,34,56)", "true");
        evaluate.accept("'A'.in(json('[\"a\",\"b\",\"c\"]'))", "false");
        // inIgnoreCase()
        evaluate.accept("'A'.inIgnoreCase('a','b','c')", "true");
        evaluate.accept("'a '.inIgnoreCase('a','b','c')", "false");
        // notIn()
        evaluate.accept("56.notIn(12,34,56)", "false");
        evaluate.accept("'56'.notIn(12,34,56)", "false");
        evaluate.accept("'A'.notIn(json('[\"a\",\"b\",\"c\"]'))", "true");
        // notInIgnoreCase()
        evaluate.accept("'A'.notInIgnoreCase('a','b','c')", "false");
        evaluate.accept("'a '.notInIgnoreCase('a','b','c')", "true");
        // isEmpty()
        evaluate.accept("''.isEmpty()", "true");
        evaluate.accept("isEmpty(' ')", "false");
        // isNotEmpty()
        evaluate.accept("''.isNotEmpty()", "false");
        evaluate.accept("isNotEmpty(' ')", "true");
        // isBlank()
        evaluate.accept("''.isBlank()", "true");
        evaluate.accept("isBlank(' ')", "true");
        // isNotBlank()
        evaluate.accept("''.isNotBlank()", "false");
        evaluate.accept("isNotBlank(' ')", "false");
        // isNull()
        evaluate.accept("null.isNull()", "!unresolvable!");
        evaluate.accept("isNull(null)", "true");
        evaluate.accept("isNull('')", "false");
        // isNotNull()
        evaluate.accept("null.isNotNull()", "!unresolvable!");
        evaluate.accept("isNotNull(null)", "false");
        evaluate.accept("isNotNull('')", "true");
        // isText()
        evaluate.accept("'text'.isText()", "true");
        evaluate.accept("isText(1)", "false");
        evaluate.accept("isText(true)", "false");
        evaluate.accept("isText(null)", "false");
        // isBoolean()
        evaluate.accept("'text'.isBoolean()", "false");
        evaluate.accept("isBoolean(1)", "false");
        evaluate.accept("isBoolean(true)", "true");
        evaluate.accept("isBoolean(null)", "false");
        // isNumber()
        evaluate.accept("'text'.isNumber()", "false");
        evaluate.accept("isNumber(1)", "true");
        evaluate.accept("isNumber(true)", "false");
        evaluate.accept("isNumber(null)", "false");
        // isEven()
        evaluate.accept("1.isEven()", "false");
        evaluate.accept("isEven(2)", "true");
        // isOdd()
        evaluate.accept("1.isOdd()", "true");
        evaluate.accept("isOdd(2)", "false");
        // not()
        evaluate.accept("true.not()", "false");
        evaluate.accept("not(false)", "true");
        evaluate.accept("not('false')", "false");
        evaluate.accept("not(0)", "false");
        evaluate.accept("not(null)", "false");
        // isWeekday
        evaluate.accept("'2021-12-31T00:00:00'.isWeekday()", "true");
        evaluate.accept("isWeekday('2022-01-01T00:00:00')", "false");
        // isWeekend
        evaluate.accept("'2021-12-31T00:00:00'.isWeekend()", "false");
        evaluate.accept("isWeekend('2022-01-01T00:00:00')", "true");
        // isLeapYear
        evaluate.accept("'2020-12-31T00:00:00'.isLeapYear()", "true");
        evaluate.accept("isLeapYear('2022-01-01T00:00:00')", "false");

        // Array functions
        // size()
        evaluate.accept("json('[7,1,9,null,5,3]').size()", "6");
        evaluate.accept("size(json('[7,1,9,null,5,3]'))", "6");
        // lastIndex()
        evaluate.accept("json('[7,1,9,null,5,3]').lastIndex()", "5");
        evaluate.accept("lastIndex(json('[7,1,9,null,5,3]'))", "5");
        // indexOf()
        evaluate.accept("json('[1,1,3,5,null,3,7,3,9]').indexOf(3)", "2");
        evaluate.accept("json('[1,1,3,5,null,3,7,3,9]').indexOf(?, 1)", "0");
        evaluate.accept("indexOf(json('[1,1,3,5,null,3,7,3,9]'), null)", "4");
        // lastIndexOf()
        evaluate.accept("json('[1,1,3,5,null,3,7,3,9]').lastIndexOf(3)", "7");
        evaluate.accept("json('[1,1,3,5,null,3,7,3,9]').lastIndexOf(?, 1)", "1");
        evaluate.accept("lastIndexOf(json('[1,1,3,5,null,3,7,3,9]'), null)", "4");
        // first()
        evaluate.accept("json('[7,1,9,null,5,3]').first()", "7");
        evaluate.accept("first(json('[null,7,1,9,5,3]'))", "null");
        // last()
        evaluate.accept("json('[7,1,9,null,5,3]').last()", "3");
        evaluate.accept("last(json('[7,1,9,5,3,null]'))", "null");
        // max()
        evaluate.accept("json('[7,1,9,null,5,3]').max()", "9");
        evaluate.accept("max(json('[7,1,9,null,5,3]'), 15, 16)", "16");
        // min()
        evaluate.accept("json('[7,1,9,null,5,3]').min()", "1");
        evaluate.accept("min(json('[7,1,9,null,5,3]'), 15, 16)", "1");
        // sum()
        evaluate.accept("json('[7,1,9,null,5,3]').sum()", "25.0");
        evaluate.accept("sum(json('[7,1,9,null,5,3]'), 15, 16)", "56.0");
        // avg()
        evaluate.accept("json('[7,1,9,null,5,3]').avg()", "5.0");
        evaluate.accept("avg(json('[7,1,9,null,5,3]'), 15, 16)", "8.0");
        // count()
        evaluate.accept("json('[7,1,9,null,5,3]').count()", "5");
        evaluate.accept("count(json('[7,1,9,null,5,3]'), 15, 16)", "7");
        // reverse()
        evaluate.accept("json('[7,1,9,null,5,3]').reverse()", "[ 3, 5, null, 9, 1, 7 ]");
        evaluate.accept("reverse(json('[7,1,9,null,5,3]'))", "[ 3, 5, null, 9, 1, 7 ]");
        // slice()
        evaluate.accept("json('[1,2,3,4,5,6,7,8,9]').slice(3)", "[ 4, 5, 6, 7, 8, 9 ]");
        evaluate.accept("json('[1,2,3,4,5,6,7,8,9]').slice(2,8)", "[ 3, 4, 5, 6, 7, 8 ]");
        evaluate.accept("json('[1,2,3,4,5,6,7,8,9]').slice(,5)", "[ 1, 2, 3, 4, 5 ]");
        evaluate.accept("json('[1,2,3,4,5,6,7,8,9]').slice(-5)", "[ 5, 6, 7, 8, 9 ]");
        evaluate.accept("json('[1,2,3,4,5,6,7,8,9]').slice(?,1,8,2)", "[ 2, 4, 6, 8 ]");
        evaluate.accept("json('[1,2,3,4,5,6,7,8,9]').slice(?,2,,2)", "[ 3, 5, 7, 9 ]");
        evaluate.accept("slice(json('[1,2,3,4,5,6,7,8,9]'),6,2,1)", "[ 7, 6, 5, 4 ]");
        evaluate.accept("slice(json('[1,2,3,4,5,6,7,8,9]'),,,3)", "[ 1, 4, 7 ]");
        evaluate.accept("slice(json('[1,2,3,4,5,6,7,8,9]'),,-5,1)", "[ 1, 2, 3, 4 ]");
        // sort()
        evaluate.accept("json('[1,1,3,5,3,7,3,9]').sort()", "[ 1, 1, 3, 3, 3, 5, 7, 9 ]");
        evaluate.accept("json('[1,1,3,5,3,7,3,9]').sort(?,-1)", "[ 9, 7, 5, 3, 3, 3, 1, 1 ]");
        evaluate.accept("json('[{\"seq\":4,\"val\":\"A\"},{\"seq\":1,\"val\":\"B\"},{\"seq\":3,\"val\":\"C\"},{\"seq\":2,\"val\":\"D\"}]').sort(seq)",
                "[ {\n" +
                        "  \"seq\" : 1,\n" +
                        "  \"val\" : \"B\"\n" +
                        "}, {\n" +
                        "  \"seq\" : 2,\n" +
                        "  \"val\" : \"D\"\n" +
                        "}, {\n" +
                        "  \"seq\" : 3,\n" +
                        "  \"val\" : \"C\"\n" +
                        "}, {\n" +
                        "  \"seq\" : 4,\n" +
                        "  \"val\" : \"A\"\n" +
                        "} ]");
        evaluate.accept("json('[{\"seq\":4,\"val\":\"A\"},{\"seq\":1,\"val\":\"B\"},{\"seq\":3,\"val\":\"C\"},{\"seq\":2,\"val\":\"D\"}]').sort(seq,-1)",
                "[ {\n" +
                        "  \"seq\" : 4,\n" +
                        "  \"val\" : \"A\"\n" +
                        "}, {\n" +
                        "  \"seq\" : 3,\n" +
                        "  \"val\" : \"C\"\n" +
                        "}, {\n" +
                        "  \"seq\" : 2,\n" +
                        "  \"val\" : \"D\"\n" +
                        "}, {\n" +
                        "  \"seq\" : 1,\n" +
                        "  \"val\" : \"B\"\n" +
                        "} ]");
        // distinct()
        evaluate.accept("json('[1,1,3,5,3,7,3,9]').distinct().sort()", "[ 1.0, 3.0, 5.0, 7.0, 9.0 ]");
        evaluate.accept("distinct(json('[\"A\",\"Z\",\"a\",\"Z\",\"A\",\"z\"]'))", "[ \"A\", \"a\", \"Z\", \"z\" ]");
        evaluate.accept("distinct(json('[\"1\",\"1.0\",1,1.0,1.00,true,\"true\",null,\"null\"]'))",
                "[ \"1\", \"1.0\", \"null\", \"true\", 1.0, true ]");
        // join()
        evaluate.accept("json('[\"Hello\", \",\", \"World\", \"!\"]').join()", "Hello,World!");
        evaluate.accept("json('[1,2,3]').join('+')", "1+2+3");
        evaluate.accept("join(json('[\"A\",1,\"B\",\"2.00\",\"C\",3.00,\"D\",true,null]'),'/')", "A/1/B/2.00/C/3.0/D/true");
        // findByMax()
        evaluate.accept("json('[{\"code\":\"A\",\"price\":8},{\"code\":\"B\"},{\"code\":\"C\",\"price\":3},{\"code\":\"D\",\"price\":8},{\"code\":\"E\",\"price\":5}]').findByMax(price)",
                "{\n" +
                        "  \"code\" : \"A\",\n" +
                        "  \"price\" : 8\n" +
                        "}");
        evaluate.accept("findByMax(json('[{\"code\":\"A\",\"price\":8},{\"code\":\"B\"},{\"code\":\"C\",\"price\":3},{\"code\":\"D\",\"price\":8},{\"code\":\"E\",\"price\":5}]'), code)",
                "{\n" +
                        "  \"code\" : \"E\",\n" +
                        "  \"price\" : 5\n" +
                        "}");
        // findByMin()
        evaluate.accept("json('[{\"code\":\"A\",\"price\":8},{\"code\":\"B\"},{\"code\":\"C\",\"price\":3},{\"code\":\"D\",\"price\":8},{\"code\":\"E\",\"price\":5}]').findByMin(?,price)",
                "{\n" +
                        "  \"code\" : \"C\",\n" +
                        "  \"price\" : 3\n" +
                        "}");
        evaluate.accept("findByMin(json('[{\"code\":\"A\",\"price\":8},{\"code\":\"B\"},{\"code\":\"C\",\"price\":3},{\"code\":\"D\",\"price\":8},{\"code\":\"E\",\"price\":5}]'), code)",
                "{\n" +
                        "  \"code\" : \"A\",\n" +
                        "  \"price\" : 8\n" +
                        "}");
        // findByNullOrMax()
        evaluate.accept("json('[{\"code\":\"A\",\"price\":8},{\"code\":\"B\"},{\"code\":\"C\",\"price\":3},{\"code\":\"D\",\"price\":8},{\"code\":\"E\",\"price\":5}]').findByNullOrMax(price)",
                "{\n" +
                        "  \"code\" : \"B\"\n" +
                        "}");
        evaluate.accept("findByNullOrMax(json('[{\"code\":\"A\",\"price\":8},{\"code\":\"B\"},{\"code\":\"C\",\"price\":3},{\"code\":\"D\",\"price\":8},{\"code\":\"E\",\"price\":5}]'), code)",
                "{\n" +
                        "  \"code\" : \"E\",\n" +
                        "  \"price\" : 5\n" +
                        "}");
        // findByNullOrMin()
        evaluate.accept("json('[{\"code\":\"A\",\"price\":8},{\"code\":\"B\"},{\"code\":\"C\",\"price\":3},{\"code\":\"D\",\"price\":8},{\"code\":\"E\",\"price\":5}]').findByNullOrMin(?,price)",
                "{\n" +
                        "  \"code\" : \"B\"\n" +
                        "}");
        evaluate.accept("findByNullOrMin(json('[{\"code\":\"A\",\"price\":8},{\"code\":\"B\"},{\"code\":\"C\",\"price\":3},{\"code\":\"D\",\"price\":8},{\"code\":\"E\",\"price\":5}]'), code)",
                "{\n" +
                        "  \"code\" : \"A\",\n" +
                        "  \"price\" : 8\n" +
                        "}");
        // findByMaxOrNull()
        evaluate.accept("json('[{\"code\":\"A\",\"price\":8},{\"code\":\"B\"},{\"code\":\"C\",\"price\":3},{\"code\":\"D\",\"price\":8},{\"code\":\"E\",\"price\":5}]').findByMaxOrNull(price)",
                "{\n" +
                        "  \"code\" : \"A\",\n" +
                        "  \"price\" : 8\n" +
                        "}");
        evaluate.accept("findByMaxOrNull(json('[{\"code\":\"A\",\"price\":8},{\"code\":\"B\"},{\"code\":\"C\",\"price\":3},{\"code\":\"D\",\"price\":8},{\"code\":\"E\",\"price\":5}]'), code)",
                "{\n" +
                        "  \"code\" : \"E\",\n" +
                        "  \"price\" : 5\n" +
                        "}");
        // findByMinOrNull()
        evaluate.accept("json('[{\"code\":\"A\",\"price\":8},{\"code\":\"B\"},{\"code\":\"C\",\"price\":3},{\"code\":\"D\",\"price\":8},{\"code\":\"E\",\"price\":5}]').findByMinOrNull(?,price)",
                "{\n" +
                        "  \"code\" : \"C\",\n" +
                        "  \"price\" : 3\n" +
                        "}");
        evaluate.accept("findByMinOrNull(json('[{\"code\":\"A\",\"price\":8},{\"code\":\"B\"},{\"code\":\"C\",\"price\":3},{\"code\":\"D\",\"price\":8},{\"code\":\"E\",\"price\":5}]'), code)",
                "{\n" +
                        "  \"code\" : \"A\",\n" +
                        "  \"price\" : 8\n" +
                        "}");

        // Structural
        // json()
        evaluate.accept("json('[1,\"2\",{\"a\":1,\"b\":2}]')",
                "[ 1, \"2\", {\n" +
                        "  \"a\" : 1,\n" +
                        "  \"b\" : 2\n" +
                        "} ]");
        evaluate.accept("'{\"a\":1,\"b\":[2,3],\"c\":{\"d\":4,\"e\":5}}'.json()",
                "{\n" +
                        "  \"a\" : 1,\n" +
                        "  \"b\" : [ 2, 3 ],\n" +
                        "  \"c\" : {\n" +
                        "    \"d\" : 4,\n" +
                        "    \"e\" : 5\n" +
                        "  }\n" +
                        "}");
        // entries()
        evaluate.accept("json('{\"a\":1,\"b\":[2,3],\"c\":{\"d\":4,\"e\":5}}').entries()",
                "[ {\n" +
                        "  \"key\" : \"a\",\n" +
                        "  \"value\" : 1\n" +
                        "}, {\n" +
                        "  \"key\" : \"b\",\n" +
                        "  \"value\" : [ 2, 3 ]\n" +
                        "}, {\n" +
                        "  \"key\" : \"c\",\n" +
                        "  \"value\" : {\n" +
                        "    \"d\" : 4,\n" +
                        "    \"e\" : 5\n" +
                        "  }\n" +
                        "} ]");
        // keys()
        evaluate.accept("json('{\"a\":1,\"b\":[2,3],\"c\":{\"d\":4,\"e\":5}}').keys()",
                "[ \"a\", \"b\", \"c\" ]");
        evaluate.accept("json('{\"a\":1,\"b\":[2,3],\"c\":{\"d\":4,\"e\":5}}').keys(2)",
                "[ \"a\", \"b\", \"c\", \"d\", \"e\" ]");
        evaluate.accept("keys(json('{\"a\":1,\"b\":[2,3],\"c\":{\"d\":4,\"e\":5}}'), -1)",
                "[ \"a\", \"b\", \"c\", \"d\", \"e\" ]");
        // toArray()
        evaluate.accept("json('{\"a\":1,\"b\":[2,3],\"c\":{\"d\":4,\"e\":5}}').toArray()",
                "[ 1, [ 2, 3 ], {\n" +
                        "  \"d\" : 4,\n" +
                        "  \"e\" : 5\n" +
                        "} ]");
        evaluate.accept("json('{\"a\":1,\"b\":[2,3],\"c\":{\"d\":4,\"e\":5}}').toArray(c)",
                "[ 4, 5 ]");
        evaluate.accept("toArray(json('{\"a\":1,\"b\":[2,3],\"c\":{\"d\":4,\"e\":5}}').toArray())",
                "[ 1, 2, 3, 4, 5 ]");
        // flatten()
        evaluate.accept("json('[[[[1,2],[3,4]],[[5,6],[7,8]]],[[[9,10],[11,12]],[[13,14],[15,16]]]]').flatten()",
                "[ [ [ 1, 2 ], [ 3, 4 ] ], [ [ 5, 6 ], [ 7, 8 ] ], [ [ 9, 10 ], [ 11, 12 ] ], [ [ 13, 14 ], [ 15, 16 ] ] ]");
        evaluate.accept("json('[[[[1,2],[3,4]],[[5,6],[7,8]]],[[[9,10],[11,12]],[[13,14],[15,16]]]]').flatten(2)",
                "[ [ 1, 2 ], [ 3, 4 ], [ 5, 6 ], [ 7, 8 ], [ 9, 10 ], [ 11, 12 ], [ 13, 14 ], [ 15, 16 ] ]");
        evaluate.accept("flatten(json('[[[[1,2],[3,4]],[[5,6],[7,8]]],[[[9,10],[11,12]],[[13,14],[15,16]]]]'), 3)",
                "[ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 ]");
        // map()
        evaluate.accept("json('{\"a\":1,\"b\":[2,3],\"c\":{\"d\":4,\"e\":5}}').map(c.e,c.d,b,a)",
                "{\n" +
                        "  \"e\" : 5,\n" +
                        "  \"d\" : 4,\n" +
                        "  \"b\" : [ 2, 3 ],\n" +
                        "  \"a\" : 1\n" +
                        "}");
        evaluate.accept("json('{\"a\":1,\"b\":[2,3],\"c\":{\"d\":4,\"e\":5}}').map(cc:c.map(dd:d,ee:e),xx:map(aa:a,bb:b))",
                "{\n" +
                        "  \"cc\" : {\n" +
                        "    \"dd\" : 4,\n" +
                        "    \"ee\" : 5\n" +
                        "  },\n" +
                        "  \"xx\" : {\n" +
                        "    \"aa\" : 1,\n" +
                        "    \"bb\" : [ 2, 3 ]\n" +
                        "  }\n" +
                        "}");
        // field()
        evaluate.accept("json('{\"a\":1,\"b\":[2,3],\"c\":{\"d\":4,\"e\":5}}').field(f:6,c:)",
                "{\n" +
                        "  \"a\" : 1,\n" +
                        "  \"b\" : [ 2, 3 ],\n" +
                        "  \"f\" : 6\n" +
                        "}");
        // coalesce()
        evaluate.accept("json('[\"abc\",\"\",123,false,null]').coalesce('xyz')", "[ \"abc\", \"\", 123, false, \"xyz\" ]");
        evaluate.accept("json('{\"a\":null,\"c\":\"abc\"}').coalesce(a,b,c,'xyz')", "abc");
        // csv()
        evaluate.accept("json('{\"len1\":\"12.3\\\"\",\"len2\":\"26.1\\\"\",\"len3\":\"64.0\\\"\"}').csv()",
                "\"12.3\"\"\",\"26.1\"\"\",\"64.0\"\"\"");
        evaluate.accept("csv(json('[[[[1,2],[\"3\",\"4\\\"\"]]],{\"a\":1,\"b\":[2.0,8.888],\"c\":{\"d\":true,\"e\":null}}]'))",
                "1,2,3,\"4\"\"\",1,2.0,8.888,true");
    }

    @Test
    public void testJossons() throws Exception {
        Josson.setLocale(Locale.ENGLISH);
        Josson.setZoneId(ZoneId.of("Asia/Hong_Kong"));
        Josson.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        Jossons jossons = Jossons.fromJsonString("{\n" +
                "    \"order\": {\n" +
                "        \"salesOrderId\": \"SO0001\",\n" +
                "        \"salesDate\": \"2022-01-01T10:01:23\",\n" +
                "        \"salesPerson\": \"Raymond\",\n" +
                "        \"customer\": {\n" +
                "            \"customerId\": \"CU0001\",\n" +
                "            \"name\": \"Peggy\",\n" +
                "            \"phone\": \"+852 62000610\"\n" +
                "        },\n" +
                "        \"items\": [\n" +
                "            {\n" +
                "                \"itemCode\": \"B00001\",\n" +
                "                \"name\": \"WinWin TShirt Series A - 2022\",\n" +
                "                \"brand\": \"WinWin\",\n" +
                "                \"property\": {\n" +
                "                    \"size\": \"M\",\n" +
                "                    \"colors\": [\n" +
                "                        \"WHITE\",\n" +
                "                        \"RED\"\n" +
                "                    ]\n" +
                "                },\n" +
                "                \"qty\": 2,\n" +
                "                \"unit\": \"Pcs\",\n" +
                "                \"unitPrice\": 15.0,\n" +
                "                \"tags\": [\n" +
                "                    \"SHIRT\",\n" +
                "                    \"WOMEN\"\n" +
                "                ]\n" +
                "            },\n" +
                "            {\n" +
                "                \"itemCode\": \"A00308\",\n" +
                "                \"name\": \"OctoPlus Tennis Racket - Star\",\n" +
                "                \"brand\": \"OctoPlus\",\n" +
                "                \"property\": {\n" +
                "                    \"colors\": [\n" +
                "                        \"BLACK\"\n" +
                "                    ]\n" +
                "                },\n" +
                "                \"qty\": 1,\n" +
                "                \"unit\": \"Pcs\",\n" +
                "                \"unitPrice\": 150.0,\n" +
                "                \"unitDiscount\": 10.0,\n" +
                "                \"tags\": [\n" +
                "                    \"TENNIS\",\n" +
                "                    \"SPORT\",\n" +
                "                    \"RACKET\"\n" +
                "                ]\n" +
                "            },\n" +
                "            {\n" +
                "                \"itemCode\": \"A00201\",\n" +
                "                \"name\": \"WinWin Sport Shoe - Super\",\n" +
                "                \"brand\": \"WinWin\",\n" +
                "                \"property\": {\n" +
                "                    \"size\": \"35\",\n" +
                "                    \"colors\": [\n" +
                "                        \"RED\"\n" +
                "                    ]\n" +
                "                },\n" +
                "                \"qty\": 1,\n" +
                "                \"unit\": \"Pair\",\n" +
                "                \"unitPrice\": 110.0,\n" +
                "                \"unitDiscount\": 10.0,\n" +
                "                \"tags\": [\n" +
                "                    \"SHOE\",\n" +
                "                    \"SPORT\",\n" +
                "                    \"WOMEN\"\n" +
                "                ]\n" +
                "            }\n" +
                "        ],\n" +
                "        \"totalAmount\": 270.0,\n" +
                "        \"discountPct\": 5.0,\n" +
                "        \"netAmount\": 256.5,\n" +
                "        \"delivery\": {\n" +
                "            \"handlingFee\": 5.0,\n" +
                "            \"address\": \"Wo Mun Street,\\nFanling, N.T.,\\nHong Kong\",\n" +
                "            \"contactPerson\": \"Cyron\",\n" +
                "            \"phone\": \"+852 26004198\"\n" +
                "        }\n" +
                "    },\n" +
                "    \"company\": {\n" +
                "        \"name\": \"Octomix Limited\",\n" +
                "        \"phone\": \"+852 12345678\",\n" +
                "        \"website\": \"www.octomix.com\",\n" +
                "        \"address\": [\n" +
                "            \"888 Queen's Road East\",\n" +
                "            \"Hong Kong\"\n" +
                "        ]\n" +
                "    }\n" +
                "}");
        System.out.println(jossons.fillInPlaceholder(
                "{{company->name.rightPad(65)}}INVOICE\n\n" +
                        "{{company->address[0].rightPad(56) ?: $->repeat(' ',56)}}Issue Date: {{order->salesDate.formatDate('dd/MM/yyyy')}}\n" +
                        "{{company->address[1].rightPad(58) ?: $->repeat(' ',58)}}Invoice#: {{order->salesOrderId.center(10)}}\n" +
                        "Phone: {{company->phone.rightPad(48)}}Customer ID: {{order->customer.customerId.center(10)}}\n" +
                        "Website: {{company->website.rightPad(49)}}Due Date: {{order->salesDate.plusMonths(1).formatDate('dd/MM/yyyy')}}\n\n" +
                        "BILL TO                        {{order->delivery!=null ? 'SHIP TO'}}\n" +
                        "{{order->customer.name.rightPad(30)}} {{order->delivery!=null ? order->coalesce(delivery.contactPerson,customer.name)}}\n" +
                        "{{order->customer.coalesce(phone,'N/A').concat('Phone: ',?).rightPad(30)}} " +
                        "{{order->delivery!=null ? order->coalesce(delivery.phone,customer.phone,'N/A').concat('Phone: ',?)}}\n" +
                        "{{order->delivery.address!=null ? order->delivery.address.split('\n').concat(repeat(' ',31),?).join('\n').concat(?,'\n')}}\n" +
                        "Item# Description                         Quantity Unit Price Discount    Total\n" +
                        "----- ----------------------------------- -------- ---------- -------- --------\n" +
                        "{{order->items.concat(\n" +
                        "    ##.center(5),' ',\n" +
                        "    name.rightPad(35),' ',\n" +
                        "    concat(qty,' ',unit).center(8),' ',\n" +
                        "    unitPrice.formatNumber('#,##0.0').leftPad(9),' ',\n" +
                        "    coalesce(unitDiscount,0).formatNumber('#,##0.0').leftPad(8),' ',\n" +
                        "    calc(qty * (unitPrice-d), d:coalesce(unitDiscount,0)).formatNumber('#,##0.0').leftPad(9),\n" +
                        "    '\n      ',itemCode,' ',\n" +
                        "    property.entries().concat(key,':',value.toString()).join(' ')\n" +
                        "  ).join('\n')\n" +
                        "}}\n" +
                        "----- ----------------------------------- -------- ---------- -------- --------\n" +
                        "{{order->totalAmount.formatNumber('US$#,##0.0').leftPad(12).concat('Subtotal:',?,'\n').leftPad(80)}}" +
                        "{{order->discountPct > 0 ? order->discountPct.formatNumber('0.0').leftPad(11).concat('Discount:',?,'%\n').leftPad(80)}}" +
                        "{{order->delivery.handlingFee!=null ? order->delivery.handlingFee.formatNumber('US$#,##0.0').leftPad(12).concat('Shipping and handling:',?,'\n').leftPad(80)}}" +
                        "{{order->calc(netAmount+fee, fee:coalesce(delivery.handlingFee,0)).formatNumber('US$#,##0.0').leftPad(12).concat('Total:',?,'\n').leftPad(80)}}" +
                        ""));

        // Test join datasets
        Map<String, String> dictionaryFinder = new LinkedHashMap<>();
        dictionaryFinder.put("stocks", "[]?{ignoredQuery}");
        dictionaryFinder.put("withStock",
                "order->items.map(itemCode,qty){itemCode} <=< stocks{itemCode}");
        BiFunction<String, String, Josson> dataFinder = (collectionName, ignoredQuery) -> {
            try {
                if (collectionName.equals("stocks[]")) {
                    return Josson.fromJsonString("[" +
                            "{\"itemCode\":\"A00201\",\"onhandQty\":18}," +
                            "{\"itemCode\":\"A00308\",\"onhandQty\":76}," +
                            "{\"itemCode\":\"A00543\",\"onhandQty\":5}," +
                            "{\"itemCode\":\"B00001\",\"onhandQty\":231}," +
                            "{\"itemCode\":\"B00002\",\"onhandQty\":0}]");
                }
            } catch (JsonProcessingException e) {
                e.printStackTrace();
            }
            return null;
        };
        ResolverProgress progress = new ResolverProgress();
        System.out.println(jossons.fillInPlaceholderWithResolver(
                "Order ID : {{order->salesOrderId}}\n" +
                        "{{withStock->concat(itemCode.rightPad(10), 'Qty: ', qty, '   Onhand: ', onhandQty).join('\n')}}",
                dictionaryFinder::get, dataFinder, progress));
        System.out.println("\n" + String.join("\n", progress.getSteps()));
    }
}
# Josson & Jossons

- _Josson_ is a complete query and transformation language for JSON.
- _Jossons_ is a template engine to generate text output.

![logo](./logo.png)

![Java](https://img.shields.io/badge/java-ED8B00?logo=java)
![License](https://badgen.net/github/license/octomix/josson)
![Maven Central](https://badgen.net/maven/v/maven-central/com.octomix.josson/josson)
[![Known Vulnerabilities](https://snyk.io/test/github/octomix/josson/badge.svg)](https://snyk.io/test/github/octomix/josson)
[![Codacy Badge](https://app.codacy.com/project/badge/Grade/b1340711f3734ef7955c2f3073fd3ee9)](https://www.codacy.com/gh/octomix/josson/dashboard?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=octomix/josson&amp;utm_campaign=Badge_Grade)

## Installation

https://mvnrepository.com/artifact/com.octomix.josson/josson

### Maven

    <dependency>
        <groupId>com.octomix.josson</groupId>
        <artifactId>josson</artifactId>
        <version>1.3.24</version>
    </dependency>

### Gradle

    implementation group: 'com.octomix.josson', name: 'josson', version: '1.3.24'

## Features and Capabilities

### Josson

- Query a JSON dataset.
- There are 215 internal functions to manipulate and format data.
- Restructure JSON data and capable of grouping and unwind data.
- Can be used as an API parameter to trim down the response JSON result.

### Jossons

- Is a template engine to fill in placeholders and generate text output.
- Support XML and HTML escaping.
- Resolve template placeholder by querying data from multiple Josson objects.
- Resolve template placeholder from external data source on demand.
- Join two JSON datasets to build a new dataset.
- Set operation on two datasets.
- I used Jossons to generate millions of SMS/Email notifications during the first year.
- I used Jossons to generate plain text and csv reports that retrieve data from MongoDB directly.
- I store the notification or report definitions in template documents. No need to write extra program coding for different template.

## Table of Contents

- [Operator Summary](#operator-summary)
  - [Relational and Logical Operators](#relational-and-logical-operators)
  - [Join and Set Operators](#join-and-set-operators)

- [Josson Basic](#josson-basic)

- [Josson Query Language](#josson-query-language)
  - [Josson Path](#josson-path)
  - [Path Chart Elements](#path-chart-elements)
  - [Tutorial](#tutorial)

- [Josson Functions](#josson-functions)
  - [Arithmetic Functions](#arithmetic-functions)
  - [String Functions](#string-functions)
  - [Date Functions](#date-functions)
  - [Format Functions](#format-functions)
  - [Logical Functions](#logical-functions)
  - [Array Functions](#array-functions)
  - [Structural Functions](#structural-functions)

- [Jossons Basic](#jossons-basic)

- [Jossons Template Language](#jossons-template-language)
  - [Placeholder](#placeholder)
  - [Nested Placeholders](#nested-placeholders)
  - [Ternary Syntax](#ternary-syntax)
  - [Join Operation](#join-operation)
  - [Set Operation](#set-operation)
  - [Join and Set Pipe Chaining](#join-and-set-pipe-chaining)
  - [Implicit Variables](#implicit-variables)
  - [Fill In](#fill-in)

- [Jossons Resolver](#jossons-resolver)
  - [Dictionary Finder](#dictionary-finder)
  - [Data Finder](#data-finder)
  - [Dictionary Function](#dictionary-function)
  - [Put Together](#put-together)

- [Appendix](#appendix)
  - [MongoDB Adapter](#mongodb-adapter)

---

## Operator Summary

### Relational and Logical Operators

| Operator | Description                             |
|:---------|:----------------------------------------|
| (        | Grouping                                |
| )        | Grouping                                |
| =        | Is equal to (support object and array)  |
| !=       | Not equal to (support object and array) |
| &gt;     | Greater than                            |
| &gt;=    | Greater than or equal to                |
| &lt;     | Less than                               |
| &lt;=    | Less than or equal to                   |
| =~       | Left matches regular expression         |
| !        | Logical NOT                             |
| &amp;    | Logical AND                             |
| &#124;   | Logical OR                              |

### Join and Set Operators

| Operator      | Description              |
|:--------------|:-------------------------|
| &gt;=&lt;     | Inner join               |
| &lt;=&lt;     | Left join one            |
| &gt;=&gt;     | Right join one           |
| &lt;=&lt;&lt; | Left join many           |
| &gt;&gt;=&gt; | Right join many          |
| &lt;!&lt;     | Left excluding join      |
| &gt;!&gt;     | Right excluding join     |
| &lt;!&gt;     | Outer excluding join     |
| &lt;+&lt;     | Left concatenate         |
| &gt;+&gt;     | Right concatenate        |
| &lt;-&lt;     | Subtract right from left |
| &gt;-&gt;     | Subtract left from right |
| &lt;-&gt;     | Symmetric difference     |
| &lt;u&gt;     | Union                    |
| &gt;n&lt;     | Intersection             |
| &#124;        | Chaining pipe            |

## Josson Basic

Initial setup for date time formatting and JSON serialization.

    Josson.setLocale(Locale.ENGLISH); // default Locale.getDefault()

    Josson.setZoneId(ZoneId.of("Asia/Hong_Kong")); // default ZoneId.systemDefault()

    Josson.setSerializationInclusion(JsonInclude.Include.NON_NULL);

To create a Josson object from a Jackson JsonNode.

    Josson josson = Josson.create(jsonNode);

To create a Josson object from a Java object.

    Josson josson = Josson.from(object);

To create a Josson object from a JSON string.

    Josson josson = Josson.fromJsonString("...");

To apply a Josson query path and get the result JsonNode.

    JsonNode node = josson.getNode(jossonPath);

## Josson Query Language

### Josson Path

A _Josson Path_ is constructed with _Path Steps_ connected by `.`.

A path step can...

- Return an element of an object node by key name.
- Filter an array node, return the first matching element or all matching elements.
- Perform a transformation operation by a [Josson Function](#josson-functions).

| Step             | Description                                                          |
|:-----------------|:---------------------------------------------------------------------|
| `key`            | A child element key name                                             |
| `[number]`       | An array element by zero-based index                                 |
| `[expression]`   | A boolean filter expression to find the first matching array element |
| `[expression]*`  | A boolean filter expression to query all matching array elements     |
| `[expression]@`  | Filter all matching elements and divert each to separate branches    |
| `[]@`            | Divert each element of the current array node to separate branches   |
| `array@`         | Divert each array element to separate branches                       |
| `function()`     | A Josson function                                                    |
| `@function()`    | Merge all branch results into a single array before manipulation     |
| `function()@`    | Divert the function output array elements to separate branches       |
| `*`              | Single wildcard symbol returns the first resolvable non-null result  |
| `**`             | Double wildcard symbol returns all object elements                   |
| `*[expression]`  | Wildcard search with filter and returns the first matching element   |
| `*[expression]*` | Wildcard search with filter and returns all matching elements        |
| `~'regex'`       | Search by regular expression and returns the first matching element  |
| `~'regex'*`      | Search by regular expression and returns all matching elements       |

To specify an array and then apply an index or a filter can be simplified by removing the `.` between them.
The following two are the same.

    array.[expression]*

    array[expression]*

Filter can also apply to an object node.
If the expression is evaluated to `true`, the object itself is returned.
Otherwise, return `null`.

For example:

    {
        "a": {
            "b": {
                "c": [1, 2, "d"]
            },
            "x": {
                "y": "z"
            }
        }
    }

    a.b.c ==> [1, 2, "d"]

    a.b.c[0] ==> 1

    a.b.c[2].upperCase() ==> "D"

    a.*.y ==> "z"

A wildcard search with filter is actually a combination of 3 steps.
The following query is the same as a wildcard search with filter that returns all matching elements:

Josson function `entries()` > Find-all array filter `[expression]*` > Select element `value`

    entries().[expression]*.value

Function `entries()` transform object `{ "name" : <JsonNode> }` into this new structure:

    {
        "key" : "name",
        "value" : <JsonNode>
    }

Therefore, use keyword `key` in a wildcard filter expression to search.
Even keyword `value` can also be used in wildcard filter expression.

For example:

    *[key.startsWith('item') & value.isText()]

    *[key.matches('^[A-Z]{10}$')]*

    *[key =~ '^[A-Z]{10}$']*

    entries().[key =~ '^[A-Z]{10}$']*.value

The last 3 examples do the same thing and can be simplified to this syntax:

    ~'^[A-Z]{10}$'*

Additional step symbols are available in filter expression and function argument.

| Step | Operation | Description                                                    |
|:-----|:----------|:---------------------------------------------------------------|
| `?`  | Scalar    | The current non-array node itself or array node's each element |
| `?`  | Aggregate | The current array node                                         |
| `@`  | Scalar    | The current array node                                         |
| `#`  | Scalar    | Zero-based index of an array element                           |
| `##` | Scalar    | One-based index of an array element                            |
| `#A` | Scalar    | Uppercase alphabetic index of an array element                 |
| `#a` | Scalar    | Lowercase alphabetic index of an array element                 |
| `#R` | Scalar    | Uppercase roman numerals index of an array element             |
| `#r` | Scalar    | Lowercase roman numerals index of an array element             |

### Path Chart Elements

Josson path chart shows data type changes and data flow along the path.
Data filtering, transformation and formatting details are not included.

| Element                       | Description                                                                              |
|-------------------------------|------------------------------------------------------------------------------------------|
| `→`                           | A progress step                                                                          |
| `⇒`                           | An end result                                                                            |
| `""`                          | A text node                                                                              |
| `$I`                          | An integer node                                                                          |
| `$D`                          | A double node                                                                            |
| `$TF`                         | A boolean node                                                                           |
| `{}`                          | An object node                                                                           |
| `obj{}`                       | A named object node                                                                      |
| `[]`                          | An array node                                                                            |
| `[]@`                         | Divert each array element to separate branches                                           |
| `array[]*`                    | A named array node                                                                       |
| `array[]@`                    | A named array node and divert each element to separate branches                          |
| `array[#]`                    | An indexed array element                                                                 |
| `[=]`                         | A find-first filter                                                                      |
| `[=]*`                        | A find-all filter                                                                        |
| `[=]@`                        | A find-all filter and divert each element to separate branches                           |
| `\ `<br>&nbsp;`@ → []`<br>`/` | Merge branches into an array                                                             |
| `func()`                      | A Josson function                                                                        |
| `(%)`                         | Function argument, the current object's child node                                       |
| `(?)`                         | Scalar function argument, the current non-array node itself or array node's each element |
| `(?[])`                       | Aggregate function argument, the current array node                                      |
| `(@)`                         | Scalar function argument, the current array node                                         |
| `!!`                          | The position where the step is unresolvable                                              |

### Tutorial

Below is the JSON for this tutorial.

    {
        "salesOrderId": "SO0001",
        "salesDate": "2022-01-01T10:01:23",
        "salesPerson": "Raymond",
        "customer": {
            "customerId": "CU0001",
            "name": "Peggy",
            "phone": "+852 62000610"
        },
        "items": [
            {
                "itemCode": "B00001",
                "name": "WinWin TShirt Series A - 2022",
                "brand": "WinWin",
                "property": {
                    "size": "M",
                    "colors": [ "WHITE", "RED" ]
                },
                "qty": 2,
                "unit": "Pcs",
                "unitPrice": 15.0,
                "tags": [ "SHIRT", "WOMEN" ]
            },
            {
                "itemCode": "A00308",
                "name": "OctoPlus Tennis Racket - Star",
                "brand": "OctoPlus",
                "property": {
                    "colors": [ "BLACK" ]
                },
                "qty": 1,
                "unit": "Pcs",
                "unitPrice": 150.0,
                "unitDiscount": 10.0,
                "tags": [ "TENNIS", "SPORT", "RACKET" ]
            },
            {
                "itemCode": "A00201",
                "name": "WinWin Sport Shoe - Super",
                "brand": "WinWin",
                "property": {
                    "size": "35",
                    "colors": [ "RED" ]
                },
                "qty": 1,
                "unit": "Pair",
                "unitPrice": 110.0,
                "unitDiscount": 10.0,
                "tags": [ "SHOE", "SPORT", "WOMEN" ]
            }
        ],
        "totalAmount": 270.0
    }

1. To query a value node.

        josson.getNode("salesPerson")
        ==>
        "Raymond"

   _Path chart_

        {} → salesPerson ⇒ ""

2. Node name is case-sensitive. Josson returns null value if the path is unresolvable.

        josson.getNode("salesperson")
        ==>
        !unresolvable!

   _Path chart_

        {} → salesperson!! ⇒ !unresolvable!

3. To query an object node.

        josson.getNode("customer")
        ==>
        {
            "customerId" : "CU0001",
            "name" : "Peggy",
            "phone" : "+852 62000610"
        }

   _Path chart_

        {} → customer{} ⇒ {}

4. Parent node and child node are connected by a `.`.

        josson.getNode("customer.name")
        ==>
        "Peggy"

   _Path chart_

        {} → customer{} → name ⇒ ""

5. Function is constructed by a function name followed by parentheses with optional comma-separated arguments.  
   A function manipulate the current node and produce an output along the path.

        josson.getNode("customer.name.upperCase()")
        ==>
        "PEGGY"

   _Path chart_

        {} → customer{} → name → upperCase(?) ⇒ ""

6. Function name is case-insensitive.  
   If one more parameter is given in addition to a function's maximum number of argument,
   the function will manipulate the data that evaluated from the 1st parameter.
   This mechanism does not apply to function that accept unlimited number of arguments.  
   e.g. upperCase() needs 0 argument. If 1 parameter is given, upperCase() will manipulate that data.

        josson.getNode("customer.UPPERCase(name)")
        ==>
        "PEGGY"

   _Path chart_

        {} → customer{} → upperCase(%) ⇒ ""

7. If the function is the first path step, it works on the root node.

        josson.getNode("upperCase(customer.name)")
        ==>
        "PEGGY"

   _Path chart_

        {} → upperCase(%) ⇒ ""

8. Functions can be nested and the parameters can refer to those child nodes of the same step.

        josson.getNode("customer.concat(upperCase(name), ' / ', phone)")
        ==>
        "PEGGY / +852 62000610"

   _Path chart_

        {} → customer{} → concat(%) ⇒ ""

9. A path start with numbers override the data and produces an integer node.

        josson.getNode("123")
        ==>
        123

   _Path chart_

        $I ⇒ $I

10. A path start with numbers and has `.` produces a double node.

        josson.getNode("123.40")
        ==>
        123.4

    _Path chart_

        $D ⇒ $D

11. A path start and end with single quote `'`override the data and produces a text string node.  
    If the string literal contains a single quote, it is replaced by two single quotes.

        josson.getNode("'She said, ''Go ahead''.'")
        ==>
        "She said, 'Go ahead'."

    _Path chart_

        "" ⇒ ""

12. A path start with `true` or `false` override the data and produces a boolean node.

        josson.getNode("true.not()")
        ==>
        false

    _Path chart_

        $TF → not(?) ⇒ $TF

13. To query an array node.

        josson.getNode("items")
        ==>
        [ {
          "itemCode" : "B00001",
          "name" : "WinWin TShirt Series A - 2022",
          "brand" : "WinWin",
          "property" : {
            "size" : "M",
            "colors" : [ "WHITE", "RED" ]
          },
          "qty" : 2,
          "unit" : "Pcs",
          "unitPrice" : 15.0,
          "tags" : [ "SHIRT", "WOMEN" ]
        }, {
          "itemCode" : "A00308",
          "name" : "OctoPlus Tennis Racket - Star",
          "brand" : "OctoPlus",
          "property" : {
            "colors" : [ "BLACK" ]
          },
          "qty" : 1,
          "unit" : "Pcs",
          "unitPrice" : 150.0,
          "unitDiscount" : 10.0,
          "tags" : [ "TENNIS", "SPORT", "RACKET" ]
        }, {
          "itemCode" : "A00201",
          "name" : "WinWin Sport Shoe - Super",
          "brand" : "WinWin",
          "property" : {
            "size" : "35",
            "colors" : [ "RED" ]
          },
          "qty" : 1,
          "unit" : "Pair",
          "unitPrice" : 110.0,
          "unitDiscount" : 10.0,
          "tags" : [ "SHOE", "SPORT", "WOMEN" ]
        } ]

    _Path chart_

        {} → items[]* ⇒ [{}]

14. An array filter is enclosed by square brackets.  
    Directly query an array element by zero-based index value.

        josson.getNode("items[0]")
        ==>
        {
          "itemCode" : "B00001",
          "name" : "WinWin TShirt Series A - 2022",
          "brand" : "WinWin",
          "property" : {
            "size" : "M",
            "colors" : [ "WHITE", "RED" ]
          },
          "qty" : 2,
          "unit" : "Pcs",
          "unitPrice" : 15.0,
          "tags" : [ "SHIRT", "WOMEN" ]
        }

    _Path chart_

        {} → items[#] ⇒ {}

15. To query a child value node of an array element.

        josson.getNode("items[1].name")
        ==>
        "OctoPlus Tennis Racket - Star"

    _Path chart_

        {} → items[#] → {} → name ⇒ ""

16. To query a child object node of an array element.

        josson.getNode("items[2].property")
        ==>
        {
          "size" : "35",
          "colors" : [ "RED" ]
        }

    _Path chart_

        {} → items[#] → {} → property{} ⇒ {}

17. To query all the elements of an array node and output them inside an array node.

        josson.getNode("items.qty")
        ==>
        [ 2, 1, 1 ]

    _Path chart_

        {} → items[]* → [{}] → [qty] ⇒ [$I]

18. A function that manipulates each array element and output all results inside an array node.

        josson.getNode("items.concat('Qty=',qty)")
        ==>
        [ "Qty=2", "Qty=1", "Qty=1" ]

    _Path chart_

        {} → items[]* → [{}] → [concat(%) ⇒ ""] ⇒ [""]

19. If a step is working on an object or value node, `?` represents that node.

        josson.getNode("items.qty.concat('Qty=',?)")
        ==>
        [ "Qty=2", "Qty=1", "Qty=1" ]

    _Path chart_

        {} → items[]* → [{}] → [qty] → [concat(?) ⇒ ""] ⇒ [""]

20. An aggregate function manipulates an array node and produce a value node.

        josson.getNode("items.qty.sum()")
        ==>
        4.0

    _Path chart_

        {} → items[]* → [{}] → [qty] → sum(?[]) ⇒ $D

21. Uses Java standard formatting pattern.

        josson.getNode("items.sum(qty).formatNumber('#,##0')")
        ==>
        "4"

    _Path chart_

        {} → items[]* → [{}] → sum(?[%]) → $D → formatNumber(?) ⇒ ""

22. Find the first matching element by array filter.

        josson.getNode("items.itemCode[!startsWith('A')]")
        ==>
        "B00001"

    _Path chart_

        {} → items[]* → [{}] → [itemCode][=] ⇒ ""

23. Filter using relational operators `=`, `!=`, `>`, `>=`, `<` and `<=`.

        josson.getNode("items[unitDiscount > 0].name")
        ==>
        "OctoPlus Tennis Racket - Star"

    _Path chart_

        {} → items[][=] → {} → name ⇒ ""

24. Returns null value if nothing matches the array filter.

        josson.getNode("items[unitDiscount > 100].name")
        ==>
        !unresolvable!

    _Path chart_

        {} → items[][=]!! → {} → name ⇒ !unresolvable!

25. To query all matching elements, add a modifier `*` after the array filter.

        josson.getNode("items[unitDiscount > 0]*.name")
        ==>
        [ "OctoPlus Tennis Racket - Star", "WinWin Sport Shoe - Super" ]

    _Path chart_

        {} → items[][=]* → [{}] → [name] ⇒ [""]

26. If a step is working on an array node, `#` denotes the zero-based index of an array element.

        josson.getNode("items[#.isEven()]*.itemCode")
        ==>
        [ "B00001", "A00201" ]

    _Path chart_

        {} → items[][=]* → [{}] → [itemCode] ⇒ [""]

27. For each path step, a nested array is flattened once.

        josson.getNode("items[true]*.tags[true]*")
        ==>
        [ "SHIRT", "WOMEN", "TENNIS", "SPORT", "RACKET", "SHOE", "SPORT", "WOMEN" ]

    _Path chart_

        {} → items[][=]* → [{}] → [tags[][=]* ⇒ [""]] ⇒ [""]

28. Path step `array.` is the same as `array[true]*.`.

        josson.getNode("items.tags")
        ==>
        [ "SHIRT", "WOMEN", "TENNIS", "SPORT", "RACKET", "SHOE", "SPORT", "WOMEN" ]

    _Path chart_

        {} → items[]* → [{}] → [tags[]* ⇒ [""]] ⇒ [""]

29. If a step is working on an array node, `?` represents an array element.  
    `=~` matches a regular expression.

        josson.getNode("items.tags[? =~ '^S.*O.+']*")
        ==>
        [ "SPORT", "SHOE", "SPORT" ]

    _Path chart_

        {} → items[]* → [{}] → [tags[][=]* ⇒ [""]] ⇒ [""]

30. The matching criteria supports logical operators and parentheses.

    >not `!`  
     and `&`  
     or `|`

        josson.getNode("items[(unitDiscount=null | unitDiscount=0) & !(qty<=1)]*.name")
        ==>
        [ "WinWin TShirt Series A - 2022" ]

    _Path chart_

        {} → items[][=]* → [{}] → [name] ⇒ [""]

31. Example of a find-all filter operation with flattened array result.

        josson.getNode("items[tags.contains('SPORT')]*.tags")
        ==>
        [ "TENNIS", "SPORT", "RACKET", "SHOE", "SPORT", "WOMEN" ]

    _Path chart_

        {} → items[][=]* → [{}] → [tags[]* ⇒ [""]] ⇒ [""]

32. An array filter modifier `@` divert each element to separate branch for upcoming manipulation.  
    The final output merges branches into an array.

        josson.getNode("items[tags.containsIgnoreCase('Women')]@.tags")
        ==>
        [ [ "SHIRT", "WOMEN" ], [ "SHOE", "SPORT", "WOMEN" ] ]

    _Path chart_

                         {} → tags[]* → [""]
                        /                   \
        {} → items[][=]@                     @ ⇒ [[""]]
                        \                   /
                         {} → tags[]* → [""]

33. Some functions work on an array node and produce a value node.

        josson.getNode("items.tags.join('+')")
        ==>
        SHIRT+WOMEN+TENNIS+SPORT+RACKET+SHOE+SPORT+WOMEN

    _Path chart_

        {} → items[]* → [{}] → [tags[]* ⇒ [""]] → [""] → join(?[]) ⇒ ""

34. An array node can apply the modifier `@` that divert each element to separate branch.

        josson.getNode("items@.tags.join('+')")
        ==>
        [ "SHIRT+WOMEN", "TENNIS+SPORT+RACKET", "SHOE+SPORT+WOMEN" ]

    _Path chart_

                      {} → tags[]* → [""] → join(?[]) → ""
                     /                                    \
        {} → items[]@                                      @ ⇒ [""]
                     \                                    /
                      {} → tags[]* → [""] → join(?[]) → ""

35. Syntax `[]@` diverts each element of the current array node.

        josson.getNode("items.join([]@.tags.join('+'),' / ')")
        ==>
        "SHIRT+WOMEN / TENNIS+SPORT+RACKET / SHOE+SPORT+WOMEN"

    _Path chart_

                                         {} → tags[]* → [""] → join(?[]) → ""
                                        /                                    \
        {} → items[]* → [{}] → join(?[]@                                      @ ⇒ [""]) ⇒ ""
                                        \                                    /
                                         {} → tags[]* → [""] → join(?[]) → ""

36. Modifier `@` before a function name merges all branch results into a single array before manipulation.

        josson.getNode("items@.tags.join('+').@join(' / ')")
        ==>
        "SHIRT+WOMEN / TENNIS+SPORT+RACKET / SHOE+SPORT+WOMEN"

    _Path chart_

                      {} → tags[]* → [""] → join(?[]) → ""
                     /                                    \
        {} → items[]@                                      @ → [""] → join(?[]) ⇒ ""
                     \                                    /
                      {} → tags[]* → [""] → join(?[]) → ""

37. Modifier `@` after a function diverts the function output array elements to separate branches.  
    It has the same effect of a path step `.[]@` after a function.

        josson.getNode("'1+2 | 3+4 | 5+6'.split('|')@.split('+').calc(?*2).round(0).join('+').concat('(',?,')/2').@join(' | ')")
        ==>
        "(2+4)/2 | (6+8)/2 | (10+12)/2"

    _Path chart_

                              "" → split(?) → [""] → [calc(?) ⇒ $D] → [round(?) ⇒ $I] → join(?[]) → "" → concat(?) → ""
                             /                                                                                          \
        "" → split(?) → [""]@                                                                                            @ → [""] → join(?[]) ⇒ ""
                             \                                                                                          /
                              "" → split(?) → [""] → [calc(?) ⇒ $D] → [round(?) ⇒ $I] → join(?[]) → "" → concat(?) → ""

38. All function parameters can refer to a child node of the step.

        josson.getNode("items@.repeat(concat('[',brand,'] ',name,'\n'), qty).@join()")
        ==>
        "[WinWin] WinWin TShirt Series A - 2022\n" +
        "[WinWin] WinWin TShirt Series A - 2022\n" +
        "[OctoPlus] OctoPlus Tennis Racket - Star\n" +
        "[WinWin] WinWin Sport Shoe - Super\n"

    _Path chart_

                      {} → repeat(%) → ""
                     /                   \
        {} → items[]@                     @ → [""] → join(?[]) ⇒ ""
                     \                   /
                      {} → repeat(%) → ""

39. Some functions work on array and produce an array, such as `concat()`, manipulate on each element.

        josson.getNode("items.concat('Item ',#,': [',itemCode,'] ',qty,unit,' x ',name,' <',property.colors.join(','),'>').join('\n')")
        ==>
        "Item 0: [B00001] 2Pcs x WinWin TShirt Series A - 2022 <WHITE,RED>\n" +
        "Item 1: [A00308] 1Pcs x OctoPlus Tennis Racket - Star <BLACK>\n" +
        "Item 2: [A00201] 1Pair x WinWin Sport Shoe - Super <RED>"

    _Path chart_

        {} → items[]* → [{}] → [concat(%) ⇒ ""] → join(?[]) ⇒ ""

40. If a step is working on an array node, `@` represents that array node.  
    `##` denotes the one-based index of an array element.

        josson.getNode("items.sort(itemCode).concat('Item ',##,'/',@.size(),': [',itemCode,'] ',qty,unit,' x ',name,' <',property.colors.join(','),'>').join('\n')")
        ==>
        "Item 1/3: [A00201] 1Pair x WinWin Sport Shoe - Super <RED>\n" +
        "Item 2/3: [A00308] 1Pcs x OctoPlus Tennis Racket - Star <BLACK>\n" +
        "Item 3/3: [B00001] 2Pcs x WinWin TShirt Series A - 2022 <WHITE,RED>"

    _Path chart_

        {} → items[]* → [{}] -> sort(%) → [{}] → [concat(@, %) ⇒ ""] → join(?[]) ⇒ ""

41. An object node with a validation filter.

        josson.getNode("customer[name='Peggy']")
        ==>
        {
          "customerId" : "CU0001",
          "name" : "Peggy",
          "phone" : "+852 62000610"
        }

    _Path chart_

        {} → customer{}[=] ⇒ {}

42. An object node with a validation filter.

        josson.getNode("customer[name='Raymond']")
        ==>
        !unresolvable!

    _Path chart_

        {} → customer{}[=]!! ⇒ !unresolvable!

43. Function `json()` parse a JSON string.

        josson.getNode("json('[1,2,"3"]')")
        ==>
        [ 1, 2, "3" ]

    _Path chart_

        json("") ⇒ []

44. Relational operator `=` and `!=` support object comparison.

        josson.getNode("[customer = json('{"name":"Peggy","phone":"+852 62000610","customerId":"CU0001"}')].isNotNull()")
        ==>
        true

    _Path chart_

        {}[=] → {} → isNotNull(?) ⇒ $TF

45. Relational operator `=` and `!=` support root level array values comparison where the position ordering is allowed to be different.

        josson.getNode("[items[0].property.colors = json('["RED","WHITE"]')].isNotNull()")
        ==>
        true

    _Path chart_

        {}[=] → {} → isNotNull(?) ⇒ $TF

46. Function `calc()` uses MathParser.org-mXparser library <http://mathparser.org/> to perform calculation.

        josson.getNode("items.calc(qty * (unitPrice-unitDiscount)).concat(##,'=',?)")
        ==>
        [ null, "2=140.0", "3=100.0" ]

    _Path chart_

        {} → items[]* → [{}] → [calc(%) ⇒ $D] → [concat(?) ⇒ ""] ⇒ [""]

47. Scalar functions preserve null element.

        josson.getNode("items.calc(qty * (unitPrice-unitDiscount)).[##<=2]*.concat(##,'=',?)")
        ==>
        [ null, "2=140.0" ]

    _Path chart_

        {} → items[]* → [{}] → [calc(%) ⇒ $D] → [=]* → [concat(?) ⇒ ""] ⇒ [""]

48. An array-to-value transformation function throws away null nodes automatically.

        josson.getNode("items.calc(qty * (unitPrice-unitDiscount)).concat(##,'=',?).join(' / ')")
        ==>
        "2=140.0 / 3=100.0"

    _Path chart_

        {} → items[]* → [{}] → [calc(%) ⇒ $D] → [concat(?) ⇒ ""] → join(?[]) ⇒ ""

49. Array filter can filter out null nodes.

        josson.getNode("items.calc(qty * (unitPrice-unitDiscount)).[isNotNull()]*.concat(##,'=',?)")
        ==>
        [ "1=140.0", "2=100.0" ]

    _Path chart_

        {} → items[]* → [{}] → [calc(%) ⇒ $D] → [=]* → [concat(?) ⇒ ""] ⇒ [""]

50. An argument `#A` denotes the uppercase alphabetic array index.

        josson.getNode("items.calc(qty * (unitPrice-unitDiscount)).[?!=null]*.concat(#A,'=',?).join(' / ')")
        ==>
        "A=140.0 / B=100.0"

    _Path chart_

        {} → items[]* → [{}] → [calc(%) ⇒ $D] → [=]* → [concat(?) ⇒ ""] → join(?[]) ⇒ ""

51. Merge Diverted branches throws away null nodes automatically.
    An argument `#a` denotes the lowercase alphabetic array index.

        josson.getNode("items@.calc(qty * (unitPrice-unitDiscount)).@concat(#a,'=',?)")
        ==>
        [ "a=140.0", "b=100.0" ]

    _Path chart_

                      {} → calc(%) → $D
                     /                 \
        {} → items[]@                   @ → [$D] → [concat(?) ⇒ ""] ⇒ [""]
                     \                 /
                      {} → calc(%) → $D

52. mXparser expression accepts single-step path only.
    To apply multi-steps path, function or filter, append arguments with syntax `newVariable:path`.

        josson.getNode("items.calc(qty * (unitPrice-x), x:coalesce(unitDiscount,0)).formatNumber('US$#,##0.00')")
        ==>
        [ "US$30.00", "US$140.00", "US$100.00" ]

    _Path chart_

        {} → items[]* → [{}] → [calc(%) ⇒ $D] → [formatNumber(?) ⇒ ""] ⇒ [""]

53. An argument `#r` and `#R` denotes the lowercase and uppercase roman numerals array index.

        josson.getNode("items.unitPrice.calc(? * 2).concat(#r,'=',?)")
        ==>
        [ "i=30.0", "ii=300.0", "iii=220.0" ]

    _Path chart_

        {} → items[]* → [{}] → [unitPrice ⇒ $D] → [calc(?) ⇒ $D] → [concat(?) ⇒ ""] ⇒ [""]

54. Function `entries()` returns an array of an object's string-keyed property `[{key, value}]` pairs.

        josson.getNode("items[0].entries()")
        ==>
        [ {
          "key" : "itemCode",
          "value" : "B00001"
        }, {
          "key" : "name",
          "value" : "WinWin TShirt Series A - 2022"
        }, {
          "key" : "brand",
          "value" : "WinWin"
        }, {
          "key" : "property",
          "value" : {
            "size" : "M",
            "colors" : [ "WHITE", "RED" ]
          }
        }, {
          "key" : "qty",
          "value" : 2
        }, {
          "key" : "unit",
          "value" : "Pcs"
        }, {
          "key" : "unitPrice",
          "value" : 15.0
        }, {
          "key" : "tags",
          "value" : [ "SHIRT", "WOMEN" ]
        } ]

    _Path chart_

        {} → items[#] → {} → entries(?) ⇒ [{}]

55. Function `keys()` lists an object's key names.

        josson.getNode("keys()")
        ==>
        [ "salesOrderId", "salesDate", "salesPerson", "customer", "items", "totalAmount" ]

    _Path chart_

        {} → keys(?) ⇒ [""]

56. `keys()` can retrieve nested child object keys for a given levels.

        josson.getNode("keys(?, 2)")
        ==>
        [ "salesOrderId", "salesDate", "salesPerson", "customer", "customerId", "name", "phone", "items", "totalAmount" ]

    _Path chart_

        {} → keys(?) ⇒ [""]

57. Function `toArray()` puts an object's values into an array.

        josson.getNode("customer.toArray()")
        ==>
        [ "CU0001", "Peggy", "+852 62000610" ]

    _Path chart_

        {} → customer{} → toArray(?) ⇒ [""]

58. Furthermore, function `toArray()` puts all arguments (values, object's values, array elements) into a single array.

        josson.getNode("toArray('Hello',customer,items.itemCode.sort())")
        ==>
        [ "Hello", "CU0001", "Peggy", "+852 62000610", "A00201", "A00308", "B00001" ]

    _Path chart_

        {} → toArray(%) ⇒ [""]

59. Function `map()` constructs a new object node.
    For multi-steps path, the last element name will become the new element name.
    To rename an element, use syntax `newFieldName:path` or `queryThatResolveToName::path`.

        josson.getNode("map(customer.name,date:salesDate,sales:map(items.concat(name,' x ',qty,unit), totalQty:items.sum(qty), totalAmount))")
        ==>
        {
          "name" : "Peggy",
          "date" : "2022-01-01T10:01:23",
          "sales" : {
            "items" : [ "WinWin TShirt Series A - 2022 x 2Pcs", "OctoPlus Tennis Racket - Star x 1Pcs", "WinWin Sport Shoe - Super x 1Pair" ],
            "totalQty" : 4.0,
            "totalAmount" : 270.0
          }
        }

    _Path chart_

        {} → map(%) ⇒ {}

60. Function `field()` adds, removes and renames field on the current object node.
    To remove an element, use syntax `fieldName:` or `queryThatResolveToName::`.

        josson.getNode("items[0].field(subtotal:calc(qty * (unitPrice-x), x:coalesce(unitDiscount,0)),brand:,property:,tags:)")
        ==>
        {
          "itemCode" : "B00001",
          "name" : "WinWin TShirt Series A - 2022",
          "qty" : 2,
          "unit" : "Pcs",
          "unitPrice" : 15.0,
          "subtotal" : 30.0
        }

    _Path chart_

        {} → items[#] → {} → field(%) ⇒ {}

61. Functions `map()` and `field()` works on array.

        josson.getNode("items.field(subtotal:calc(qty * (unitPrice-x), x:coalesce(unitDiscount,0)),brand:,property:,tags:)")
        ==>
        [ {
          "itemCode" : "B00001",
          "name" : "WinWin TShirt Series A - 2022",
          "qty" : 2,
          "unit" : "Pcs",
          "unitPrice" : 15.0,
          "subtotal" : 30.0
        }, {
          "itemCode" : "A00308",
          "name" : "OctoPlus Tennis Racket - Star",
          "qty" : 1,
          "unit" : "Pcs",
          "unitPrice" : 150.0,
          "unitDiscount" : 10.0,
          "subtotal" : 140.0
        }, {
          "itemCode" : "A00201",
          "name" : "WinWin Sport Shoe - Super",
          "qty" : 1,
          "unit" : "Pair",
          "unitPrice" : 110.0,
          "unitDiscount" : 10.0,
          "subtotal" : 100.0
        } ]

    _Path chart_

        {} → items[]* → [{}] → [field(%) ⇒ {}] ⇒ [{}]

62. Function `group()` works like SQL `group by`. It will build a structure of `[{key, [elements]}]`.
    The first parameter is the grouping key. If it is a function, it will be given a name `key` in the output.
    The optional second parameter is to evaluate the grouped element. The default is the whole array element.
    And the default output array name is `elements`.
    The names can be renamed by preceding with `newName:` or `queryThatResovleToName::`.
    
        josson.getNode("items.group(brand,map(name,qty,netPrice:calc(unitPrice-x,x:coalesce(unitDiscount,0))))")
        ==>
        [ {
          "brand" : "WinWin",
          "elements" : [ {
            "name" : "WinWin TShirt Series A - 2022",
            "qty" : 2,
            "netPrice" : 15.0
          }, {
            "name" : "WinWin Sport Shoe - Super",
            "qty" : 1,
            "netPrice" : 100.0
          } ]
        }, {
          "brand" : "OctoPlus",
          "elements" : [ {
            "name" : "OctoPlus Tennis Racket - Star",
            "qty" : 1,
            "netPrice" : 140.0
          } ]
        } ]

        josson.getNode(
            "items.group(brand,map(name,qty,netPrice:calc(unitPrice-x,x:coalesce(unitDiscount,0))))@" +
            ".concat('Brand : ',brand,'\n',elements.concat('- ',name,' : Qty=',qty,' Amt=',calc(qty*netPrice),'\n').join()," +
            "'> Sub-total : Qty=',elements.sum(qty),' Amt=',elements.sum(calc(qty*netPrice))).@join('\n\n')")
        ==>
        Brand : WinWin
        - WinWin TShirt Series A - 2022 : Qty=2 Amt=30.0
        - WinWin Sport Shoe - Super : Qty=1 Amt=100.0
        > Sub-total : Qty=3.0 Amt=130.0

        Brand : OctoPlus
        - OctoPlus Tennis Racket - Star : Qty=1 Amt=140.0
        > Sub-total : Qty=1.0 Amt=140.0

    _Path chart_

                                                {} → concat(%) → ""
                                               /                   \
        {} → items[]* → [{}] → group(%) → [{}]@                     @ → [""] → join(?[]) ⇒ ""
                                               \                   /
                                                {} → concat(%) → ""

63. Function `unwind()` works like MongoDB `$unwind` operation. The operation is the reverse of `group()`.

        josson.getNode("items.group(brand,map(name,qty,netPrice:calc(unitPrice-x,x:coalesce(unitDiscount,0)))).unwind(elements)")
        ==>
        [ {
          "brand" : "WinWin",
          "name" : "WinWin TShirt Series A - 2022",
          "qty" : 2,
          "netPrice" : 15.0
        }, {
          "brand" : "WinWin",
          "name" : "WinWin Sport Shoe - Super",
          "qty" : 1,
          "netPrice" : 100.0
        }, {
          "brand" : "OctoPlus",
          "name" : "OctoPlus Tennis Racket - Star",
          "qty" : 1,
          "netPrice" : 140.0
        } ]

    _Path chart_

        {} → items[]* → [{}] → group(%) → [{}] → unwind(%) ⇒ [{}]

64. Function `flatten()` flatten an array same as the default path step behavior. But more readable.

        josson.getNode("items@.tags")
        ==>
        [ [ "SHIRT", "WOMEN" ], [ "TENNIS", "SPORT", "RACKET" ], [ "SHOE", "SPORT", "WOMEN" ] ]


        josson.getNode("items@.tags.@flatten()")
        ==>
        [ "SHIRT", "WOMEN", "TENNIS", "SPORT", "RACKET", "SHOE", "SPORT", "WOMEN" ]


        josson.getNode("items@.tags.@[true]*")
        ==>
        [ "SHIRT", "WOMEN", "TENNIS", "SPORT", "RACKET", "SHOE", "SPORT", "WOMEN" ]

    _Path chart_

                      {} → tags[]* → [""]
                     /                   \
        {} → items[]@                     @ → [[""]] → flatten() ⇒ [""]
                     \                   /
                      {} → tags[]* → [""]

65. Functions `map()`,`field()`,`group()`,`unwind()` - key name support evaluation using syntax `keyQuery::valueQuery`.

        josson.getNode("items.map(itemCode::qty)")
        ==>
        [ {
          "B00001" : 2
        }, {
          "A00308" : 1
        }, {
          "A00201" : 1
        } ]

    _Path chart_

        {} → items[]* → [{}] → [map(%) ⇒ {}] ⇒ [{}]

66. Function `mergeObjects()` merge all objects in an array into one object.

        josson.getNode("mergeObjects(customer, items.map(itemCode::qty))")
        ==>
        {
          "customerId" : "CU0001",
          "name" : "Peggy",
          "phone" : "+852 62000610",
          "B00001" : 2,
          "A00308" : 1,
          "A00201" : 1
        }

    _Path chart_

        {} → mergeObjects(%) ⇒ {}

## Josson Functions

There are 215 functions. They are classified into 7 categories:

Arithmetic Functions

1. [abs()](#1-abs)
2. [calc()](#2-calc)
3. [ceil()](#3-ceil)
4. [floor()](#4-floor)
5. [mod()](#5-mod)
6. [round()](#6-round)

String Functions

7. [abbreviate()](#7-abbreviate)
8. [append()](#8-append)
9. [appendIfMissing()](#9-appendifmissing)
10. [appendIfMissingIgnoreCase()](#10-appendifmissingignorecase)
11. [capitalize()](#11-capitalize)
12. [center()](#12-center)
13. [concat()](#13-concat)
14. [concatFree()](#14-concatfree)
15. [keepAfter()](#15-keepafter)
16. [keepAfterIgnoreCase()](#16-keepafterignorecase)
17. [keepAfterLast()](#17-keepafterlast)
18. [keepAfterLastIgnoreCase()](#18-keepafterlastignorecase)
19. [keepBefore()](#19-keepbefore)
20. [keepBeforeIgnoreCase()](#20-keepbeforeignorecase)
21. [keepBeforeLast()](#21-keepbeforelast)
22. [keepBeforeLastIgnoreCase()](#22-keepbeforelastignorecase)
23. [leftPad()](#23-leftpad)
24. [length()](#24-length)
25. [lowerCase()](#25-lowercase)
26. [notEmpty()](#26-notempty)
27. [notBlank()](#27-notblank)
28. [prepend()](#28-prepend)
29. [prependIfMissing()](#29-prependifmissing)
30. [prependIfMissingIgnoreCase()](#30-prependifmissingignorecase)
31. [removeEnd()](#31-removeend)
32. [removeEndIgnoreCase()](#32-removeendignorecase)
33. [removeStart()](#33-removestart)
34. [removeStartIgnoreCase()](#34-removestartignorecase)
35. [repeat()](#35-repeat)
36. [replace()](#36-replace)
37. [replaceIgnoreCase()](#37-replaceignorecase)
38. [rightPad()](#38-rightpad)
39. [split()](#39-split)
40. [strip()](#40-strip)
41. [stripEnd()](#41-stripend)
42. [stripStart()](#42-stripstart)
43. [substr()](#43-substr)
44. [trim()](#44-trim)
45. [uncapitalize()](#45-uncapitalize)
46. [upperCase()](#46-uppercase)
47. [camelCase()](#47-camelcase)
48. [upperCamelCase()](#48-uppercamelcase)
49. [snakeCase()](#49-snakecase)
50. [lowerSnakeCase()](#50-lowersnakecase)
51. [upperSnakeCase()](#51-uppersnakecase)
52. [camelSnakeCase()](#52-camelsnakecase)
53. [singleQuote() / quote() / q()](#53-singlequote--quote--q)
54. [doubleQuote() / qq()](#54-doublequote--qq)

Date Functions

55. [amPmOfDay()](#55-ampmofday)
56. [second()](#56-second)
57. [secondOfDay()](#57-secondofday)
58. [minute()](#58-minute)
59. [minuteOfDay()](#59-minuteofday)
60. [hourOfAmPm()](#60-hourofampm)
61. [hour()](#61-hour)
62. [dayOfWeek()](#62-dayofweek)
63. [day()](#63-day)
64. [dayOfYear()](#64-dayofyear)
65. [month()](#65-month)
66. [year()](#66-year)
67. [plusSeconds()](#67-plusseconds)
68. [plusMinutes()](#68-plusminutes)
69. [plusHours()](#69-plushours)
70. [plusDays()](#70-plusdays)
71. [plusWeeks()](#71-plusweeks)
72. [plusMonths()](#72-plusmonths)
73. [plusYears()](#73-plusyears)
74. [minusSeconds()](#74-minusseconds)
75. [minusMinutes()](#75-minusminutes)
76. [minusHours()](#76-minushours)
77. [minusDays()](#77-minusdays)
78. [minusWeeks()](#78-minusweeks)
79. [minusMonths()](#79-minusmonths)
80. [minusYears()](#80-minusyears)
81. [truncateToMicro()](#81-truncatetomicro)
82. [truncateToMilli()](#82-truncatetomilli)
83. [truncateToSecond()](#83-truncatetosecond)
84. [truncateToMinute()](#84-truncatetominute)
85. [truncateToHour()](#85-truncatetohour)
86. [truncateToDay()](#86-truncatetoday)
87. [truncateToMonth()](#87-truncatetomonth)
88. [truncateToYear()](#88-truncatetoyear)
89. [withNano()](#89-withnano)
90. [withMicro()](#90-withmicro)
91. [withMilli()](#91-withmilli)
92. [withSecond()](#92-withsecond)
93. [withMinute()](#93-withminute)
94. [withHour()](#94-withhour)
95. [withDay()](#95-withday)
96. [withDayOfYear()](#96-withdayofyear)
97. [withMonth()](#97-withmonth)
98. [withYear()](#98-withyear)
99. [dayEnd()](#99-dayend)
100. [monthEnd()](#100-monthend)
101. [yearEnd()](#101-yearend)
102. [lengthOfMonth()](#102-lengthofmonth)
103. [lengthOfYear()](#103-lengthofyear)
104. [untilInSecond()](#104-untilinsecond)
105. [untilInMinute()](#105-untilinminute)
106. [untilInHour()](#106-untilinhour)
107. [untilInDay()](#107-untilinday)
108. [untilInMonth()](#108-untilinmonth)
109. [untilInYear()](#109-untilinyear)
110. [localToOffsetDate()](#110-localtooffsetdate)
111. [offsetToLocalDate()](#111-offsettolocaldate)

Format Functions

112. [b64Encode()](#112-b64encode)
113. [b64EncodeNoPadding()](#113-b64encodenopadding)
114. [b64MimeEncode()](#114-b64mimeencode)
115. [b64MimeEncodeNoPadding()](#115-b64mimeencodenopadding)
116. [b64UrlEncode()](#116-b64urlencode)
117. [b64UrlEncodeNoPadding()](#117-b64urlencodenopadding)
118. [b64Decode()](#118-b64decode)
119. [b64MimeDecode()](#119-b64mimedecode)
120. [b64UrlDecode()](#120-b64urldecode)
121. [urlEncode()](#121-urlencode)
122. [urlDecode()](#122-urldecode)
123. [if()](#123-if)
124. [coalesce()](#124-coalesce)
125. [caseValue()](#125-casevalue)
126. [caseValueIgnoreCase()](#126-casevalueignorecase)
127. [cycleValue()](#127-cyclevalue)
128. [default()](#128-default)
129. [indexedValue()](#129-indexedvalue)
130. [formatDate()](#130-formatdate)
131. [formatNumber()](#131-formatnumber)
132. [formatText()](#132-formattext)
133. [formatTexts()](#133-formattexts)
134. [toNumber()](#134-tonumber)
135. [toString()](#135-tostring)
136. [toText()](#136-totext)
137. [csv()](#137-csv)
138. [csvShowNull()](#138-csvshownull)
139. [csvParams()](#139-csvparams)

Logical Functions

140. [contains()](#140-contains)
141. [containsIgnoreCase()](#141-containsignorecase)
142. [notContains()](#142-notcontains)
143. [notContainsIgnoreCase()](#143-notcontainsignorecase)
144. [startsWith()](#144-startswith)
145. [startsWithIgnoreCase()](#145-startswithignorecase)
146. [notStartsWith()](#146-notstartswith)
147. [notStartsWithIgnoreCase()](#147-notstartswithignorecase)
148. [endsWith()](#148-endswith)
149. [endsWithIgnoreCase()](#149-endswithignorecase)
150. [notEndsWith()](#150-notendswith)
151. [notEndsWithIgnoreCase()](#151-notendswithignorecase)
152. [equals()](#152-equals)
153. [equalsIgnoreCase()](#153-equalsignorecase)
154. [notEquals()](#154-notequals)
155. [notEqualsIgnoreCase()](#155-notequalsignorecase)
156. [matches()](#156-matches)
157. [notMatches()](#157-notmatches)
158. [in()](#158-in)
159. [inIgnoreCase()](#159-inignorecase)
160. [notIn()](#160-notin)
161. [notInIgnoreCase()](#161-notinignorecase)
162. [isEmpty()](#162-isempty)
163. [isNotEmpty()](#163-isnotempty)
164. [isBlank()](#164-isblank)
165. [isNotBlank()](#165-isnotblank)
166. [isNull()](#166-isnull)
167. [isNotNull()](#167-isnotnull)
168. [isText()](#168-istext)
169. [isBoolean()](#169-isboolean)
170. [isNumber()](#170-isnumber)
171. [isEven()](#171-iseven)
172. [isOdd()](#172-isodd)
173. [isArray()](#173-isarray)
174. [isObject()](#174-isobject)
175. [isEmptyArray()](#175-isemptyarray)
176. [isEmptyObject](#176-isemptyobject)
177. [not()](#177-not)
178. [isWeekday()](#178-isweekday)
179. [isWeekend()](#179-isweekend)
180. [isLeapYear()](#180-isleapyear)

Array Functions

181. [size()](#181-size)
182. [lastIndex()](#182-lastindex)
183. [indexOf()](#183-indexof)
184. [lastIndexOf()](#184-lastindexof)
185. [first()](#185-first)
186. [last()](#186-last)
187. [max()](#187-max)
188. [min()](#188-min)
189. [topN()](#189-topn)
190. [bottomN()](#190-bottomn)
191. [sum()](#191-sum)
192. [avg()](#192-avg)
193. [count()](#193-count)
194. [reverse()](#194-reverse)
195. [slice()](#195-slice)
196. [sort()](#196-sort)
197. [distinct()](#197-distinct)
198. [join()](#198-join)
199. [findByMax()](#199-findbymax)
200. [findByMin()](#200-findbymin)
201. [findByNullOrMax()](#201-findbynullormax)
202. [findByNullOrMin()](#202-findbynullormin)
203. [findByMaxOrNull()](#203-findbymaxornull)
204. [findByMinOrNull()](#204-findbyminornull)

Structural Functions

205. [json()](#205-json)
206. [entries()](#206-entries)
207. [keys()](#207-keys)
208. [toArray()](#208-toarray)
209. [toObject()](#209-toobject)
210. [mergeObjects()](#210-mergeobjects)
211. [flatten()](#211-flatten)
212. [map()](#212-map)
213. [field()](#213-field)
214. [group()](#214-group)
215. [unwind()](#215-unwind)

Following are some examples of each function.

### Arithmetic Functions

#### 1. abs()

    -3.14.abs() ==> 3.14

    abs(3.14) ==> 3.14

#### 2. calc()

    1.5.calc(? * 2 + ? / 2) ==> 3.75

    calc(2^8) ==> 256.0

    calc(sqrt(a^2 + b^2), a:3, b:4) ==> 5.0

#### 3. ceil()

    3.14.ceil() ==> 4

    ceil(-3.14) ==> -3

#### 4. floor()

    3.14.floor() ==> 3

    floor(-3.14) ==> -4

#### 5. mod()

    8.mod(3) ==> 2

    8.mod(?, 3) ==> 2

    mod(-8, 3) ==> 1

    3.mod(-8, ?) ==> 1

#### 6. round()

    3.14.round(1) ==> 3.1

    3.14.round(?, 1) ==> 3.1

    round(3.56, 0) ==> 4

### String Functions

#### 7. abbreviate()

    'abcdefghijkl'.abbreviate(9) ==> "abcdef..."

    'abcdefghijkl'.abbreviate(5, 9) ==> "...fgh..."

    'abcdefghijkl'.abbreviate(?, 7, 9) ==> "...ghijkl"

    abbreviate('abcdefghijkl', 0, 9) ==> "abcdef..."

    abbreviate('abcdefghijkl', 1, 9) ==> "abcdef..."

    abbreviate('abcdefghijkl', 4, 9) ==> "abcdef..."

    abbreviate('abcdefghijkl', 5, 9) ==> "...fgh..."

    abbreviate('abcdefghijkl', 6, 9) ==> "...ghijkl"

    abbreviate('abcdefghijkl', 10, 9) ==> "...ghijkl"

    abbreviate('abcdefghijkl', 11, 9) ==> "...ghijkl"

#### 8. append()

    'abc'.append('xyz') ==> "abcxyz"

    'abc'.append(?, 'xyz') ==> "abcxyz"

    append('abcxyz', 'xyz') ==> "abcxyzxyz"

    'xyz'.append('abcXYZ', ?) ==> "abcXYZxyz"

#### 9. appendIfMissing()

    'abc'.appendIfMissing('xyz') ==> "abcxyz"

    'abc'.appendIfMissing(?, 'xyz') ==> "abcxyz"

    appendIfMissing('abcxyz', 'xyz') ==> "abcxyz"

    'xyz'.appendIfMissing('abcXYZ', ?) ==> "abcXYZxyz"

#### 10. appendIfMissingIgnoreCase()

    'abc'.appendIfMissingIgnoreCase('xyz') ==> "abcxyz"

    'abc'.appendIfMissingIgnoreCase(?, 'xyz') ==> "abcxyz"

    appendIfMissingIgnoreCase('abcxyz', 'xyz') ==> "abcxyz"

    'xyz'.appendIfMissingIgnoreCase('abcXYZ', ?) ==> "abcXYZ"

#### 11. capitalize()

    'cat'.capitalize() ==> "Cat"

    capitalize('cAt') ==> "CAt"

#### 12. center()

    'abc'.center(7) ==> "  abc  "

    'abc'.center(7, 'X') ==> "XXabcXX"

    'abc'.center(?, 7, upperCase(?)) ==> "ABabcAB"

    center('abc', 7, '') ==> "  abc  "

    4.center('a', ?, 'yz') ==> "yayz"

#### 13. concat()

    'Hello'.concat(2022, '... ', ?, ' World!') ==> "2022... Hello World!"

    json('{"a":"Hello","c":" World!"}').concat(a,b,c) ==> !unresolvable!

#### 14. concatFree()

    'Hello'.concatFree(2022, '... ', ?, ' World!') ==> "2022... Hello World!"

    json('{"a":"Hello","c":" World!"}').concatFree(a,b,c) ==> "Hello World!"

#### 15. keepAfter()

    'abcxmnxyz'.keepAfter('x') ==> "mnxyz"

    'abcxmnxyz'.keepAfter(?, 'X') ==> ""

    keepAfter('abcxmnxyz', 'mn') ==> "xyz"

#### 16. keepAfterIgnoreCase()

    'abcxmnxyz'.keepAfterIgnoreCase('x') ==> "mnxyz"

    'abcxmnxyz'.keepAfterIgnoreCase(?, 'X') ==> "mnxyz"

    keepAfterIgnoreCase('abcxmnxyz', 'mn') ==> "xyz"

#### 17. keepAfterLast()

    'abcxmnxyz'.keepAfterLast('x') ==> "yz"

    'abcxmnxyz'.keepAfterLast(?, 'X') ==> ""

    keepAfterLast('abcxmnxyz', 'mn') ==> "xyz"

#### 18. keepAfterLastIgnoreCase()

    'abcxmnxyz'.keepAfterLastIgnoreCase('x') ==> "yz"

    'abcxmnxyz'.keepAfterLastIgnoreCase(?, 'X') ==> "yz"

    keepAfterLastIgnoreCase('abcxmnxyz', 'mn') ==> "xyz"

#### 19. keepBefore()

    'abcxmnxyz'.keepBefore('x') ==> "abc"

    'abcxmnxyz'.keepBefore(?, 'X') ==> ""

    keepBefore('abcxmnxyz', 'mn') ==> "abcx"

#### 20. keepBeforeIgnoreCase()

    'abcxmnxyz'.keepBeforeIgnoreCase('x') ==> "abc"

    'abcxmnxyz'.keepBeforeIgnoreCase(?, 'X') ==> "abc"

    keepBeforeIgnoreCase('abcxmnxyz', 'mn') ==> "abcx"

#### 21. keepBeforeLast()

    'abcxmnxyz'.keepBeforeLast('x') ==> "abcxmn"

    'abcxmnxyz'.keepBeforeLast(?, 'X') ==> ""

    keepBeforeLast('abcxmnxyz', 'mn') ==> "abcx"

#### 22. keepBeforeLastIgnoreCase()

    'abcxmnxyz'.keepBeforeLastIgnoreCase('x') ==> "abcxmn"

    'abcxmnxyz'.keepBeforeLastIgnoreCase(?, 'X') ==> "abcxmn"

    keepBeforeLastIgnoreCase('abcxmnxyz', 'mn') ==> "abcx"

#### 23. leftPad()

    'bat'.leftPad(5) ==> "  bat"

    'bat'.leftPad(?, 8, 'yz') ==> "yzyzybat"

    leftPad('bat', 3, 'yz') ==> "bat"

    5.leftPad('bat', ?, '') ==> "  bat"

#### 24. length()

    'Josson'.length() ==> 6

    length('Josson') ==> 6

    length(2022) ==> 4

#### 25. lowerCase()

    'Cat'.lowerCase() ==> "cat"

    lowerCase('cAt') ==> "cat"

#### 26. notEmpty()

    'abc'.notEmpty('xyz') ==> "abc"

    ''.notEmpty(null, '', 'xyz') ==> "xyz"

    json('{"a":"","b":"","c":"abc"}').notEmpty(a,b,c,'xyz') ==> "abc"

#### 27. notBlank()

    'abc'.notBlank('xyz') ==> "abc"

    ' '.notBlank(null, '  ', 'xyz') ==> "xyz"

    json('{"a":" ","b":" ","c":"abc"}').notBlank(a,b,c,'xyz') ==> "abc"

#### 28. prepend()

    'abc'.prepend('xyz') ==> "xyzabc"

    'abc'.prepend(?, 'xyz') ==> "xyzabc"

    prepend('xyzabc', 'xyz') ==> "xyzxyzabc"

    'xyz'.prepend('XYZabc', ?) ==> "xyzXYZabc"

#### 29. prependIfMissing()

    'abc'.prependIfMissing('xyz') ==> "xyzabc"

    'abc'.prependIfMissing(?, 'xyz') ==> "xyzabc"

    prependIfMissing('xyzabc', 'xyz') ==> "xyzabc"

    'xyz'.prependIfMissing('XYZabc', ?) ==> "xyzXYZabc"

#### 30. prependIfMissingIgnoreCase()

    'abc'.prependIfMissingIgnoreCase('xyz') ==> "xyzabc"

    'abc'.prependIfMissingIgnoreCase(?, 'xyz') ==> "xyzabc"

    prependIfMissingIgnoreCase('xyzabc', 'xyz') ==> "xyzabc"

    'xyz'.prependIfMissingIgnoreCase('XYZabc', ?) ==> "XYZabc"

#### 31. removeEnd()

    'www.domain.com'.removeEnd('.com') ==> "www.domain"

    'www.domain.com'.removeEnd(?, '.Com') ==> "www.domain.com"

    removeEnd('www.domain.com', '.com') ==> "www.domain"

#### 32. removeEndIgnoreCase()

    'www.domain.COM'.removeEndIgnoreCase('.com') ==> "www.domain"

    'www.domain.com'.removeEndIgnoreCase(?, '.Com') ==> "www.domain"

    removeEndIgnoreCase('www.domain.com', '.COM') ==> "www.domain"

#### 33. removeStart()

    'www.domain.com'.removeStart('www.') ==> "domain.com"

    'www.domain.com'.removeStart(?, '.Www') ==> "www.domain.com"

    removeStart('www.domain.com', 'www.') ==> "domain.com"

#### 34. removeStartIgnoreCase()

    'WWW.domain.com'.removeStartIgnoreCase('www.') ==> "domain.com"

    'www.domain.com'.removeStartIgnoreCase(?, '.Www') ==> "www.domain.com"

    removeStartIgnoreCase('www.domain.com', 'WWW.') ==> "domain.com"

#### 35. repeat()

    'a'.repeat(3) ==> "aaa"

    'ab'.repeat(?, 2) ==> "abab"

    repeat('abc', 2) ==> "abcabc"

    3.repeat('abc', ?) ==> "abcabcabc"

#### 36. replace()

    'abaa'.replace('a', 'z') ==> "zbzz"

    'abaa'.replace(?, 'a', 'z', -1) ==> "zbzz"

    replace('abaa', 'a', '', -1) ==> "b"

    replace('abaa', 'A', 'z', 1) ==> "abaa"

    'a'.replace('abaa', ?, 'z', 2) ==> "zbza"

#### 37. replaceIgnoreCase()

    'abaa'.replaceIgnoreCase('a', 'z') ==> "zbzz"

    'abaa'.replaceIgnoreCase(?, 'a', 'z', -1) ==> "zbzz"

    replaceIgnoreCase('abaa', 'a', '', -1) ==> "b"

    replaceIgnoreCase('abaa', 'A', 'z', 1) ==> "zbaa"

    'a'.replaceIgnoreCase('abaa', ?, 'z', 2) ==> "zbza"

#### 38. rightPad()

    'bat'.rightPad(5) ==> "bat  "

    'bat'.rightPad(?, 8, 'yz') ==> "batyzyzy"

    rightPad('bat', 3, 'yz') ==> "bat"

    rightPad('bat', 5, '') ==> "bat  "

#### 39. split()

    'abc def'.split() ==> [ "abc", "def" ]

    'abc  def'.split(' ') ==> [ "abc", "def" ]

    ' abc  def '.split(?, ' ') ==> [ "abc", "def" ]

    split('ab:cd:ef', ':') ==> [ "ab", "cd", "ef" ]

#### 40. strip()

    '  abc  '.strip(' ') ==> "abc"

    '  abcyx'.strip('xyz') ==> "  abc"

    strip('z abcyx', 'xyz') ==> " abc"

#### 41. stripEnd()

    '  abc  '.stripEnd(' ') ==> "  abc"

    'z abcyx'.stripEnd('xyz') ==> "z abc"

    stripEnd('z abcyx', 'xyz') ==> "z abc"

#### 42. stripStart()

    '  abc  '.stripStart(' ') ==> "abc  "

    'z abcyx'.stripStart('xyz') ==> " abcyx"

    stripStart('z abcyx', 'xyz') ==> " abcyx"

#### 43. substr()

    'abc'.substr(1) ==> "bc"

    'abc'.substr(0, 2) ==> "ab"

    'abc'.substr(?, 1, 2) ==> "b"

    substr('abc', -2, -1) ==> "b"

    2.substr('abc', -4, ?) ==> "ab"

#### 44. trim()

    'abc'.trim() ==> "abc"

    trim('  abc  ') ==> "abc"

#### 45. uncapitalize()

    'Cat'.uncapitalize() ==> "cat"

    uncapitalize('CAt') ==> "cAt"

#### 46. upperCase()

    'Cat'.upperCase() ==> "CAT"

    upperCase('cAt') ==> "CAT"

#### 47. camelCase()

    'i am  a   man .and..i have_a__pen'.camelCase() ==> "iAmAManAndIHaveAPen"

    ' Text  to__c@mel case '.camelCase() ==> "textToC@melCase"

    ' Text  to__c@mel case '.camelCase(' _.@') ==> "textToCMelCase"

#### 48. upperCamelCase()

    'i am  a   man .and..i have_a__pen'.camelCase() ==> "IAmAManAndIHaveAPen"

    ' Text  to__c@mel case '.camelCase() ==> "TextToC@melCase"

    ' Text  to__c@mel case '.camelCase(' _.@') ==> "TextToCMelCase"

#### 49. snakeCase()

    ' Text  to__snake case '.snakeCase() ==> "Text_to_snake_case"

    'camelToSnakeCase'.snakeCase() ==> "camel_To_Snake_Case"

#### 50. lowerSnakeCase()

    ' Text  to__snake case '.lowerSnakeCase() ==> "text_to_snake_case"

    'camelToSnakeCase'.lowerSnakeCase() ==> "camel_to_snake_case"

#### 51. upperSnakeCase()

    ' Text  to__snake case '.upperSnakeCase() ==> "TEXT_TO_SNAKE_CASE"

    'camelToSnakeCase'.upperSnakeCase() ==> "CAMEL_TO_SNAKE_CASE"

#### 52. camelSnakeCase()

    ' Text  to__snake case '.camelSnakeCase() ==> "Text_To_Snake_Case"

    'camelToSnakeCase'.camelSnakeCase() ==> "Camel_To_Snake_Case"

#### 53. singleQuote() / quote() / q()

    'Peggy''s cat'.singleQuote() ==> "'Peggy''s cat'"

    123.singleQuote() ==> "'123'"

    quote('Raymond''s dog') ==> "'Raymond''s dog'"

    q(True) ==> "'true'"

#### 54. doubleQuote() / qq()

    'Peggy\"s cat'.doubleQuote() ==> "\"Peggy\\\"s cat\""

    12.3.doubleQuote() ==> "\"12.3\""

    qq('Raymond\"s dog') ==> "\"Raymond\\\"s dog\""

    qq(False) ==> "\"false\""

### Date Functions

#### 55. amPmOfDay()

    '2022-01-02T03:04:05'.amPmOfDay() ==> "AM"

    amPmOfDay('2022-02-04T13:14:15') ==> "PM"

#### 56. second()

    '2022-01-02T03:04:05'.second() ==> 5

    second('2022-02-04T13:14:15') ==> 15

#### 57. secondOfDay()

    '2022-01-02T03:04:05'.secondOfDay() ==> 11045

    secondOfDay('2022-02-04T13:14:15') ==> 47655

#### 58. minute()

    '2022-01-02T03:04:05'.minute() ==> 4

    minute('2022-02-04T13:14:15') ==> 14

#### 59. minuteOfDay()

    '2022-01-02T03:04:05'.minuteOfDay() ==> 184

    minuteOfDay('2022-02-04T13:14:15') ==> 794

#### 60. hourOfAmPm()

    '2022-01-02T03:04:05'.hourOfAmPm() ==> 3

    hourOfAmPm('2022-02-04T13:14:15') ==> 1

#### 61. hour()

    '2022-01-02T03:04:05'.hour() ==> 3

    hour('2022-02-04T13:14:15') ==> 13

#### 62. dayOfWeek()

    '2022-01-02T03:04:05'.dayOfWeek() ==> 7

    dayOfWeek('2022-02-04T13:14:15') ==> 5

#### 63. day()

    '2022-01-02T03:04:05'.day() ==> 2

    day('2022-02-04T13:14:15') ==> 4

#### 64. dayOfYear()

    '2022-01-02T03:04:05'.dayOfYear() ==> 2

    dayOfYear('2022-02-04T13:14:15') ==> 35

#### 65. month()

    '2022-01-02T03:04:05'.month() ==> 1

    month('2022-02-04T13:14:15') ==> 2

#### 66. year()

    '2022-01-02T03:04:05'.year() ==> 2022

    year('2022-02-04T13:14:15') ==> 2022

#### 67. plusSeconds()

    '2022-01-02T03:04:05'.plusSeconds(9) ==> "2022-01-02T03:04:14"

    '2022-01-02T03:04:05'.plusSeconds(?, 10) ==> "2022-01-02T03:04:15"

    plusSeconds('2022-02-04T13:14:15', 9) ==> "2022-02-04T13:14:24"

#### 68. plusMinutes()

    '2022-01-02T03:04:05'.plusMinutes(9) ==> "2022-01-02T03:13:05"

    '2022-01-02T03:04:05'.plusMinutes(?, 10) ==> "2022-01-02T03:14:05"

    plusMinutes('2022-02-04T13:14:15', 9) ==> "2022-02-04T13:23:15"

#### 69. plusHours()

    '2022-01-02T03:04:05'.plusHours(9) ==> "2022-01-02T12:04:05"

    '2022-01-02T03:04:05'.plusHours(?, 10) ==> "2022-01-02T13:04:05"

    plusHours('2022-02-04T13:14:15', 9) ==> "2022-02-04T22:14:15"

#### 70. plusDays()

    '2022-01-02T03:04:05'.plusDays(9) ==> "2022-01-11T03:04:05"

    '2022-01-02T03:04:05'.plusDays(?, 10) ==> "2022-01-12T03:04:05"

    plusDays('2022-02-04T13:14:15', 9) ==> "2022-02-13T13:14:15"

#### 71. plusWeeks()

    '2022-01-02T03:04:05'.plusWeeks(9) ==> "2022-03-06T03:04:05"

    '2022-01-02T03:04:05'.plusWeeks(?, 10) ==> "2022-03-13T03:04:05"

    plusWeeks('2022-02-04T13:14:15', 9) ==> "2022-04-08T13:14:15"

#### 72. plusMonths()

    '2022-01-02T03:04:05'.plusMonths(9) ==> "2022-10-02T03:04:05"

    '2022-01-02T03:04:05'.plusMonths(?, 10) ==> "2022-11-02T03:04:05"

    plusMonths('2022-02-04T13:14:15', 9) ==> "2022-11-04T13:14:15"

#### 73. plusYears()

    '2022-01-02T03:04:05'.plusYears(9) ==> "2031-01-02T03:04:05"

    '2022-01-02T03:04:05'.plusYears(?, 10) ==> "2032-01-02T03:04:05"

    plusYears('2022-02-04T13:14:15', 9) ==> "2031-02-04T13:14:15"

#### 74. minusSeconds()

    '2022-01-02T03:04:05'.minusSeconds(9) ==> "2022-01-02T03:03:56"

    '2022-01-02T03:04:05'.minusSeconds(?, 10) ==> "2022-01-02T03:03:55"

    minusSeconds('2022-02-04T13:14:15', 9) ==> "2022-02-04T13:14:06"

#### 75. minusMinutes()

    '2022-01-02T03:04:05'.minusMinutes(9) ==> "2022-01-02T02:55:05"

    '2022-01-02T03:04:05'.minusMinutes(?, 10) ==> "2022-01-02T02:54:05"

    minusMinutes('2022-02-04T13:14:15', 9) ==> "2022-02-04T13:05:15"

#### 76. minusHours()

    '2022-01-02T03:04:05'.minusHours(9) ==> "2022-01-01T18:04:05"

    '2022-01-02T03:04:05'.minusHours(?, 10) ==> "2022-01-01T17:04:05"

    minusHours('2022-02-04T13:14:15', 9) ==> "2022-02-04T04:14:15"

#### 77. minusDays()

    '2022-01-02T03:04:05'.minusDays(9) ==> "2021-12-24T03:04:05"

    '2022-01-02T03:04:05'.minusDays(?, 10) ==> "2021-12-23T03:04:05"

    minusDays('2022-02-04T13:14:15', 9) ==> "2022-01-26T13:14:15"

#### 78. minusWeeks()

    '2022-01-02T03:04:05'.minusWeeks(9) ==> "2021-10-31T03:04:05"

    '2022-01-02T03:04:05'.minusWeeks(?, 10) ==> "2021-10-24T03:04:05"

    minusWeeks('2022-02-04T13:14:15', 9) ==> "2021-12-03T13:14:15"

#### 79. minusMonths()

    '2022-01-02T03:04:05'.minusMonths(9) ==> "2021-04-02T03:04:05"

    '2022-01-02T03:04:05'.minusMonths(?, 10) ==> "2021-03-02T03:04:05"

    minusMonths('2022-02-04T13:14:15', 9) ==> "2021-05-04T13:14:15"

#### 80. minusYears()

    '2022-01-02T03:04:05'.minusYears(9) ==> "2013-01-02T03:04:05"

    '2022-01-02T03:04:05'.minusYears(?, 10) ==> "2012-01-02T03:04:05"

    minusYears('2022-02-04T13:14:15', 9) ==> "2013-02-04T13:14:15"

#### 81. truncateToMicro()

    '2022-01-02T03:04:05.229390600'.truncateToMicro() ==> "2022-01-02T03:04:05.229390"

    truncateToMicro('2022-02-04T13:14:15.229390600') ==> "2022-02-04T13:14:15.229390"

#### 82. truncateToMilli()

    '2022-01-02T03:04:05.229390600'.truncateToMilli() ==> "2022-01-02T03:04:05.229"

    truncateToMilli('2022-02-04T13:14:15.229390600') ==> "2022-02-04T13:14:15.229"

#### 83. truncateToSecond()

    '2022-01-02T03:04:05.229390600'.truncateToSecond() ==> "2022-01-02T03:04:05"

    truncateToSecond('2022-02-04T13:14:15.229390600') ==> "2022-02-04T13:14:15"

#### 84. truncateToMinute()

    '2022-01-02T03:04:05.229390600'.truncateToMinute() ==> "2022-01-02T03:04"

    truncateToMinute('2022-02-04T13:14:15.229390600') ==> "2022-02-04T13:14"

#### 85. truncateToHour()

    '2022-01-02T03:04:05.229390600'.truncateToHour() ==> "2022-01-02T03:00"

    truncateToHour('2022-02-04T13:14:15.229390600') ==> "2022-02-04T13:00"

#### 86. truncateToDay()

    '2022-01-02T03:04:05.229390600'.truncateToDay() ==> "2022-01-02T00:00"

    truncateToDay('2022-02-04T13:14:15.229390600') ==> "2022-02-04T00:00"

#### 87. truncateToMonth()

    '2022-01-02T03:04:05.229390600'.truncateToMonth() ==> "2022-01-01T00:00"

    truncateToMonth('2022-02-04T13:14:15.229390600') ==> "2022-02-01T00:00"

#### 88. truncateToYear()

    '2022-01-02T03:04:05.229390600'.truncateToYear() ==> "2022-01-01T00:00"

    truncateToYear('2022-02-04T13:14:15.229390600') ==> "2022-01-01T00:00"

#### 89. withNano()

    '2022-01-02T03:04'.withNano(789) ==> "2022-01-02T03:04:00.000000789"

    '2022-01-02T03:04'.withNano(?, 789) ==> "2022-01-02T03:04:00.000000789"

    withNano('2022-02-04T13:14', 789) ==> "2022-02-04T13:14:00.000000789"

#### 90. withMicro()

    '2022-01-02T03:04'.withMicro(789) ==> "2022-01-02T03:04:00.000789"

    '2022-01-02T03:04'.withMicro(?, 789) ==> "2022-01-02T03:04:00.000789"

    withMicro('2022-02-04T13:14', 789) ==> "2022-02-04T13:14:00.000789"

#### 91. withMilli()

    '2022-01-02T03:04'.withMilli(789) ==> "2022-01-02T03:04:00.789"

    '2022-01-02T03:04'.withMilli(?, 789) ==> "2022-01-02T03:04:00.789"

    withMilli('2022-02-04T13:14', 789) ==> "2022-02-04T13:14:00.789"

#### 92. withSecond()

    '2022-01-02T03:04'.withSecond(35) ==> "2022-01-02T03:04:35"

    '2022-01-02T03:04'.withSecond(?, 35) ==> "2022-01-02T03:04:35"

    withSecond('2022-02-04T13:14', 35) ==> "2022-02-04T13:14:35"

#### 93. withMinute()

    '2022-01-02T03:04'.withMinute(35) ==> "2022-01-02T03:35"

    '2022-01-02T03:04'.withMinute(?, 35) ==> "2022-01-02T03:35"

    withMinute('2022-02-04T13:14', 35) ==> "2022-02-04T13:35"

#### 94. withHour()

    '2022-01-02T03:04'.withHour(16) ==> "2022-01-02T16:04"

    '2022-01-02T03:04'.withHour(?, 16) ==> "2022-01-02T16:04"

    withHour('2022-02-04T13:14', 16) ==> "2022-02-04T16:14"

#### 95. withDay()

    '2022-01-02T03:04'.withDay(25) ==> "2022-01-25T03:04"

    '2022-01-02T03:04'.withDay(?, 25) ==> "2022-01-25T03:04"

    withDay('2022-02-04T13:14', 25) ==> "2022-02-25T13:14"

#### 96. withDayOfYear()

    '2022-01-02T03:04'.withDayOfYear(123) ==> "2022-05-03T03:04"

    '2022-01-02T03:04'.withDayOfYear(?, 123) ==> "2022-05-03T03:04"

    withDayOfYear('2022-02-04T13:14', 123) ==> "2022-05-03T13:14"

#### 97. withMonth()

    '2022-01-02T03:04'.withMonth(7) ==> "2022-07-02T03:04"

    '2022-01-02T03:04'.withMonth(?, 7) ==> "2022-07-02T03:04"

    withMonth('2022-02-04T13:14', 7) ==> "2022-07-04T13:14"

#### 98. withYear()

    '2022-01-02T03:04'.withYear(2047) ==> "2047-01-02T03:04"

    '2022-01-02T03:04'.withYear(?, 2047) ==> "2047-01-02T03:04"

    withYear('2022-02-04T13:14', 2047) ==> "2047-02-04T13:14"

#### 99. dayEnd()

    '2022-01-02T03:04'.dayEnd() ==> "2022-01-02T23:59:59.999999999"

    dayEnd('2022-02-04T13:14') ==> "2022-02-04T23:59:59.999999999"

#### 100. monthEnd()

    '2022-01-02T03:04'.monthEnd() ==> "2022-01-31T23:59:59.999999999"

    monthEnd('2022-02-04T13:14') ==> "2022-02-28T23:59:59.999999999"

#### 101. yearEnd()

    '2022-01-02T03:04'.yearEnd() ==> "2022-12-31T23:59:59.999999999"

    yearEnd('2022-02-04T13:14') ==> "2022-12-31T23:59:59.999999999"

#### 102. lengthOfMonth()

    '2022-01-02T03:04'.lengthOfMonth() ==> 31

    lengthOfMonth('2022-02-04T13:14') ==> 28

#### 103. lengthOfYear()

    '2022-01-02T03:04'.lengthOfYear() ==> 365

    lengthOfYear('2024-02-04T13:14') ==> 366

#### 104. untilInSecond()

    '2020-01-02T23:04'.untilInSecond('2022-06-11T01:02') ==> 76903080

    untilInSecond('2021-12-12T13:14','2021-03-03T01:00') ==> -24581640

#### 105. untilInMinute()

    '2020-01-02T23:04'.untilInMinute('2022-06-11T01:02') ==> 1281718

    untilInMinute('2021-12-12T13:14','2021-03-03T01:00') ==> -409694

#### 106. untilInHour()

    '2020-01-02T23:04'.untilInHour('2022-06-11T01:02') ==> 21361

    untilInHour('2021-12-12T13:14','2021-03-03T01:00') ==> -6828

#### 107. untilInDay()

    '2020-01-02T23:04'.untilInDay('2022-06-11T01:02') ==> 890

    untilInDay('2021-12-12T13:14','2021-03-03T01:00') ==> -284

#### 108. untilInMonth()

    '2020-01-02T23:04'.untilInMonth('2022-06-11T01:02') ==> 29

    untilInMonth('2021-12-12T13:14','2021-03-03T01:00') ==> -9

#### 109. untilInYear()

    '2020-01-02T23:04'.untilInYear('2022-06-11T01:02') ==> 2

    untilInYear('2021-12-12T13:14','2021-03-03T01:00') ==> 0

#### 110. localToOffsetDate()

    '2022-01-02T03:04:05'.localToOffsetDate() ==> "2022-01-02T03:04:05+08:00"

    localToOffsetDate('2022-02-04T13:14:15') ==> "2022-02-04T13:14:15+08:00"

#### 111. offsetToLocalDate()

    '2022-01-02T03:04:05+08:00'.offsetToLocalDate() ==> "2022-01-02T03:04:05"

    offsetToLocalDate('2022-02-04T13:14:15+08:00') ==> "2022-02-04T13:14:15"

### Format Functions

#### 112. b64Encode()

    'abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ'.b64Encode()
    ==>
    "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg=="

#### 113. b64EncodeNoPadding()

    b64EncodeNoPadding('abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ')
    ==>
    "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg"

#### 114. b64MimeEncode()

    'abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ'.b64MimeEncode()
    ==>
    "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9Q\r\nUVJTVFVWV1hZWg=="

#### 115. b64MimeEncodeNoPadding()

    b64MimeEncodeNoPadding('abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ')
    ==>
    "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9Q\r\nUVJTVFVWV1hZWg"

#### 116. b64UrlEncode()

    'abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ'.b64UrlEncode()
    ==>
    "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp-IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg=="

#### 117. b64UrlEncodeNoPadding()

    b64UrlEncodeNoPadding('abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ')
    ==>
    "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp-IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg"

#### 118. b64Decode()

    'YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg=='.b64Decode()
    ==>
    "abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    b64Decode('YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg')
    ==>
    "abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ"

#### 119. b64MimeDecode()

    'YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9Q\nUVJTVFVWV1hZWg=='.b64MimeDecode()
    ==>
    "abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    b64MimeDecode('YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9Q\r\nUVJTVFVWV1hZWg')
    ==>
    "abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ"

#### 120. b64UrlDecode()

    'YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp-IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg=='.b64UrlDecode()
    ==>
    "abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    b64UrlDecode('YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp-IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg')
    ==>
    "abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ"

#### 121. urlEncode()

    'www.domain.com?a=1+2&b=3+4'.urlEncode() ==> "www.domain.com%3Fa%3D1%2B2%26b%3D3%2B4"

    urlEncode('www.domain.com?a=1+2&b=3+4') ==> "www.domain.com%3Fa%3D1%2B2%26b%3D3%2B4"

#### 122. urlDecode()

    'www.domain.com%3Fa%3D1%2B2%26b%3D3%2B4'.urlDecode() ==> "www.domain.com?a=1+2&b=3+4"

    urlDecode('www.domain.com%3Fa%3D1%2B2%26b%3D3%2B4') ==> "www.domain.com?a=1+2&b=3+4"

#### 123. if()

    json('{"a":1,"b":2,"c":3}').if(a.isEven(), 'T', 'F') ==> "F"

    json('{"a":1,"b":2,"c":3}').if([a=1], 'T', 'F') ==> "T"

    json('{"a":1,"b":2,"c":3}').if([a=1 & b=3], 'T', 'F') ==> "F"

    json('{"a":1,"b":2,"c":3}').if([a=1 & b=3], 'T') ==> !unresolvable!

    json('{"a":1,"b":2,"c":3}').if([a=b], 'T', if([c=3], 'C', 'F')) ==> "C"

    json('[1,2,3,4,5]').if(isOdd(), calc(?*2), ?) ==> [ 2.0, 2, 6.0, 4, 10.0 ]

#### 124. coalesce()

    json('["abc","",123,false,null]').coalesce('xyz') ==> [ "abc", "", 123, false, "xyz" ]

    json('{"a":null,"c":"abc"}').coalesce(a,b,c,'xyz') ==> "abc"

#### 125. caseValue()

    'a'.caseValue('A',1,'b',2,'a',3,4) ==> 3

    'z'.caseValue('A',1,'b',2,'a',3,4) ==> 4

    'z'.caseValue('A',1,'b',2,'a',3) ==> !unresolvable!

    json('[{"s":1},{"s":null},{"s":3}]').s.caseValue(1,'A',null,'B') ==> [ "A", "B", null ]

#### 126. caseValueIgnoreCase()

    'a'.caseValue('A',1,'b',2,'a',3,4) ==> 1

    'z'.caseValue('A',1,'b',2,'a',3,4) ==> 4

    'z'.caseValue('A',1,'b',2,'a',3) ==> !unresolvable!

#### 127. cycleValue()

    0.cycleValue('a','b','c','d') ==> "a"

    1.cycleValue(json('["a","b","c","d"]')) ==> "b"

    '3'.cycleValue('a','b','c','d') ==> "d"

    4.cycleValue('a','b','c','d') ==> "a"

    -1.cycleValue('a','b','c','d') ==> "d"

    -6.cycleValue('a','b','c','d') ==> "c"

#### 128. default()

    json('{"a":1,"b":"B","c":null}').default(x) ==> ""

    json('{"a":1,"b":"B","c":null}').default(x,'Hi') ==> "Hi"

    json('{"a":1,"b":"B","c":null}').default(x,null,c,a,b) ==> 1

    json('{"a":1,"b":"B","c":null}').default(x,null,c,b,a) ==> "B"

#### 129. indexedValue()

    0.indexedValue('a','b','c','d') ==> "a"

    1.indexedValue(json('["a","b","c","d"]')) ==> "b"

    '3'.indexedValue('a','b','c','d') ==> "d"

    4.indexedValue('a','b','c','d') ==> !unresolvable!

    -1.indexedValue('a','b','c','d') ==> !unresolvable!

#### 130. formatDate()

    '2022-01-02T03:04:05'.formatDate('dd/MM/yyyy HH:mm:ss') ==> "02/01/2022 03:04:05"

    '2022-01-02T03:04:05'.formatDate(?, 'yyyy-MM-dd') ==> "2022-01-02"

    formatDate('2022-01-02T03:04:05', 'EEE, MMM d, yyyy') ==> "Sun, Jan 2, 2022"

#### 131. formatNumber()

    12345.6.formatNumber('HK$#,##0.00') ==> "HK$12,345.60"

    123.formatNumber(?, '#,##0.#') ==> "123"

    formatNumber(123.45, '#,##0.#') ==> "123.5"

#### 132. formatText()

    'Dog'.formatText('[%-5s]') ==> "[Dog  ]"

    123.formatText(?, '[%5d]') ==> "[  123]"

    formatText('Dog', '[%5s]') ==> "[  Dog]"

#### 133. formatTexts()

    formatTexts('1:%s 2:%s 3:%s', 'a', 'b', 'c') ==> "1:a 2:b 3:c"

    'b'.formatTexts('1:%s 2:%s 3:%s', 'a', ?, 'c') ==> "1:a 2:b 3:c"

    json('{"A":"a","B":"b"}').formatTexts('1:%s 2:%s 3:%s', A, B, 'c') ==> "1:a 2:b 3:c"

    json('[{"a":1,"b":3},{"a":2,"b":4}]').formatTexts('a=%d b=%d',a,b) ==> [ "a=1 b=3", "a=2 b=4" ]

#### 134. toNumber()

    '123'.toNumber() ==> 123.0

    toNumber('abc') ==> 0.0

    toNumber(true) ==> 1.0

    toNumber(null) ==> !unresolvable!

    toNumber(json('{"a":1}')) ==> !unresolvable!

    toNumber(json('[1,2.0,"a",true,null]')) ==> [ 1, 2.0, 0.0, 1.0, null ]

#### 135. toString()

    123.toString() ==> "123"

    toString(false) ==> "false"

    toString(null) ==> "null"

    toString(json('{"a":1}')) ==> "{\"a\":1}"

    toString(json('[1,2.0,"a",true,null]')) ==> "[1,2.0,\"a\",true,null]"

#### 136. toText()

    123.toText() ==> "123"

    toText(false) ==> "false"

    toText(null) ==> "null"

    toText(json('{"a":1}')) ==> !unresolvable!

    toText(json('[1,2.0,"a",true,null]')) ==> [ "1", "2.0", "a", "true", "null" ]

#### 137. csv()

    json('{"len1":"12.3\"","len2":null,"len3":"64.0\""}').csv() ==> "\"12.3\"\"\",,\"64.0\"\"\""

    csv(json('[[[[1,2],["3","4\""]]],{"a":1,"b":[2.0,8.888],"c":{"d":true,"e":null}}]')) ==> "1,2,3,\"4\"\"\",1,2.0,8.888,true,"

#### 138. csvShowNull()

    json('{"len1":"12.3\"","len2":null,"len3":"64.0\""}').csvShowNull() ==> "12.3\"\"\",null,\"64.0\"\""

    csvShowNull(json('[[[[1,2],["3","4\""]]],{"a":1,"b":[2.0,8.888],"c":{"d":true,"e":null}}]')) ==> "1,2,3,\"4\"\"\",1,2.0,8.888,true,null"

#### 139. csvParams()

    json('{"len1":"12.3","len2":null,"len3":"64.0\""}').csvParams() ==> "'12.3',null,'64.0\"'"

    csvParams(json('[[[[1,2],["3","4''"]]],{"a":1,"b":[2.0,8.888],"c":{"d":true,"e":null}}]')) ==> "1,2,'3','4''',1,2.0,8.888,true,null"

### Logical Functions

#### 140. contains()

    'abcde'.contains('bc') ==> true

    contains('abcde','B') ==> false

    json('[1.0,2.8,3.0]').contains(?, '1') ==> false

    json('[1.0,2.8,3.0]').contains(1) ==> true

    contains(json('["1","2","3"]'), 2.0) ==> true

    contains(json('[1.0,2.8,3.00]'), '3.0') ==> true

    json('["1.0","2.0","3.0"]').contains(?, '3.0') ==> true

    json('[1,2,null,4]').contains(null) ==> true

    json('{"a":1,"b":2,"c":3}').contains('a') ==> true

#### 141. containsIgnoreCase()

    'abcde'.containsIgnoreCase('bc') ==> true

    containsIgnoreCase('abcde','B') ==> true

    json('["a","b","c"]').containsIgnoreCase(?, 'B') ==> true

    containsIgnoreCase(json('["a","b","c"]'), 'bc') ==> false

    json('{"a":1,"b":2,"c":3}').containsIgnoreCase('A') ==> true

#### 142. notContains()

    'abcde'.notContains('bc') ==> false

    notContains('abcde','B') ==> true

    json('[1.0,2.8,3.0]').notContains(?, 1) ==> false

    json('[1,2,null,4]').notContains(null) ==> false

#### 143. notContainsIgnoreCase()

    'abcde'.notContainsIgnoreCase('bc') ==> false

    notContainsIgnoreCase('abcde','B') ==> false

    json('["a","b","c"]').notContainsIgnoreCase(?, 'D') ==> true

#### 144. startsWith()

    'abcdef'.startsWith('abc') ==> true

    'ABCDEF'.startsWith(?,'abc') ==> false

    startsWith('ABCDEF','cde') ==> false

#### 145. startsWithIgnoreCase()

    'abcdef'.startsWithIgnoreCase('abc') ==> true

    'ABCDEF'.startsWithIgnoreCase(?,'abc') ==> true

    startsWithIgnoreCase('ABCDEF','cde') ==> false

#### 146. notStartsWith()

    'abcdef'.notStartsWith('abc') ==> false

    'ABCDEF'.notStartsWith(?,'abc') ==> true

    notStartsWith('ABCDEF','cde') ==> true

#### 147. notStartsWithIgnoreCase()

    'abcdef'.notStartsWithIgnoreCase('abc') ==> false

    'ABCDEF'.notStartsWithIgnoreCase(?,'abc') ==> false

    notStartsWithIgnoreCase('ABCDEF','cde') ==> true

#### 148. endsWith()

    'abcdef'.endsWith('def') ==> true

    'ABCDEF'.endsWith(?,'def') ==> false

    endsWith('ABCDEF','cde') ==> false

#### 149. endsWithIgnoreCase()

    'abcdef'.endsWithIgnoreCase('def') ==> true

    'ABCDEF'.endsWithIgnoreCase(?,'def') ==> true

    endsWithIgnoreCase('ABCDEF','cde') ==> false

#### 150. notEndsWith()

    'abcdef'.notEndsWith('def') ==> false

    'ABCDEF'.notEndsWith(?,'def') ==> true

    notEndsWith('ABCDEF','cde') ==> true

#### 151. notEndsWithIgnoreCase()

    'abcdef'.notEndsWithIgnoreCase('def') ==> false

    'ABCDEF'.notEndsWithIgnoreCase(?,'def') ==> false

    notEndsWithIgnoreCase('ABCDEF','cde') ==> true

#### 152. equals()

    'abc'.equals('abc') ==> true

    'abc'.equals(?,' abc') ==> false

    equals('ABC','abc') ==> false

#### 153. equalsIgnoreCase()

    'abc'.equalsIgnoreCase('abc') ==> true

    'abc'.equalsIgnoreCase(?,' abc') ==> false

    equalsIgnoreCase('ABC','abc') ==> true

#### 154. notEquals()

    'abc'.notEquals('abc') ==> false

    'abc'.notEquals(?, ' abc') ==> true

    notEquals('ABC','abc') ==> true

#### 155. notEqualsIgnoreCase()

    'abc'.notEqualsIgnoreCase('abcd') ==> true

    'abc'.notEqualsIgnoreCase(' abc') ==> true

    notEqualsIgnoreCase('ABC','abc') ==> false

#### 156. matches()

    '123a'.matches('^[0-9]+$') ==> false

    '784238'.matches(?,'^[0-9]+$') ==> true

    matches('63 56','^[0-9]+$') ==> false

#### 157. notMatches()

    '1234-123456'.notMatches('\\d{4}-\\d{6}') ==> false

    '888-123456'.notMatches(?,'\\d{4}-\\d{6}') ==> true

    notMatches('4444-5555','\\d{4}-\\d{6}') ==> true

#### 158. in()

    56.in(12,34,56) ==> true

    '56'.in(12,34,56) ==> true

    'A'.in(json('["a","b","c"]')) ==> false

#### 159. inIgnoreCase()

    'A'.inIgnoreCase('a','b','c') ==> true

    'a '.inIgnoreCase('a','b','c') ==> false

#### 160. notIn()

    56.notIn(12,34,56) ==> false

    '56'.notIn(12,34,56) ==> false

    'A'.notIn(json('["a","b","c"]')) ==> true

#### 161. notInIgnoreCase()

    'A'.notInIgnoreCase('a','b','c') ==> false

    'a '.notInIgnoreCase('a','b','c') ==> true

#### 162. isEmpty()

    ''.isEmpty() ==> true

    isEmpty(' ') ==> false

    isEmpty(1) ==> false

    isEmpty(true) ==> false

    isEmpty(null) ==> true

    isEmpty(json('[""," ",0,false,null]')) ==> [ true, false, false, false, true ]

#### 163. isNotEmpty()

    ''.isNotEmpty() ==> false

    isNotEmpty(' ') ==> true

    isNotEmpty(1) ==> true

    isNotEmpty(true) ==> true

    isNotEmpty(null) ==> false

    isNotEmpty(json('[""," ",0,false,null]')) ==> [ false, true, true, true, false ]

#### 164. isBlank()

    ''.isBlank() ==> true

    isBlank(' ') ==> true

    isBlank(json('[""," ","X",0,false,null]')) ==> [ true, true, false, false, false, false ]

#### 165. isNotBlank()

    ''.isNotBlank() ==> false

    isNotBlank(' ') ==> false

    isNotBlank(json('[""," ","X",0,false,null]')) ==> [ false, false, true, false, false, false ]

#### 166. isNull()

    null.isNull() ==> !unresolvable!

    isNull(null) ==> true

    isNull('') ==> false

    isNull(json('["text",1,true,null]')) ==> [ false, false, false, true ]

#### 167. isNotNull()

    null.isNotNull() ==> !unresolvable!

    isNotNull(null) ==> false

    isNotNull('') ==> true

    isNotNull(json('["text",1,true,null]')) ==> [ true, true, true, false ]

#### 168. isText()

    'text'.isText() ==> true

    isText(1) ==> false

    isText(true) ==> false

    isText(json('["text",1,true,null]')) ==> [ true, false, false, false ]

#### 169. isBoolean()

    'text'.isBoolean() ==> false

    isBoolean(1) ==> false

    isBoolean(true) ==> true

    isBoolean(json('["text",1,true,null]')) ==> [ false, false, true, false ]

#### 170. isNumber()

    'text'.isNumber() ==> false

    isNumber(1) ==> true

    isNumber(true) ==> false

    isNumber(json('["text",1,true,null]')) ==> [ false, true, false, false ]

#### 171. isEven()

    1.isEven() ==> false

    isEven(2) ==> true

    isEven(json('["text",1,2,null]')) ==> [ false, false, true, false ]

#### 172. isOdd()

    1.isOdd() ==> true

    isOdd(2) ==> false

    isOdd(json('["text",1,2,null]')) ==> [ false, true, false, false ]

#### 173. isArray()

    'text'.isArray() ==> false

    isArray(1) ==> false

    isArray(null) ==> false

    json('[1,2]').isArray() ==> true

    isArray(json('{"a":1}')) ==> false

#### 174. isObject()

    'text'.isObject() ==> false

    isObject(1) ==> false

    isObject(null) ==> false

    json('[1,2]').isObject() ==> false

    isObject(json('{"a":1}')) ==> true

#### 175. isEmptyArray()

    json('[]').isEmptyArray() ==> true

    isEmptyArray(json('[0]')) ==> false

#### 176. isEmptyObject()

    json('{}').isEmptyObject() ==> true

    isEmptyObject(json('{"a":1}')) ==> false

#### 177. not()

    true.not() ==> false

    not(false) ==> true

    not('false') ==> false

    not(0) ==> false

    not(null) ==> false

#### 178. isWeekday

    '2021-12-31T00:00:00'.isWeekday() ==> true

    isWeekday('2022-01-01T00:00:00') ==> false

#### 179. isWeekend

    '2021-12-31T00:00:00'.isWeekend() ==> false

    isWeekend('2022-01-01T00:00:00') ==> true

#### 180. isLeapYear

    '2020-12-31T00:00:00'.isLeapYear() ==> true

    isLeapYear('2022-01-01T00:00:00') ==> false

### Array Functions

#### 181. size()

    json('[7,1,9,null,5,3]').size() ==> 6

    size(json('[7,1,9,null,5,3]')) ==> 6

#### 182. lastIndex()

    json('[7,1,9,null,5,3]').lastIndex() ==> 5

    lastIndex(json('[7,1,9,null,5,3]')) ==> 5

#### 183. indexOf()

    json('[1,1,3,5,null,3,7,3,9]').indexOf(3) ==> 2

    json('[1,1,3,5,null,3,7,3,9]').indexOf(?, '1') ==> 0

    indexOf(json('[1,1,3,5,null,3,7,3,9]'), null) ==> 4

#### 184. lastIndexOf()

    json('[1,1,3,5,null,3,7,3,9]').lastIndexOf(3) ==> 7

    json('[1,1,3,5,null,3,7,3,9]').lastIndexOf(?, '1') ==> 1

    lastIndexOf(json('[1,1,3,5,null,3,7,3,9]'), null) ==> 4

#### 185. first()

    json('[7,1,9,null,5,3]').first() ==> 7

    first(json('[null,7,1,9,5,3]')) ==> null

#### 186. last()

    json('[7,1,9,null,5,3]').last() ==> 3

    last(json('[7,1,9,5,3,null]')) ==> null

#### 187. max()

    json('[7,1,9,null,5,3]').max() ==> 9

    max(json('[7,1,9,null,5,3]'), 15, 16) ==> 16

#### 188. min()

    json('[7,1,9,null,5,3]').min() ==> 1

    min(json('[7,1,9,null,5,3]'), 15, 16) ==> 1

#### 189. topN()

    json('[7,1,9,null,5,3]').topN(2) ==> [ 9, 7 ]

    topN(json('[7,1,9,null,5,3]'), 6) ==> [ 9, 7, 5, 3, 1 ]

#### 190. bottomN()

    json('[7,1,9,null,5,3]').bottomN(2) ==> [ 1, 3 ]

    bottomN(json('[7,1,9,null,5,3]'), 6) ==> [ 1, 3, 5, 7, 9 ]

#### 191. sum()

    json('[7,1,9,null,5,3]').sum() ==> 25.0

    sum(json('[7,1,9,null,5,3]'), 15, 16) ==> 56.0

#### 192. avg()

    json('[7,1,9,null,5,3]').avg() ==> 5.0

    avg(json('[7,1,9,null,5,3]'), 15, 16) ==> 8.0

#### 193. count()

    json('[7,1,9,null,5,3]').count() ==> 5

    count(json('[7,1,9,null,5,3]'), 15, 16) ==> 7

#### 194. reverse()

    json('[7,1,9,null,5,3]').reverse() ==> [ 3, 5, null, 9, 1, 7 ]

    reverse(json('[7,1,9,null,5,3]')) ==> [ 3, 5, null, 9, 1, 7 ]

#### 195. slice()

    json('[1,2,3,4,5,6,7,8,9]').slice(3) ==> [ 4, 5, 6, 7, 8, 9 ]

    json('[1,2,3,4,5,6,7,8,9]').slice(2,8) ==> [ 3, 4, 5, 6, 7, 8 ]

    json('[1,2,3,4,5,6,7,8,9]').slice(,5) ==> [ 1, 2, 3, 4, 5 ]

    json('[1,2,3,4,5,6,7,8,9]').slice(-5) ==> [ 5, 6, 7, 8, 9 ]

    json('[1,2,3,4,5,6,7,8,9]').slice(?,1,8,2) ==> [ 2, 4, 6, 8 ]

    json('[1,2,3,4,5,6,7,8,9]').slice(?,2,,2) ==> [ 3, 5, 7, 9 ]

    slice(json('[1,2,3,4,5,6,7,8,9]'),6,2,1) ==> [ 7, 6, 5, 4 ]

    slice(json('[1,2,3,4,5,6,7,8,9]'),,,3) ==> [ 1, 4, 7 ]

    slice(json('[1,2,3,4,5,6,7,8,9]'),,-5,1) ==> [ 1, 2, 3, 4 ]

#### 196. sort()

    json('[1,1,3,5,3,7,3,9]').sort() ==> [ 1, 1, 3, 3, 3, 5, 7, 9 ]

    json('[1,1,3,5,3,7,3,9]').sort(?,-1) ==> [ 9, 7, 5, 3, 3, 3, 1, 1 ]

    json('[{"seq":4,"val":"A"},{"seq":1,"val":"B"},{"seq":3,"val":"C"},{"seq":2,"val":"D"}]').sort(seq)
    ==>
    [ {
      "seq" : 1,
      "val" : "B"
    }, {
      "seq" : 2,
      "val" : "D"
    }, {
      "seq" : 3,
      "val" : "C"
    }, {
      "seq" : 4,
      "val" : "A"
    } ]

    json('[{"seq":4,"val":"A"},{"seq":1,"val":"B"},{"seq":3,"val":"C"},{"seq":2,"val":"D"}]').sort(seq,-1)
    ==>
    [ {
      "seq" : 4,
      "val" : "A"
    }, {
      "seq" : 3,
      "val" : "C"
    }, {
      "seq" : 2,
      "val" : "D"
    }, {
      "seq" : 1,
      "val" : "B"
    } ]

#### 197. distinct()

    json('[1,1,3,5,3,7,3,9]').distinct().sort() ==> [ 1.0, 3.0, 5.0, 7.0, 9.0 ]

    distinct(json('["A","Z","a","Z","A","z"]')) ==> [ "A", "a", "Z", "z" ]

    distinct(json('["1","1.0",1,1.0,1.00,true,"true",null,"null"]')) ==> [ "1", "1.0", "null", "true", 1.0, true ]

#### 198. join()

    json('["Hello", ",", "World", "!"]').join() ==> "Hello,World!"

    json('[1,2,3]').join('+') ==> "1+2+3"

    join(json('["A",1,"B","2.00","C",3.00,"D",true,null]'),'/') ==> "A/1/B/2.00/C/3.0/D/true"

#### 199. findByMax()

    json('[{"code":"A","price":8},{"code":"B"},{"code":"C","price":3},{"code":"D","price":8},{"code":"E","price":5}]').findByMax(price)
    ==>
    {
      "code" : "A",
      "price" : 8
    }

    findByMax(json('[{"code":"A","price":8},{"code":"B"},{"code":"C","price":3},{"code":"D","price":8},{"code":"E","price":5}]'), code)
    ==>
    {
      "code" : "E",
      "price" : 5
    }

#### 200. findByMin()

    json('[{"code":"A","price":8},{"code":"B"},{"code":"C","price":3},{"code":"D","price":8},{"code":"E","price":5}]').findByMin(?,price)
    ==>
    {
      "code" : "C",
      "price" : 3
    }

    findByMin(json('[{"code":"A","price":8},{"code":"B"},{"code":"C","price":3},{"code":"D","price":8},{"code":"E","price":5}]'), code)
    ==>
    {
      "code" : "A",
      "price" : 8
    }

#### 201. findByNullOrMax()

    json('[{"code":"A","price":8},{"code":"B"},{"code":"C","price":3},{"code":"D","price":8},{"code":"E","price":5}]').findByNullOrMax(price)
    ==>
    {
      "code" : "B"
    }

    findByNullOrMax(json('[{"code":"A","price":8},{"code":"B"},{"code":"C","price":3},{"code":"D","price":8},{"code":"E","price":5}]'), code)
    ==>
    {
      "code" : "E",
      "price" : 5
    }

#### 202. findByNullOrMin()

    json('[{"code":"A","price":8},{"code":"B"},{"code":"C","price":3},{"code":"D","price":8},{"code":"E","price":5}]').findByNullOrMin(?,price)
    ==>
    {
      "code" : "B"
    }

    findByNullOrMin(json('[{"code":"A","price":8},{"code":"B"},{"code":"C","price":3},{"code":"D","price":8},{"code":"E","price":5}]'), code)
    ==>
    {
      "code" : "A",
      "price" : 8
    }

#### 203. findByMaxOrNull()

    json('[{"code":"A","price":8},{"code":"B"},{"code":"C","price":3},{"code":"D","price":8},{"code":"E","price":5}]').findByMaxOrNull(price)
    ==>
    {
      "code" : "A",
      "price" : 8
    }

    findByMaxOrNull(json('[{"code":"A","price":8},{"code":"B"},{"code":"C","price":3},{"code":"D","price":8},{"code":"E","price":5}]'), code)
    ==>
    {
      "code" : "E",
      "price" : 5
    }

#### 204. findByMinOrNull()

    json('[{"code":"A","price":8},{"code":"B"},{"code":"C","price":3},{"code":"D","price":8},{"code":"E","price":5}]').findByMinOrNull(?,price)
    ==>
    {
      "code" : "C",
      "price" : 3
    }

    findByMinOrNull(json('[{"code":"A","price":8},{"code":"B"},{"code":"C","price":3},{"code":"D","price":8},{"code":"E","price":5}]'), code)
    ==>
    {
      "code" : "A",
      "price" : 8
    }

### Structural Functions

#### 205. json()

    json('[1,"2",{"a":1,"b":2}]')
    ==>
    [ 1, "2", {
      "a" : 1,
      "b" : 2
    } ]

    '{"a":1,"b":[2,3],"c":{"d":4,"e":5}}'.json()
    ==>
    {
      "a" : 1,
      "b" : [ 2, 3 ],
      "c" : {
        "d" : 4,
        "e" : 5
      }
    }

#### 206. entries()

    json('{"a":1,"b":[2,3],"c":{"d":4,"e":5}}').entries()
    ==>
    [ {
      "key" : "a",
      "value" : 1
    }, {
      "key" : "b",
      "value" : [ 2, 3 ]
    }, {
      "key" : "c",
      "value" : {
        "d" : 4,
        "e" : 5
      }
    } ]

#### 207. keys()

    json('{"a":1,"b":[2,3],"c":{"d":4,"e":5}}').keys() ==> [ "a", "b", "c" ]

    json('{"a":1,"b":[2,3],"c":{"d":4,"e":5}}').keys(2) ==> [ "a", "b", "c", "d", "e" ]

    keys(json('{"a":1,"b":[2,3],"c":{"d":4,"e":5}}'), -1) ==> [ "a", "b", "c", "d", "e" ]

#### 208. toArray()

    json('{"a":1,"b":[2,3],"c":{"d":4,"e":5}}').toArray()
    ==>
    [ 1, [ 2, 3 ], {
      "d" : 4,
      "e" : 5
    } ]

    json('{"a":1,"b":[2,3],"c":{"d":4,"e":5}}').toArray(c) ==> [ 4, 5 ]

    toArray(json('{"a":1,"b":[2,3],"c":{"d":4,"e":5}}').toArray()) ==> [ 1, 2, 3, 4, 5 ]

#### 209. toObject()

    'a'.toObject('text')
    ==>
    {
      "text" : "a"
    }

    99.toObject('number')
    ==>
    {
      "number" : 99
    }

    json('[1,2,3]').toObject('array')
    ==>
    {
      "array" : [ 1, 2, 3 ]
    }

    json('{"a":1,"b":2}').toObject('obj')
    ==>
    {
      "obj" : {
        "a" : 1,
        "b" : 2
      }
    }

#### 210. mergeObjects()

    json('[{"a":1,"x":11},{"b":2,"y":12},{"c":3,"x":13}]').mergeObjects()
    ==>
    {
      "a" : 1,
      "x" : 13,
      "b" : 2,
      "y" : 12,
      "c" : 3
    }

    mergeObjects(json('[{"a":1,"x":11},{"b":2,"y":12}]'), json('{"c":3,"x":13}'))
    ==>
    {
      "a" : 1,
      "x" : 13,
      "b" : 2,
      "y" : 12,
      "c" : 3
    }

#### 211. flatten()

    json('[[[[1,2],[3,4]],[[5,6],[7,8]]],[[[9,10],[11,12]],[[13,14],[15,16]]]]').flatten()
    ==>
    [ [ [ 1, 2 ], [ 3, 4 ] ], [ [ 5, 6 ], [ 7, 8 ] ], [ [ 9, 10 ], [ 11, 12 ] ], [ [ 13, 14 ], [ 15, 16 ] ] ]

    json('[[[[1,2],[3,4]],[[5,6],[7,8]]],[[[9,10],[11,12]],[[13,14],[15,16]]]]').flatten(2)
    ==>
    [ [ 1, 2 ], [ 3, 4 ], [ 5, 6 ], [ 7, 8 ], [ 9, 10 ], [ 11, 12 ], [ 13, 14 ], [ 15, 16 ] ]

    flatten(json('[[[[1,2],[3,4]],[[5,6],[7,8]]],[[[9,10],[11,12]],[[13,14],[15,16]]]]'), 3)
    ==>
    [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 ]

#### 212. map()

    json('{"a":1,"b":[2,3],"c":{"d":4,"e":5}}').map(c.e,c.d,b,a)
    ==>
    {
      "e" : 5,
      "d" : 4,
      "b" : [ 2, 3 ],
      "a" : 1
    }

    json('{"a":1,"b":[2,3],"c":{"d":4,"e":5}}').map(cc:c.map(dd:d,ee:e),xx:map(aa:a,bb:b))
    ==>
    {
      "cc" : {
        "dd" : 4,
        "ee" : 5
      },
      "xx" : {
        "aa" : 1,
        "bb" : [ 2, 3 ]
      }
    }

#### 213 field()

    json('{"a":1,"b":[2,3],"c":{"d":4,"e":5}}').field(f:6,c:)
    ==>
    {
      "a" : 1,
      "b" : [ 2, 3 ],
      "f" : 6
    }

    json('{"id":"1782-734828-A","name":"Cyron"}').field(id.split('-')@.repeat('X',length()).@join('-'))
    ==>
    {
      "id" : "XXXX-XXXXXX-X",
      "name" : "Cyron"
    }

#### 214. group()

    json('[{"a":1,"b":"A"},{"a":2,"b":"B"},{"a":3,"b":"C"},{"a":2,"b":"D"},{"a":1,"b":"E"}]').group(a)
    ==>
    [ {
      "a" : 1,
      "elements" : [ {
        "a" : 1,
        "b" : "A"
      }, {
        "a" : 1,
        "b" : "E"
      } ]
    }, {
      "a" : 2,
      "elements" : [ {
        "a" : 2,
        "b" : "B"
      }, {
        "a" : 2,
        "b" : "D"
      } ]
    }, {
      "a" : 3,
      "elements" : [ {
        "a" : 3,
        "b" : "C"
      } ]
    } ]

    json('[{"a":1,"b":"A"},{"a":2,"b":"B"},{"a":3,"b":"C"},{"a":2,"b":"D"},{"a":1,"b":"E"}]').group(a,bs:b)
    ==>
    [ {
      "a" : 1,
      "bs" : [ "A", "E" ]
    }, {
      "a" : 2,
      "bs" : [ "B", "D" ]
    }, {
      "a" : 3,
      "bs" : [ "C" ]
    } ]

#### 215. unwind()

    json('[{"a":1,"bs":["A","E"]},{"a":2,"bs":["B","D"]},{"a":3,"bs":["C"]}]').unwind(b:bs)
    ==>
    [ {
      "a" : 1,
      "b" : "A"
    }, {
      "a" : 1,
      "b" : "E"
    }, {
      "a" : 2,
      "b" : "B"
    }, {
      "a" : 2,
      "b" : "D"
    }, {
      "a" : 3,
      "b" : "C"
    } ]

---

## Jossons Basic

Jossons stores JSON datasets in a map of type `Map<String, Josson>` for placeholder resolution.

To create a Jossons object without data.

    Jossons jossons = new Jossons();

To create a Jossons object with given Jackson ObjectNode.
Each entry under root of the ObjectNode will become a member of the default dataset mapping.

    Jossons jossons = Jossons.create(jsonNode);

To create a Jossons object with given JSON string that deserialized to a Jackson ObjectNode.
Each entry under root of the ObjectNode will become a member of the default dataset mapping.

    Jossons jossons = Jossons.fromJsonString("{...}");

To create a Jossons object with given text-based dataset mapping `Map<String, String>`.

    Jossons jossons = Jossons.fromMap(mapping);

To create a Jossons object with given integer-based dataset mapping `Map<String, Integer>`.

    Jossons jossons = Jossons.fromMapOfInt(mapping);

To add more default dataset entry to a Jossons object afterward.

    jossons.putDataset("key", josson);

To tell Jossons what markup language escaping operation is required.
Default is `MarkupLanguage.NONE` for plain text.

    jossons.escapingMarkup(MarkupLanguage.XML);
    jossons.escapingMarkup(MarkupLanguage.HTML);

## Jossons Template Language

### Placeholder

A placeholder is enclosed by double curly braces.
A template is a text based document layout with Jossons placeholders.
It can be any format, such as plain text, HTML or XML.

To present a dataset entry's value content as text.

    {{key}}

To apply a Josson Query on a dataset entry's value.

    {{key->query}}

### Nested Placeholders

Placeholders can be nested and are resolved from inside to outside.
Resolved placeholder is replaced with text and continue for the next round.

__Example__

    |<----------------------------------------------- 3 --------------------------------->|
    |                  |<---------------------------- 2 -------------------------->|      |
    |                  |                     |<------ 1 ------>|                   |      |
    |                  |                     |                 |                   |      |
    {{stock->[itemCode={{order->items[qrCode={{qrCode->quote()}}].itemCode.quote()}}].qty}}

1. `{{qrCode->quote()}}` is resolved to `'1234567890'`
2. `{{order->items[qrCode='1234567890'].itemCode.quote()}}` is resolved to `'ABCDE'`
3. `{{stock->[itemCode='ABCDE'].qty}}` is resolved to `100`

The resolved text value is allowed to contain `{` and `}`.
Any combination of these symbols in the text will not influence the next round of template placeholder resolution.
This mechanism can prevent injection attack.

### Ternary Syntax

The ternary pattern can be repeated with no limit.

    {{boolean ? trueValue : falseValue}}
    {{boolean ? trueValue : boolean ? trueValue : falseValue}}
    {{boolean ? trueValue [: boolean ? trueValue]* : falseValue}}

If all conditions are evaluated to be false and `falseValue` is not given, it returns an empty string.

    {{boolean ? trueValue}}
    {{boolean ? trueValue : boolean ? trueValue}}
    {{boolean ? trueValue [: boolean ? trueValue]*}}

Syntax `?:` is much like `coalesce()` but the checking conditions of `valueAsText` are unresolvable and empty string in addition to null node.

    {{valueAsText ?: valueAsText}}
    {{valueAsText [?: valueAsText]*}}

If `valueAsText` is unresolvable, `valueAsText?` returns an empty string instead of throws `NoValuePresentException`.
The following two statements have the same result.

    {{valueAsText?}}
    {{valueAsText ?: ''}}

The above syntax can be mixed in a placeholder.  For example: 

    {{boolean ? trueValue : anotherValue ?: anotherAnotherValue?}}

### Join Operation

Josson query works on single JSON dataset.
In order to let a placeholder output to include data from two datasets.
It is required to use join operation to build a new dataset for the placeholder.

At least one matching key must be given and the number of key on both side must be the same.
Join operations match `keyL1` with `keyR1`, `keyL2` with `keyR2` and so on.

There are two join types for left and right join.

- _Join One_ - Find the first matched object node and merge the object elements.
- _Join Many_ - Find all matched nodes and embed inside the object as a new array node.

For Join Many operations, the `arrayName:` is optional.
If `arrayName` is not given, the last element name of the query is used.

- _Inner Join_ `>=<`

      "leftQuery{keyL1,keyL2...} >=< rightQuery{keyR1,keyR2...}"

- _Left Join One_ `<=<`

      "leftQuery{keyL1,keyL2...} <=< rightQuery{keyR1,keyR2...}"

- _Right Join One_ `>=>`

      "leftQuery{keyL1,keyL2...} >=> rightQuery{keyR1,keyR2...}"

- _Left Join Many_ `<=<<`

      "leftQuery{keyL1,keyL2...} <=<< rightQuery{arrayName:keyR1,keyR2...}"

- _Right Join Many_ `>>=>`

      "leftQuery{arrayName:keyL1,keyL2...} >>=> rightQuery{keyR1,keyR2...}"

- _Left Excluding Join_ `<!<`

      "leftQuery{keyL1,keyL2...} <!< rightQuery{keyR1,keyR2...}"

- _Right Excluding Join_ `>!>`

      "leftQuery{keyL1,keyL2...} >!> rightQuery{keyR1,keyR2...}"

- _Outer Excluding Join_ `<!>`

      "leftQuery{keyL1,keyL2...} <!> rightQuery{keyR1,keyR2...}"

### Set Operation

Set operations do not need matching key.

- _Left Concatenate_ `<+<`

  Concatenate right into left. Works on two objects or two arrays.

      "leftQuery <+< rightQuery"

- _Right Concatenate_ `>+>`

  Concatenate left into right. Works on two objects or two arrays.

      "leftQuery >+> rightQuery"

- _Subtract Right From Left_ `<-<`

  Set of elements in the left set that are not in the right set. Works on two objects or two arrays.

      "leftQuery <-< rightQuery"

- _Subtract Left From Right_ `>->`

  Set of elements in the right set that are not in the left set. Works on two objects or two arrays.

      "leftQuery >-> rightQuery"

- _Symmetric Difference_ `<->`

  Set of elements in either set but not in the intersection. Works on two objects or two arrays.

      "leftQuery <-> rightQuery"

- _Union_ `<u>`

  Set of all elements in the collection of sets. Works on two arrays.

      "leftQuery <u> rightQuery"

- _Intersection_ `>n<`

  Set of elements that exists in both set. Works on two arrays.

      "leftQuery >n< rightQuery"

### Join and Set Pipe Chaining

The chaining Pipe is used to perform multiple join and set operations within a single expression.
This chaining operation will be chained using the pipe operator `|`.

    ... | {keyL1,keyL2...} <JoinOperator> query{keyR1,keyR2...} | ...

    ... | <SetOperator> query | ...

__Example__

    "queryA{aKey1} >=> queryB{bKey1} | <+< queryC | {cKey1,cKey2} <=<< queryD{arrayName:dKey1,dKey2}"

### Implicit Variables

Key `$` returns a `BooleanNode` with `true` value.

Key `$now` returns a `TextNode` of now with date and time. e.g. `2022-01-01T19:34:47.787144100`

Key `$today` returns a `TextNode` of today's date. e.g. `2022-01-01T00:00`

Key `$yesterday` returns a `TextNode` of yesterday's date. e.g. `2021-12-31T00:00`

Key `$tomorrow` returns a `TextNode` of tomorrow's date. e.g. `2022-01-02T00:00`

Key `$params` returns an `ArrayNode` of a Dictionary Function's parameters in an array.

Key `$0`, `$1`, `$2`... returns a `JsonNode` of a Dictionary Function's individual parameter naming in zero-based index.

### Fill In

Below is the JSON for this tutorial.
The created Jossons object's dataset map has two entries where the keys are "order" and "company".

    {
        "order": {
            "salesOrderId": "SO0001",
            "salesDate": "2022-01-01T10:01:23",
            "salesPerson": "Raymond",
            "customer": {
                "customerId": "CU0001",
                "name": "Peggy",
                "phone": "+852 62000610"
            },
            "items": [
                {
                    "itemCode": "B00001",
                    "name": "WinWin TShirt Series A - 2022",
                    "brand": "WinWin",
                    "property": {
                        "size": "M",
                        "colors": [
                            "WHITE",
                            "RED"
                        ]
                    },
                    "qty": 2,
                    "unit": "Pcs",
                    "unitPrice": 15.0,
                    "tags": [
                        "SHIRT",
                        "WOMEN"
                    ]
                },
                {
                    "itemCode": "A00308",
                    "name": "OctoPlus Tennis Racket - Star",
                    "brand": "OctoPlus",
                    "property": {
                        "colors": [
                            "BLACK"
                        ]
                    },
                    "qty": 1,
                    "unit": "Pcs",
                    "unitPrice": 150.0,
                    "unitDiscount": 10.0,
                    "tags": [
                        "TENNIS",
                        "SPORT",
                        "RACKET"
                    ]
                },
                {
                    "itemCode": "A00201",
                    "name": "WinWin Sport Shoe - Super",
                    "brand": "WinWin",
                    "property": {
                        "size": "35",
                        "colors": [
                            "RED"
                        ]
                    },
                    "qty": 1,
                    "unit": "Pair",
                    "unitPrice": 110.0,
                    "unitDiscount": 10.0,
                    "tags": [
                        "SHOE",
                        "SPORT",
                        "WOMEN"
                    ]
                }
            ],
            "totalAmount": 270.0,
            "discountPct": 5.0,
            "netAmount": 256.5,
            "delivery": {
                "handlingFee": 5.0,
                "address": "Wo Mun Street,\nFanling, N.T.,\nHong Kong",
                "contactPerson": "Cyron",
                "phone": "+852 26004198"
            }
        },
        "company": {
            "name": "Octomix Limited",
            "phone": "+852 12345678",
            "website": "www.octomix.com",
            "address": [
                "888 Queen's Road East",
                "Hong Kong"
            ]
        }
    }

Function `fillInPlaceholder()` uses the stored dataset mapping to merge and fill all placeholders in a template.
Any unresolvable placeholder will raise `NoValuePresentException` with the incomplete merged text content.
All unresolvable placeholders are quoted with `**` to replace the original double curly braces.

    Jossons jossons = Jossons.fromJsonString(orderJsonString);
    String output = jossons.fillInPlaceholder(template);

___Template___

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
    "{{order->items.concat(" +
    "    ##.center(5),' '," +
    "    name.rightPad(35),' '," +
    "    concat(qty,' ',unit).center(8),' '," +
    "    unitPrice.formatNumber('#,##0.0').leftPad(9),' '," +
    "    coalesce(unitDiscount,0).formatNumber('#,##0.0').leftPad(8),' '," +
    "    calc(qty * (unitPrice-d), d:coalesce(unitDiscount,0)).formatNumber('#,##0.0').leftPad(9)," +
    "    '\n      ',itemCode,' '," +
    "    property.entries().concat(key,':',value.toString()).join(' ')" +
    "  ).join('\n')" +
    "}}\n" +
    "----- ----------------------------------- -------- ---------- -------- --------\n" +
    "{{order->totalAmount.formatNumber('US$#,##0.0').leftPad(12).concat('Subtotal:',?,'\n').leftPad(80)}}" +
    "{{order->discountPct > 0 ? order->discountPct.formatNumber('0.0').leftPad(11).concat('Discount:',?,'%\n').leftPad(80)}}" +
    "{{order->delivery.handlingFee!=null ? order->delivery.handlingFee.formatNumber('US$#,##0.0').leftPad(12).concat('Shipping and handling:',?,'\n').leftPad(80)}}" +
    "{{order->calc(netAmount+fee, fee:coalesce(delivery.handlingFee,0)).formatNumber('US$#,##0.0').leftPad(12).concat('Total:',?,'\n').leftPad(80)}}"

1. If `company->address[0]` is unresolvable, 56 spaces are printed.

       {{company->address[0].rightPad(56) ?: $->repeat(' ',56)}}

2. Due date is calculated from one month after the `salesDate`.

       {{order->salesDate.plusMonths(1).formatDate('dd/MM/yyyy')}}

3. "SHIP TO" is not printed if `order->delivery` does not exists.

       {{order->delivery!=null ? 'SHIP TO'}}

4. Delivery contact person is printed only if `order->delivery` is defined.
   If `order->delivery.contactPerson` does not exists, `order->customer.name` is printed instead.

       {{order->delivery!=null ? order->coalesce(delivery.contactPerson,customer.name)}}

5. If `order->delivery.address` exists, split it with delimiter `\n`.
   Then add 31 spaces in front of each line and join them together with `\n`.
   At last, add an extra `\n` at the end.

       {{order->delivery.address!=null ? order->delivery.address.split('\n').concat(repeat(' ',31),?).join('\n').concat(?,'\n')}}

   _Path chart_

       order → delivery{} → address → split(?) → [""]  → [concat(?) ⇒ ""] → join(?[]) ⇒ ""

6. Construct two lines for each item. Each item amount is calculated from `qty`, `unitPrice` and `unitDiscount`. 

       {{
           order->items.concat(
               ##.center(5), ' ',
               name.rightPad(35), ' ',
               concat(qty,' ',unit).center(8), ' ',
               unitPrice.formatNumber('#,##0.0').leftPad(9), ' ',
               coalesce(unitDiscount,0).formatNumber('#,##0.0').leftPad(8), ' ',
               calc(qty * (unitPrice-d), d:coalesce(unitDiscount,0)).formatNumber('#,##0.0').leftPad(9),
               '\n      ', itemCode, ' ',
               property.entries().concat(key,':',value.toString()).join(' ')
           ).join('\n')
       }}

   _Path chart_

       order → items[]* → [{}] → [concat(%) ⇒ ""] → join(?[]) ⇒ ""

7. If `order->discountPct` is not > 0, the discount line is not printed.

       {{order->discountPct > 0 ? order->discountPct.formatNumber('0.0').leftPad(11).concat('Discount:',?,'%\n').leftPad(80)}}

8. Order total amount is calculated by adding `netAmount` and `delivery.handlingFee`.

       {{order->calc(netAmount+fee, fee:coalesce(delivery.handlingFee,0)).formatNumber('US$#,##0.0').leftPad(12).concat('Total:',?,'\n').leftPad(80)}}

___Output___

    Octomix Limited                                                  INVOICE
    
    888 Queen's Road East                                   Issue Date: 01/01/2022
    Hong Kong                                                 Invoice#:   SO0001  
    Phone: +852 12345678                                   Customer ID:   CU0001  
    Website: www.octomix.com                                  Due Date: 01/02/2022
    
    BILL TO                        SHIP TO
    Peggy                          Cyron
    Phone: +852 62000610           Phone: +852 26004198
                                   32 Wo Mun Street,
                                   Fanling, N.T.,
                                   Hong Kong
    
    Item# Description                         Quantity Unit Price Discount    Total
    ----- ----------------------------------- -------- ---------- -------- --------
      1   WinWin TShirt Series A - 2022        2 Pcs        15.0      0.0      30.0
          B00001 size:M colors:["WHITE","RED"]
      2   OctoPlus Tennis Racket - Star        1 Pcs       150.0     10.0     140.0
          A00308 colors:["BLACK"]
      3   WinWin Sport Shoe - Super            1 Pair      110.0     10.0     100.0
          A00201 size:35 colors:["RED"]
    ----- ----------------------------------- -------- ---------- -------- --------
                                                              Subtotal:    US$270.0
                                                              Discount:        5.0%
                                                 Shipping and handling:      US$5.0
                                                                 Total:    US$261.5

## Jossons Resolver

Function `fillInPlaceholderWithResolver()` uses the stored dataset mapping and with the help of on demand callback
dataset resolver to merge and fill all placeholders in a template.

    String output = jossons.fillInPlaceholderWithResolver(template, dictionaryFinder, dataFinder, progress);

The last parameter `progress` is a `ResolverProgress` which record all the resolution progress steps.
By default, the last step "End" is added automatically.

The resolution process has three debug levels:

1. `ResolverDebugLevel.SHOW_CONTENT_OF_VALUE_NODE_ONLY` (default)
2. `ResolverDebugLevel.SHOW_CONTENT_UP_TO_OBJECT_NODE`
3. `ResolverDebugLevel.SHOW_CONTENT_UP_TO_ARRAY_NODE`

Basic constructors and methods:

    ResolverProgress progress = new ResolverProgress();

    ResolverProgress progress = new ResolverProgress("subject");

    progress.debugLevel(ResolverDebugLevel.SHOW_CONTENT_UP_TO_OBJECT_NODE);

    progress.autoMarkEnd(false);

    List<String> steps = progress.getSteps();

### Dictionary Finder

If a key cannot be found in the default dataset mapping during the placeholder resolution process,
the resolver will ask `Function<String, String> dictionaryFinder` for an answer.
`dictionaryFinder` takes an argument `String key` and returns a resolution statement of either:

- A statement that represent a value.

      "1"      // IntNode
      "2.3"    // DoubleNode
      "'Text'" // TextNode
      "true"   // BooleanNode
      "null"   // NullNode

- A Jossons query that retrieve data from other dataset.

      "otherKey->jossonQuery"

- A Jossons query with ternary syntax, please refer to [Ternary Syntax](#ternary-syntax).

      "statement ? otherKey1->jossonQuery : otherKey2->jossonQuery"

- A join operation to merge two datasets, please refer to [Join Operation](#join-operation).

      "leftQuery{keyL1,keyL2...} <=< rightQuery{keyR1,keyR2...}"

- A set operation on two datasets, please refer to [Set Operation](#set-operation).

      "leftQuery <u> rightQuery"

- A database query statement, please refer to [Data Finder](#data-finder).

      "collectionName ? {findStatement}"

Resolved result will be cached with the key name except for key name starts with `$`.
Next time a placeholder or statement query for the same key will return the cached value without evaluation.

### Data Finder

After [Dictionary Finder](#dictionary-finder) returned a valid database query statement,
resolver will further trigger `BiFunction<String, String, Josson> dataFinder` callback.
`dataFinder` takes two arguments `String collectionName` and `String query`, and returns a Josson object.

One-document query syntax that request for an `ObjectNode`:

    "collectionName ? {findStatement}"

    "collectionName ? [aggregateStatements]"

    "? {findStatement}"

    "? [aggregateStatements]"

Many-documents query syntax that request for an `ArrayNode`:

    "collectionName[] ? {findStatement}"

    "collectionName[] ? [aggregateStatements]"

    "[] ? {findStatement}"

    "[] ? [aggregateStatements]"

`collectionName` is optional. If not given, the resolving key will be passed to `dataFinder` in the collection name argument.
For Many-documents query request, the collection name argument has a suffix of `[]`.

[Appendix](#appendix) has an example of MongoDB adapter for this _Data Finder_.

### Dictionary Function

If a `dictionaryFinder` key ends with `()`, then it is a dictionary function.
It's resolution statement can contain the following implicit variables.

- `$params` the calling statement's parameters in an array.
- `$0`, `$1`, `$2`... the calling statement's individual parameter naming in zero-based index.

If it is necessary to pass the values in an array node as the function parameters in the form of (elem0, elem1, elem2...),
use placeholder to transform by Josson function csvParams().

    "customFunction({{arrayNode->csvParams()}})"

#### Examples

_Dictionary finder entries_

    "double()" : "$0->calc(?*2)"

    "sum2num()" : "$->calc({{$0}} + {{$1}})"

    "sum2numThenDouble()" : "double(sum2num({{$0}},{{$1}}))->formatText('({{$0}}+{{$1}})x2 = %.1f')"

    "projectName()" : "$0='CHI' ? '早晨' : 'Josson'"

    "titledList()" : "$params->slice(1).concat(##,'. ',?).join('\n').concat({{$0->quote()}},'\n',{{$0->repeat('=',length()).quote()}},'\n',?)"

_Placeholders_

    {{double(3)}} ==> "6.0"

    {{sum2num(4,5)}} ==> "9.0"

    {{sum2numThenDouble(1,2)}} ==> "(1+2)x2 = 6.0"

    {{projectName()}} ==> "Josson"

    {{projectName('CHI')}} ==> "早晨"

    {{projectName('ENG')}} ==> "Josson"

    {{titledList('List Title','Item A','Item B','Item C')}}
    ==>
    List Title
    ==========
    1. Item A
    2. Item B
    3. Item C

### Put Together

#### An example of _Left Join One_ with _Dictionary Finder_ and _Data Finder_

    Map<String, String> dictionaryFinder = new HashMap<>();
    dictionaryFinder.put("stocks", "[]?{ignoredQuery}");
    dictionaryFinder.put("withStock", "order->items.map(itemCode,qty){itemCode} <=< stocks{itemCode}");

    BiFunction<String, String, Josson> dataFinder = (collectionName, ignoredQuery) -> {
        try {
            if (collectionName.equals("stocks[]")) {
                // Hardcode instead of database query
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

    // Use the JSON data from section "Fill in"
    Jossons jossons = Jossons.fromJsonString(orderJsonString);
    String output = jossons.fillInPlaceholderWithResolver(
        "Order ID : {{order->salesOrderId}}\n" +
        "{{withStock->concat(itemCode.rightPad(10), 'Qty: ', qty, '   Onhand: ', onhandQty).join('\n')}}",
        dictionaryFinder::get, dataFinder, progress));

___Output___

    Order ID : SO0001
    B00001    Qty: 2   Onhand: 231
    A00308    Qty: 1   Onhand: 76
    A00201    Qty: 1   Onhand: 18

___Progress Steps___

    Round 1 : Resolving withStock from order->items.map(itemCode,qty){itemCode} <=< stocks{itemCode}
    Round 1 : Resolving stocks from []?{ignoredQuery}
    Round 1 : Resolved stocks = Array with 5 elements
    Round 2 : Resolved withStock = Array with 3 elements
    Round 3 : End

---

## Appendix

### MongoDB Adapter

Customize a BSON to JSON converter.

    import org.bson.Document;
    import org.bson.json.Converter;
    import org.bson.json.JsonMode;
    import org.bson.json.JsonWriterSettings;
    import org.bson.json.StrictJsonWriter;
    import org.bson.types.ObjectId;
    
    import java.time.Instant;
    import java.time.LocalDateTime;
    import java.time.ZoneId;
    import java.time.format.DateTimeFormatter;
    import java.util.List;
    import java.util.stream.Collectors;
    
    public class Converters {
    
        private static class ObjectIdConverter implements Converter<ObjectId> {
    
            public static final ObjectIdConverter INSTANCE = new ObjectIdConverter();
    
            @Override
            public void convert(ObjectId value, StrictJsonWriter writer) {
                writer.writeString(value.toHexString());
            }
        }
    
        private static class EpochToLocalDateTimeConverter implements Converter<Long> {
    
            public static final EpochToLocalDateTimeConverter INSTANCE = new EpochToLocalDateTimeConverter();
    
            @Override
            public void convert(Long value, StrictJsonWriter writer) {
                LocalDateTime date = LocalDateTime.ofInstant(Instant.ofEpochMilli(value), ZoneId.of("Asia/Hong_Kong"));
                writer.writeString(date.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME));
            }
        }
    
        private static final JsonWriterSettings JSON_WRITER_SETTINGS = JsonWriterSettings
            .builder()
            .outputMode(JsonMode.RELAXED)
            .objectIdConverter(ObjectIdConverter.INSTANCE)
            .dateTimeConverter(EpochToLocalDateTimeConverter.INSTANCE)
            .build();
    
        public static String bsonToJson(Document bson) {
            return bson == null ? null : bson.toJson(JSON_WRITER_SETTINGS);
        }
    
        public static String bsonListToJson(List<Document> bsonList) {
            return bsonList == null || bsonList.isEmpty() ? null :
                "[" + bsonList.stream()
                    .map(Converters::bsonToJson)
                    .collect(Collectors.joining(",")) +
                "]";
        }
    }

Define `dataFinder()`. Use `MongoTemplate` to query MongoDB directly.

    import com.fasterxml.jackson.core.JsonProcessingException;
    import com.fasterxml.jackson.databind.node.ArrayNode;
    import com.octomix.josson.Josson;
    import org.bson.BsonDocument;
    import org.bson.BsonValue;
    import org.bson.Document;
    import org.bson.codecs.BsonArrayCodec;
    import org.bson.codecs.DecoderContext;
    import org.bson.json.JsonReader;
    import org.springframework.beans.factory.annotation.Autowired;
    import org.springframework.data.mongodb.core.MongoTemplate;
    import org.springframework.data.mongodb.core.query.BasicQuery;
    import org.springframework.stereotype.Repository;
    
    import java.util.ArrayList;
    import java.util.List;
    import java.util.function.BiFunction;
    import java.util.stream.Collectors;
    
    @Repository
    public class JsonDao {
    
        @Autowired
        private MongoTemplate mongoTemplate;
    
        public List<Document> findDocuments(String collectionName, String query) {
            return mongoTemplate.find(
                new BasicQuery(query),
                Document.class,
                collectionName
            );
        }
    
        public List<Document> aggregateDocuments(String collectionName, String operations) {
            List<BsonDocument> pipeline = new BsonArrayCodec()
                .decode(new JsonReader(operations), DecoderContext.builder().build())
                .stream()
                .map(BsonValue::asDocument)
                .collect(Collectors.toList());
            return mongoTemplate.getDb().getCollection(collectionName).aggregate(pipeline).into(new ArrayList<>());
        }
    
        public String findJsonString(String collectionName, String query) {
            return bsonToJson(mongoTemplate.findOne(
                new BasicQuery(query),
                Document.class,
                collectionName
            ));
        }
    
        public String aggregateJsonString(String collectionName, String operations) {
            List<Document> documents = aggregateDocuments(collectionName, operations);
            return documents.isEmpty() ? null : bsonToJson(documents.get(0));
        }
    
        public BiFunction<String, String, Josson> dataFinder() {
            return (collectionName, query) -> {
                if (collectionName.endsWith("[]")) {
                    collectionName = collectionName.substring(0, collectionName.length()-2);
                    List<Document> documents = query.charAt(0) == '['
                        ? aggregateDocuments(collectionName, query)
                        : findDocuments(collectionName, query);
                    if (!documents.isEmpty()) {
                        ArrayNode array = Josson.createArrayNode();
                        documents.stream()
                            .map(Converters::bsonToJson)
                            .forEach(json -> {
                                try {
                                    array.add(Josson.readJsonNode(json));
                                } catch (JsonProcessingException e) {
                                    e.printStackTrace();
                                }
                            });
                        return Josson.create(array);
                    }
                } else {
                    String json = query.charAt(0) == '['
                        ? aggregateJsonString(collectionName, query)
                        : findJsonString(collectionName, query);
                    if (json != null) {
                        try {
                            return Josson.fromJsonString(json);
                        } catch (JsonProcessingException e) {
                            e.printStackTrace();
                        }
                    }
                }
                return null;
            };
        }
    }

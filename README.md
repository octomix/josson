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
        <version>1.5.0</version>
    </dependency>

### Gradle

    implementation group: 'com.octomix.josson', name: 'josson', version: '1.5.0'

## Features and Capabilities

### Josson

- Query a JSON dataset.
- There are 246 internal functions to manipulate and format data.
- Restructure JSON data and capable of grouping and unwind data.
- Multithreading capability for array elements manipulation.
- Support custom function to manipulate node.
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
  - [Join, Set and Compare Operators](#join-set-and-compare-operators)

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
  - [Programmable Functions](#programmable-functions)

- [Custom Function](#custom-function)

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
  - [Examples from Stack Overflow Question](#examples-from-stack-overflow-question)

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
| !=~      | Not match regular expression            |
| !        | Logical NOT                             |
| &amp;    | Logical AND                             |
| &#124;   | Logical OR                              |

### Join, Set and Compare Operators

| Operator      | Description                      |
|:--------------|:---------------------------------|
| &gt;=&lt;     | Inner join                       |
| &lt;=&lt;     | Left join one                    |
| &gt;=&gt;     | Right join one                   |
| &lt;=&lt;&lt; | Left join many                   |
| &gt;&gt;=&gt; | Right join many                  |
| &lt;!&lt;     | Left excluding join              |
| &gt;!&gt;     | Right excluding join             |
| &lt;!&gt;     | Outer excluding join             |
| &lt;+&lt;     | Left concatenate                 |
| &gt;+&gt;     | Right concatenate                |
| &lt;-&lt;     | Subtract right from left         |
| &gt;-&gt;     | Subtract left from right         |
| &lt;-&gt;     | Symmetric difference             |
| &lt;u&gt;     | Union                            |
| &gt;n&lt;     | Intersection                     |
| &#124;        | Chaining pipe                    |
| &lt;==&gt;    | Equals (returns BooleanNode)     |
| &lt;!=&gt;    | Not equals (returns BooleanNode) |

## Josson Basic

Initial setup for date time formatting and JSON serialization.

    Josson.setLocale(Locale.ENGLISH); // default Locale.getDefault()

    Josson.setZoneId(ZoneId.of("Asia/Hong_Kong")); // default ZoneId.systemDefault()

    Josson.setSerializationInclusion(JsonInclude.Include.NON_NULL);

Josson support multithreading for array elements manipulation.
When thread pool size is 2 or above, a little more system resource is required to retain the array order.
Turn it off if the array elements order of the result is negligible.

    Josson.setMinArraySizeToUseMultiThread(200); // default size is 100

    Josson.setThreadPoolSize(2); // default size is 4

    Josson.setRetainArrayOrder(false); // default ture

To create a Josson object from a Jackson JsonNode.

    Josson josson = Josson.create(jsonNode);

To create a Josson object from a Java object.

    Josson josson = Josson.from(object);

To create a Josson object from a JSON string.

    Josson josson = Josson.fromJsonString("...");

To apply a Josson query path and get the result JsonNode.

    JsonNode node = josson.getNode(expression);

Define initial variables for a query. Variable name must start with "$".

    Map<String, JsonNode> vars = new HashMap<>();
    vars.put("$a", IntNode.valueOf(3));
    JsonNode node = josson.getNode("concat('qty=',$a)", vars);

To apply a Josson query path and get the path trace along the main branch.
The trace object contains all progressive nodes and variables defined along the main branch.

    PathTrace trace = josson.getPathTrace(expression);

## Josson Query Language

### Josson Path

A _Josson Path_ is constructed with _Path Steps_ connected by `.`.

A path step can...

- Return an element of an object node by key name.
- Filter an array node, return the first matching element or all matching elements.
- Perform a transformation operation by a [Josson Function](#josson-functions).

| Step Syntax      | Description                                                               |
|:-----------------|:--------------------------------------------------------------------------|
| `key`            | A child element key name                                                  |
| `[number]`       | An array element by zero-based index                                      |
| `[expression]`   | A boolean filter expression to find the first matching array element      |
| `[expression]*`  | A boolean filter expression to query all matching array elements          |
| `[expression]@`  | Filter all matching elements and divert each to separate branches         |
| `[]@`            | Divert each element of the current array node to separate branches        |
| `array@`         | Divert each array element to separate branches                            |
| `function()`     | A Josson function                                                         |
| `@function()`    | Merge all branch results into a single array before manipulation          |
| `function()@`    | Divert the function output array elements to separate branches            |
| `*`              | Wildcard search that return the first resolvable non-null result          |
| `**`             | Wildcard search that return all object elements                           |
| `*@`             | Wildcard search and divert each object element to separate branches       |
| `*(expression)`  | Multi-level wildcard search and return the first resolvable element       |
| `*[expression]`  | Wildcard search with filter and return the first matching element         |
| `*[expression]*` | Wildcard search with filter and return all matching elements              |
| `*[expression]@` | Wildcard search with filter and divert each element to separate branches  |
| `~'regex'`       | Search by regular expression and return the first matching element        |
| `~'regex'*`      | Search by regular expression and return all matching elements             |
| `~'regex'@`      | Search by regular expression and divert each element to separate branches |

To specify an array and then apply an index or a filter can be simplified by removing the `.` between them.
The following two statements produce the same final result. But have different number of path steps.
The first one contains two path steps, select an array and then filter.
The second one contains one path step only, filter an array directly.

    array.[expression]*

    array[expression]*

Filter can also apply to an object node.
If the expression is evaluated to `true`, the object itself is returned.
Otherwise, return `null`.

_Example_

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

    a.b.c[isNumber()]* ==> [ 1, 2 ]

    a.x[y='z'] ==> { "y": "z" }

    a.x[y='1'] ==> null

    a.*.y ==> "z"

Enclose the key name with double quote if it contains "." or starts/ends with spaces. 

_Example_

    {
        "a.b": {
            " c.d ": 123
        }
    }

    "a.b"." c.d " ==> 123

Wildcard search and regular expression search path steps work on object node.
Multi-level wildcard searches for the first resolvable result on path steps in order.
No argument or an argument of value `0` means unlimited levels.

_Example_

    // Search for *.y
    *(1).y ==> null

    // Search for *.y and then *.*.y
    *(2).y ==> "z"

    // Unlimited levels
    *(0).y ==> "z"
    *().y ==> "z"

A wildcard search with filter is actually a combination of 3 steps.
The following query is the same as a wildcard search with filter that returns all matching elements:

Josson function `entries()` > Find-all array filter `[expression]*` > Select element `value`

    // *[expression]* is expanded to...
    entries().[expression]*.value

Function `entries()` transform object `{ "name" : <JsonNode>, ... }` into this new structure:

    [
        {
            "key" : "name",
            "value" : <JsonNode>
        },
        :
        :
    ]

Therefore, use keyword `key` in a wildcard filter expression to search.
Even keyword `value` can also be used in wildcard filter expression.

_Example_

    *[key.startsWith('item') & value.isText()]

    *[key.matches('^[A-Z]{10}$')]*

    *[key =~ '^[A-Z]{10}$']*

    entries().[key =~ '^[A-Z]{10}$']*.value

The last 3 examples do the same thing and can be simplified to this syntax:

    ~'^[A-Z]{10}$'*

Additional step symbols are available in filter expression and function argument.

| Step      | Operation | Description                                                           |
|:----------|:----------|:----------------------------------------------------------------------|
| `$`       | N/A       | Restart from the root node                                            |
| `..`      | N/A       | Node of the previous step (each additional dot go back one more step) |
| `...`     | N/A       | Node of the previous step's previous step                             |
| `$letVar` | N/A       | Variable defined by function `let()`                                  |
| `?`       | Aggregate | Current array node                                                    |
| `?`       | Scalar    | Current non-array node or current array node's each element           |
| `@`       | Scalar    | Current array node                                                    |
| `#`       | Scalar    | Zero-based index of an array element                                  |
| `##`      | Scalar    | One-based index of an array element                                   |
| `#A`      | Scalar    | Uppercase alphabetic index of an array element                        |
| `#a`      | Scalar    | Lowercase alphabetic index of an array element                        |
| `#R`      | Scalar    | Uppercase roman numerals index of an array element                    |
| `#r`      | Scalar    | Lowercase roman numerals index of an array element                    |

_Exception_

Function `let()` and option setting functions are not counted as a path step.

### Path Chart Elements

Josson path chart shows data type changes and data flow along the path.
Data filtering, transformation and formatting details are not included.

| Element                                    | Description                                                                              |
|--------------------------------------------|------------------------------------------------------------------------------------------|
| `→`                                        | A progress step                                                                          |
| `⇒`                                        | An end result                                                                            |
| `""`                                       | A text node                                                                              |
| `$I`                                       | An integer node                                                                          |
| `$D`                                       | A double node                                                                            |
| `$TF`                                      | A boolean node                                                                           |
| `{}`                                       | An object node                                                                           |
| `{=}`                                      | An object validator                                                                      |
| `[]`                                       | An array node                                                                            |
| `[]@`                                      | Divert each array element to separate branches                                           |
| `[#]`                                      | An indexed array element                                                                 |
| `[=]`                                      | A find-first filter                                                                      |
| `[=]*`                                     | A find-all filter                                                                        |
| `[=]@`                                     | A find-all filter and divert each element to separate branches                           |
| &nbsp;&nbsp;`/`<br>`@`<br>&nbsp;&nbsp;`\ ` | Divert to separate branches                                                              |
| `\ `<br>&nbsp;`@ → []`<br>`/`              | Merge branches into an array                                                             |
| `func()`                                   | A Josson function                                                                        |
| `(%)`                                      | Function argument, the current object's child node                                       |
| `(?)`                                      | Scalar function argument, the current non-array node itself or array node's each element |
| `(?[])`                                    | Aggregate function argument, the current array node                                      |
| `(@)`                                      | Scalar function argument, the current array node                                         |
| `!!`                                       | The position where the step is unresolvable                                              |

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

        {} → items[] ⇒ [{}]

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

        {} → items[] → [{}] → [qty] ⇒ [$I]

18. A function that manipulates each array element and output all results inside an array node.

        josson.getNode("items.concat('Qty=',qty)")
        ==>
        [ "Qty=2", "Qty=1", "Qty=1" ]

    _Path chart_

        {} → items[] → [{}] → [concat(%) ⇒ ""] ⇒ [""]

19. If a step is working on an object or value node, `?` represents that node.

        josson.getNode("items.qty.concat('Qty=',?)")
        ==>
        [ "Qty=2", "Qty=1", "Qty=1" ]

    _Path chart_

        {} → items[] → [{}] → [qty] → [concat(?) ⇒ ""] ⇒ [""]

20. An aggregate function manipulates an array node and produce a value node.

        josson.getNode("items.qty.sum()")
        ==>
        4.0

    _Path chart_

        {} → items[] → [{}] → [qty] → sum(?[]) ⇒ $D

21. Uses Java standard formatting pattern.

        josson.getNode("items.sum(qty).formatNumber('#,##0')")
        ==>
        "4"

    _Path chart_

        {} → items[] → [{}] → sum(?[%]) → $D → formatNumber(?) ⇒ ""

22. Find the first matching element by array filter.

        josson.getNode("items.itemCode[!startsWith('A')]")
        ==>
        "B00001"

    _Path chart_

        {} → items[] → [{}] → [itemCode][=] ⇒ ""

23. Filter using relational operators `=`, `!=`, `>`, `>=`, `<` and `<=`.

        josson.getNode("items[unitDiscount > 0].name")
        ==>
        "OctoPlus Tennis Racket - Star"

    _Path chart_

        {} → items[=] → {} → name ⇒ ""

24. Returns null value if nothing matches the array filter.

        josson.getNode("items[unitDiscount > 100].name")
        ==>
        !unresolvable!

    _Path chart_

        {} → items[=]!! → {} → name ⇒ !unresolvable!

25. To query all matching elements, add a modifier `*` after the array filter.

        josson.getNode("items[unitDiscount > 0]*.name")
        ==>
        [ "OctoPlus Tennis Racket - Star", "WinWin Sport Shoe - Super" ]

    _Path chart_

        {} → items[=]* → [{}] → [name] ⇒ [""]

26. If a step is working on an array node, `#` denotes the zero-based index of an array element.

        josson.getNode("items[#.isEven()]*.itemCode")
        ==>
        [ "B00001", "A00201" ]

    _Path chart_

        {} → items[=]* → [{}] → [itemCode] ⇒ [""]

27. A succession of two path steps that produced a nested array will be flattened automatically.

        josson.getNode("items[true]*.tags[true]*")
        ==>
        [ "SHIRT", "WOMEN", "TENNIS", "SPORT", "RACKET", "SHOE", "SPORT", "WOMEN" ]

    _Path chart_

        {} → items[=]* → [{}] → [tags[=]* ⇒ [""]] ⇒ [""]

28. Path step `array.` is the same as `array[true]*.`.

        josson.getNode("items.tags")
        ==>
        [ "SHIRT", "WOMEN", "TENNIS", "SPORT", "RACKET", "SHOE", "SPORT", "WOMEN" ]

    _Path chart_

        {} → items[] → [{}] → [tags[] ⇒ [""]] ⇒ [""]

29. To simulate cancellation of the automatic flatten mechanism, add a divert-branch modifier "@" to the end of the first array name.

        josson.getNode("items@.tags")
        ==>
        [ [ "SHIRT", "WOMEN" ], [ "TENNIS", "SPORT", "RACKET" ], [ "SHOE", "SPORT", "WOMEN" ] ]

    _Path chart_

                           {} → tags[] → [""]
                          /                  \
        {} → items[] → []@                    @ ⇒ [[""]]
                          \                  /
                           {} → tags[] → [""]

30. Modifier `@` after a path step separator `.` merges all branch results into a single array before manipulation.

        josson.getNode("items@.@tags")
        ==>
        [ "SHIRT", "WOMEN", "TENNIS", "SPORT", "RACKET", "SHOE", "SPORT", "WOMEN" ]

    _Path chart_

                           {}
                          /  \
        {} → items[] → []@    @ ⇒ [{}] → [tags[] ⇒ [""]] ⇒ [""]
                          \  /
                           {}

31. If a step is working on an array node, `?` represents an array element.  
    `=~` matches a regular expression.

        josson.getNode("items.tags[? =~ '^S.*O.+']*")
        ==>
        [ "SPORT", "SHOE", "SPORT" ]

    _Path chart_

        {} → items[] → [{}] → [tags[=]* ⇒ [""]] ⇒ [""]

32. The matching criteria supports logical operators and parentheses.

    >not `!`  
     and `&`  
     or `|`

        josson.getNode("items[(unitDiscount=null | unitDiscount=0) & !(qty<=1)]*.name")
        ==>
        [ "WinWin TShirt Series A - 2022" ]

    _Path chart_

        {} → items[=]* → [{}] → [name] ⇒ [""]

33. Example of a find-all filter operation with flattened array result.

        josson.getNode("items[tags.contains('SPORT')]*.tags")
        ==>
        [ "TENNIS", "SPORT", "RACKET", "SHOE", "SPORT", "WOMEN" ]

    _Path chart_

        {} → items[=]* → [{}] → [tags[] ⇒ [""]] ⇒ [""]

34. An array filter modifier `@` divert each element to separate branch for upcoming manipulation.  
    The final output merges branches into an array.

        josson.getNode("items[tags.containsIgnoreCase('Women')]@.tags")
        ==>
        [ [ "SHIRT", "WOMEN" ], [ "SHOE", "SPORT", "WOMEN" ] ]

    _Path chart_

                       {} → tags[] → [""]
                      /                  \
        {} → items[=]@                    @ ⇒ [[""]]
                      \                  /
                       {} → tags[] → [""]

35. Aggregate functions work on an array node and produce a value node.

        josson.getNode("items.tags.join('+')")
        ==>
        SHIRT+WOMEN+TENNIS+SPORT+RACKET+SHOE+SPORT+WOMEN

    _Path chart_

        {} → items[] → [{}] → [tags[] ⇒ [""]] → [""] → join(?[]) ⇒ ""

36. An array node can apply the modifier `@` that divert each element to separate branch.

        josson.getNode("items@.tags.join('+')")
        ==>
        [ "SHIRT+WOMEN", "TENNIS+SPORT+RACKET", "SHOE+SPORT+WOMEN" ]

    _Path chart_

                           {} → tags[] → [""] → join(?[]) → ""
                          /                                   \
        {} → items[] → []@                                     @ ⇒ [""]
                          \                                   /
                           {} → tags[] → [""] → join(?[]) → ""

37. Syntax `[]@` diverts each element of the current array node.

        josson.getNode("items.join([]@.tags.join('+'),' / ')")
        ==>
        "SHIRT+WOMEN / TENNIS+SPORT+RACKET / SHOE+SPORT+WOMEN"

    _Path chart_

                                        {} → tags[] → [""] → join(?[]) → ""
                                       /                                   \
        {} → items[] → [{}] → join(?[]@                                     @ ⇒ [""]) ⇒ ""
                                       \                                   /
                                        {} → tags[] → [""] → join(?[]) → ""

38. Modifier `@` before a function name merges all branch results into a single array before manipulation.

        josson.getNode("items@.tags.join('+').@join(' / ')")
        ==>
        "SHIRT+WOMEN / TENNIS+SPORT+RACKET / SHOE+SPORT+WOMEN"

    _Path chart_

                           {} → tags[] → [""] → join(?[]) → ""
                          /                                   \
        {} → items[] → []@                                     @ → [""] → join(?[]) ⇒ ""
                          \                                   /
                           {} → tags[] → [""] → join(?[]) → ""

39. Modifier `@` after a function diverts the function output array elements to separate branches.  
    It has the same effect of a path step `.[]@` after a function.

        josson.getNode("'1+2 | 3+4 | 5+6'.split('|')@.split('+').calc(?*2).round(0).join('+').concat('(',?,')/2').@join(' | ')")
        ==>
        "(2+4)/2 | (6+8)/2 | (10+12)/2"

    _Path chart_

                              "" → split(?) → [""] → [calc(?) ⇒ $D] → [round(?) ⇒ $I] → join(?[]) → "" → concat(?) → ""
                             /                                                                                         \
        "" → split(?) → [""]@                                                                                           @ → [""] → join(?[]) ⇒ ""
                             \                                                                                         /
                              "" → split(?) → [""] → [calc(?) ⇒ $D] → [round(?) ⇒ $I] → join(?[]) → "" → concat(?) → ""

40. All function parameters can refer to a child node of the step.

        josson.getNode("items@.repeat(concat('[',brand,'] ',name,'\n'), qty).@join()")
        ==>
        "[WinWin] WinWin TShirt Series A - 2022\n" +
        "[WinWin] WinWin TShirt Series A - 2022\n" +
        "[OctoPlus] OctoPlus Tennis Racket - Star\n" +
        "[WinWin] WinWin Sport Shoe - Super\n"

    _Path chart_

                           {} → repeat(%) → ""
                          /                   \
        {} → items[] → []@                     @ → [""] → join(?[]) ⇒ ""
                          \                   /
                           {} → repeat(%) → ""

41. Scalar functions work on array and produce an array, such as `concat()`, manipulate on each element.

        josson.getNode("items.concat('Item ',#,': [',itemCode,'] ',qty,unit,' x ',name,' <',property.colors.join(','),'>').join('\n')")
        ==>
        "Item 0: [B00001] 2Pcs x WinWin TShirt Series A - 2022 <WHITE,RED>\n" +
        "Item 1: [A00308] 1Pcs x OctoPlus Tennis Racket - Star <BLACK>\n" +
        "Item 2: [A00201] 1Pair x WinWin Sport Shoe - Super <RED>"

    _Path chart_

        {} → items[] → [{}] → [concat(%) ⇒ ""] → join(?[]) ⇒ ""

42. If a step is working on an array node, `@` represents that array node.  
    `##` denotes the one-based index of an array element.

        josson.getNode("items.sort(itemCode).concat('Item ',##,'/',@.size(),': [',itemCode,'] ',qty,unit,' x ',name,' <',property.colors.join(','),'>').join('\n')")
        ==>
        "Item 1/3: [A00201] 1Pair x WinWin Sport Shoe - Super <RED>\n" +
        "Item 2/3: [A00308] 1Pcs x OctoPlus Tennis Racket - Star <BLACK>\n" +
        "Item 3/3: [B00001] 2Pcs x WinWin TShirt Series A - 2022 <WHITE,RED>"

    _Path chart_

        {} → items[] → [{}] -> sort(%) → [{}] → [concat(@, %) ⇒ ""] → join(?[]) ⇒ ""

43. An object node with a validation filter.

        josson.getNode("customer[name='Peggy']")
        ==>
        {
          "customerId" : "CU0001",
          "name" : "Peggy",
          "phone" : "+852 62000610"
        }

    _Path chart_

        {} → customer{=} ⇒ {}

44. An object node that cannot meet the validation filter criteria returns null.

        josson.getNode("customer[name='Raymond']")
        ==>
        !unresolvable!

    _Path chart_

        {} → customer{=}!! ⇒ !unresolvable!

45. In filter expression and function argument, a path starts with symbol "$" restart from the root node.

        josson.getNode("items.concat($.customer.customerId, '-', itemCode)")
        ==>
        [ "CU0001-B00001", "CU0001-A00308", "CU0001-A00201" ]

    _Path chart_

         .------------- → --------------.
         |                              |
        {} → items[] → [{}] → [concat(%,$) ⇒ ""] ⇒ [""]

46. In filter expression and function argument, a path starts with symbol ".." go back to the previous step's node.
    Each additional dot go back one more step.

        josson.getNode("items.property.concat(...customer.name, ' items=', ..size(), ' colors=', colors.join(','))")
        ==>
        [ "Peggy items=3 colors=WHITE,RED", "Peggy items=3 colors=BLACK", "Peggy items=3 colors=RED" ]

    _Path chart_

         .---------------------------- → ---------------.
         |               .------------ → ------------.  |
         |               |                           |  |
        {} → items[] → [{}] → [property] → [concat(%,..,...) ⇒ ""] ⇒ [""]

47. One more example.

        josson.getNode("items@.property.concat(....customer.name, ' ', ..itemCode, ' colors=', colors.join(','))")
        ==>
        [ "Peggy items=3 colors=WHITE,RED", "Peggy items=3 colors=BLACK", "Peggy items=3 colors=RED" ]

    _Path chart_

         .------------------------------ → --------------.
         |                  .----------- → -----------.  |
         |                  |                         |  |
         |                 {} → property{} → concat(%,..,....) → ""
         |                /                                        \
        {} → items[] → []@                                          @ ⇒ [""]
                          \                                        /
                           {} → property{} → concat(%,..,....) → ""

48. Function `json()` parse a JSON string.

        josson.getNode("json('[1,2,\"3\"]')")
        ==>
        [ 1, 2, "3" ]

    _Path chart_

        json("") ⇒ []

49. Relational operator `=` and `!=` support object comparison.

        josson.getNode("[customer = json('{\"name\":\"Peggy\",\"phone\":\"+852 62000610\",\"customerId\":\"CU0001\"}')].isNotNull()")
        ==>
        true

    _Path chart_

        {} → {=} → {} → isNotNull(?) ⇒ $TF

50. Relational operator `=` and `!=` support root level array values comparison where the position ordering is allowed to be different.

        josson.getNode("[items[0].property.colors = json('[\"RED\",\"WHITE\"]')].isNotNull()")
        ==>
        true

    _Path chart_

        {} → {=} → {} → isNotNull(?) ⇒ $TF

51. Function `calc()` uses MathParser.org-mXparser library <http://mathparser.org/> to perform calculation.

        josson.getNode("items.calc(qty * (unitPrice-unitDiscount)).concat(##,'=',?)")
        ==>
        [ null, "2=140.0", "3=100.0" ]

    _Path chart_

        {} → items[] → [{}] → [calc(%) ⇒ $D] → [concat(?) ⇒ ""] ⇒ [""]

52. Scalar functions preserve null element.

        josson.getNode("items.calc(qty * (unitPrice-unitDiscount)).[##<=2]*.concat(##,'=',?)")
        ==>
        [ null, "2=140.0" ]

    _Path chart_

        {} → items[] → [{}] → [calc(%) ⇒ $D] → [=]* → [concat(?) ⇒ ""] ⇒ [""]

53. An array-to-value transformation function throws away null nodes automatically.

        josson.getNode("items.calc(qty * (unitPrice-unitDiscount)).concat(##,'=',?).join(' / ')")
        ==>
        "2=140.0 / 3=100.0"

    _Path chart_

        {} → items[] → [{}] → [calc(%) ⇒ $D] → [concat(?) ⇒ ""] → join(?[]) ⇒ ""

54. Array filter can filter out null nodes.

        josson.getNode("items.calc(qty * (unitPrice-unitDiscount)).[isNotNull()]*.concat(##,'=',?)")
        ==>
        [ "1=140.0", "2=100.0" ]

    _Path chart_

        {} → items[] → [{}] → [calc(%) ⇒ $D] → [=]* → [concat(?) ⇒ ""] ⇒ [""]

55. An argument `#A` denotes the uppercase alphabetic array index.

        josson.getNode("items.calc(qty * (unitPrice-unitDiscount)).[?!=null]*.concat(#A,'=',?).join(' / ')")
        ==>
        "A=140.0 / B=100.0"

    _Path chart_

        {} → items[] → [{}] → [calc(%) ⇒ $D] → [=]* → [concat(?) ⇒ ""] → join(?[]) ⇒ ""

56. Merge Diverted branches throws away null nodes automatically.
    An argument `#a` denotes the lowercase alphabetic array index.

        josson.getNode("items@.calc(qty * (unitPrice-unitDiscount)).@concat(#a,'=',?)")
        ==>
        [ "a=140.0", "b=100.0" ]

    _Path chart_

                           {} → calc(%) → $D
                          /                 \
        {} → items[] → []@                   @ → [$D] → [concat(?) ⇒ ""] ⇒ [""]
                          \                 /
                           {} → calc(%) → $D

57. mXparser expression accepts single-step path only.
    To apply multi-steps path, function or filter, append arguments with syntax `newVariable:path`.

        josson.getNode("items.calc(qty * (unitPrice-x), x:coalesce(unitDiscount,0)).formatNumber('US$#,##0.00')")
        ==>
        [ "US$30.00", "US$140.00", "US$100.00" ]

    _Path chart_

        {} → items[] → [{}] → [calc(%) ⇒ $D] → [formatNumber(?) ⇒ ""] ⇒ [""]

58. An argument `#r` and `#R` denotes the lowercase and uppercase roman numerals array index.

        josson.getNode("items.unitPrice.calc(? * 2).concat(#r,'=',?)")
        ==>
        [ "i=30.0", "ii=300.0", "iii=220.0" ]

    _Path chart_

        {} → items[] → [{}] → [unitPrice ⇒ $D] → [calc(?) ⇒ $D] → [concat(?) ⇒ ""] ⇒ [""]

59. Function `entries()` returns an array of an object's string-keyed property `[{key, value}]` pairs.

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

60. Function `keys()` lists an object's key names.

        josson.getNode("keys()")
        ==>
        [ "salesOrderId", "salesDate", "salesPerson", "customer", "items", "totalAmount" ]

    _Path chart_

        {} → keys(?) ⇒ [""]

61. `keys()` can retrieve nested child object keys for a given levels.

        josson.getNode("keys(2)")
        ==>
        [ "salesOrderId", "salesDate", "salesPerson", "customer", "customerId", "name", "phone", "items", "totalAmount" ]

    _Path chart_

        {} → keys(?) ⇒ [""]

62. Function `collect()` puts all argument values into an array.
    Function `wrap()` is equivalent to `collect(?)` which is wrap the node inside an array.

        josson.getNode("collect(salesDate, customer, items.itemCode)")
        ==>
        [ "2022-01-01T10:01:23", {
          "customerId" : "CU0001",
          "name" : "Peggy",
          "phone" : "+852 62000610"
        }, [ "B00001", "A00308", "A00201" ] ]

    _Path chart_

        {} → collect(%) ⇒ []

63. Function `cumulateCollect()` require 2 arguments.
    The 1st parameter is a query to evaluate a result that will be collected into an array.
    The 2nd parameter is a query to evaluate the next dataset that loop back for the 1st parameter evaluation again.
    The operation loop will be stopped when the next dataset is null.

        josson.getNode("json('{\"id\":1,\"val\":11,\"item\":{\"id\":2,\"val\":22,\"item\":{\"id\":3,\"val\":33,\"item\":{\"id\":4,\"val\":44}}}}')" +
                       ".cumulateCollect(map(id,val.calc(?*2)), item)")
        ==>
        [ {
          "id" : 1,
          "val" : 22.0
        }, {
          "id" : 2,
          "val" : 44.0
        }, {
          "id" : 3,
          "val" : 66.0
        }, {
          "id" : 4,
          "val" : 88.0
        } ]

    _Path chart_

        {} → cumulateCollect(%) ⇒ [{}]

64. Function `toArray()` puts an object's values into an array.

        josson.getNode("customer.toArray()")
        ==>
        [ "CU0001", "Peggy", "+852 62000610" ]

    _Path chart_

        {} → customer{} → toArray(?) ⇒ [""]

65. Furthermore, function `toArray()` puts all arguments (values, object's values, array elements) into a single array.

        josson.getNode("toArray('Hello',customer,items.itemCode.sort())")
        ==>
        [ "Hello", "CU0001", "Peggy", "+852 62000610", "A00201", "A00308", "B00001" ]

    _Path chart_

        {} → toArray(%) ⇒ [""]

66. Function `map()` constructs a new object node.
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

67. Function `field()` adds, removes and renames field on the current object node.
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

68. Functions `map()` and `field()` works on array.

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

        {} → items[] → [{}] → [field(%) ⇒ {}] ⇒ [{}]

69. For functions `map()` and `field()`, parameter syntax `path:+` present an unresolvable path as a NullNode.

        josson.getNode("items.map(itemCode, unitDiscount)")
        ==>
        [ {
          "itemCode" : "B00001",
        }, {
          "itemCode" : "A00308",
          "unitDiscount" : 10.0
        }, {
          "itemCode" : "A00201",
          "unitDiscount" : 10.0
        } ]

        josson.getNode("items.map(itemCode, unitDiscount:+)")
        ==>
        [ {
          "itemCode" : "B00001",
          "unitDiscount" : null
        }, {
          "itemCode" : "A00308",
          "unitDiscount" : 10.0
        }, {
          "itemCode" : "A00201",
          "unitDiscount" : 10.0
        } ]

        josson.getNode("items.map(itemCode, unitDiscount:if([unitDiscount=null],null,unitDiscount))")
        ==>
        [ {
          "itemCode" : "B00001",
          "unitDiscount" : null
        }, {
          "itemCode" : "A00308",
          "unitDiscount" : 10.0
        }, {
          "itemCode" : "A00201",
          "unitDiscount" : 10.0
        } ]

70. For functions `map()` and `field()`, parameter syntax `newFieldName:+path` present an unresolvable path as a NullNode with a new field name.

        josson.getNode("items.map(itemCode, discount:+unitDiscount)")
        ==>
        [ {
          "itemCode" : "B00001",
          "discount" : null
        }, {
          "itemCode" : "A00308",
          "discount" : 10.0
        }, {
          "itemCode" : "A00201",
          "discount" : 10.0
        } ]

71. For functions `map()` and `field()`, parameter syntax `**:object` extracts all the fields within a given object.

        josson.getNode("items.slice(0,2).field(**:properties, properties:)")
        ==>
        [ {
          "itemCode" : "B00001",
          "name" : "WinWin TShirt Series A - 2022",
          "brand" : "WinWin",
          "qty" : 2,
          "unit" : "Pcs",
          "unitPrice" : 15.0,
          "tags" : [ "SHIRT", "WOMEN" ],
          "size" : "M",
          "colors" : [ "WHITE", "RED" ]
        }, {
          "itemCode" : "A00308",
          "name" : "OctoPlus Tennis Racket - Star",
          "brand" : "OctoPlus",
          "qty" : 1,
          "unit" : "Pcs",
          "unitPrice" : 150.0,
          "unitDiscount" : 10.0,
          "tags" : [ "TENNIS", "SPORT", "RACKET" ],
          "colors" : [ "BLACK" ]
        } ]

72. Function `group()` works like SQL `group by`. It will build a structure of `[{key, [elements]}]`.
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
        {} → items[] → [{}] → group(%) → [{}]@                     @ → [""] → join(?[]) ⇒ ""
                                              \                   /
                                               {} → concat(%) → ""

73. Function `unwind()` works like MongoDB `$unwind` operation. The operation is the reverse of `group()`.
    It supports parameter with prefix `+` in order to add element even the path is unresolvable.

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

        {} → items[] → [{}] → group(%) → [{}] → unwind(%) ⇒ [{}]

74. Function `flatten()` flatten an array for a given number of times.

        josson.getNode("items@.tags")
        ==>
        [ [ "SHIRT", "WOMEN" ], [ "TENNIS", "SPORT", "RACKET" ], [ "SHOE", "SPORT", "WOMEN" ] ]


        flatten(items@.tags, 1, null)
        ==>
        [ "SHIRT", "WOMEN", "TENNIS", "SPORT", "RACKET", "SHOE", "SPORT", "WOMEN" ]

75. Function `flatten()` flatten an array same as the default path step behavior. But more readable.

        josson.getNode("items@.tags.@.[true]*")
        ==>
        [ "SHIRT", "WOMEN", "TENNIS", "SPORT", "RACKET", "SHOE", "SPORT", "WOMEN" ]


        josson.getNode("items@.tags.@.flatten(1)")
        ==>
        [ "SHIRT", "WOMEN", "TENNIS", "SPORT", "RACKET", "SHOE", "SPORT", "WOMEN" ]

    _Path chart_

                           {} → tags[] → [""]
                          /                  \
        {} → items[] → []@                    @ → [[""]] → flatten() ⇒ [""]
                          \                  /
                           {} → tags[] → [""]

76. If the parameter value of `flatten()` is textual, it will act as a key name separator to build a flattened object.
    If the separator is `null`, the standalone end node name will be used instead.
    The second parameter is optional and is the string format for array index, usually use with this combination `('.', '[%d]')`.

        josson.getNode("flatten('_')")
        ==>
        {
          "salesOrderId" : "SO0001",
          "salesDate" : "2022-01-01T10:01:23",
          "salesPerson" : "Raymond",
          "customer_customerId" : "CU0001",
          "customer_name" : "Peggy",
          "customer_phone" : "+852 62000610",
          "items_0_itemCode" : "B00001",
          "items_0_name" : "WinWin TShirt Series A - 2022",
          "items_0_brand" : "WinWin",
          "items_0_property_size" : "M",
          "items_0_property_colors_0" : "WHITE",
          "items_0_property_colors_1" : "RED",
          "items_0_qty" : 2,
          "items_0_unit" : "Pcs",
          "items_0_unitPrice" : 15.0,
          "items_0_tags_0" : "SHIRT",
          "items_0_tags_1" : "WOMEN",
          "items_1_itemCode" : "A00308",
          "items_1_name" : "OctoPlus Tennis Racket - Star",
          "items_1_brand" : "OctoPlus",
          "items_1_property_colors_0" : "BLACK",
          "items_1_qty" : 1,
          "items_1_unit" : "Pcs",
          "items_1_unitPrice" : 150.0,
          "items_1_unitDiscount" : 10.0,
          "items_1_tags_0" : "TENNIS",
          "items_1_tags_1" : "SPORT",
          "items_1_tags_2" : "RACKET",
          "items_2_itemCode" : "A00201",
          "items_2_name" : "WinWin Sport Shoe - Super",
          "items_2_brand" : "WinWin",
          "items_2_property_size" : "35",
          "items_2_property_colors_0" : "RED",
          "items_2_qty" : 1,
          "items_2_unit" : "Pair",
          "items_2_unitPrice" : 110.0,
          "items_2_unitDiscount" : 10.0,
          "items_2_tags_0" : "SHOE",
          "items_2_tags_1" : "SPORT",
          "items_2_tags_2" : "WOMEN",
          "totalAmount" : 270.0
        }

77. Function `unflatten()` reverse the operation of `flatten()`.

        josson.getNode("items[1].flatten('_').unflatten('_')")
        ==>
        {
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
        }

78. Functions `map()`,`field()`,`group()`,`unwind()` - key name support evaluation using syntax `keyQuery::valueQuery`.

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

        {} → items[] → [{}] → [map(%) ⇒ {}] ⇒ [{}]

79. Syntax `keyQuery::+valueQuery` present an unresolvable path as a NullNode.

        josson.getNode("items.map(itemCode::unitDiscount)")
        ==>
        [ { }, {
          "A00308" : 10.0
        }, {
          "A00201" : 10.0
        } ]

        josson.getNode("items.map(itemCode::+unitDiscount)")
        ==>
        [ {
          "B00001" : null
        }, {
          "A00308" : 10.0
        }, {
          "A00201" : 10.0
        } ]

80. Function `mergeObjects()` merge all objects in an array into one object.

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

81. Function `assort()` separates an object's entries according to different path conditions in sequence,
    and put them into the corresponding array if the evaluated result is not null.
    Entries will be removed if no condition can be matched.
    If the last argument is `??`, each of the remaining entry will be added to the end of result array separately.
    If no argument is provided, each entry will be added to the result array separately.

        josson.getNode("json('{\"xy1\": 1,\"xy2\": 2,\"ab1\": 3,\"ab2\": 4,\"ab3\": 5,\"zz1\": 6,\"xy3\": 7,\"zz2\": 9,\"zz3\": {\"k\":10}}}')" +
                       ".assort(*.[isEven()], ~'xy.*', ~'ab.*', *)")
        ==>
        [ {
          "xy2" : 2,
          "ab2" : 4,
          "zz1" : 6
        }, {
          "xy1" : 1,
          "xy3" : 7
        }, {
          "ab1" : 3,
          "ab3" : 5
        }, {
          "zz2" : 9,
          "zz3" : {
            "k" : 10
          }
        } ]

        josson.getNode("json('{\"xy1\": 1,\"xy2\": 2,\"ab1\": 3,\"ab2\": 4,\"ab3\": 5,\"zz1\": 6,\"xy3\": 7,\"zz2\": 9,\"zz3\": {\"k\":10}}')" +
                       ".assort(*.[isEven()], ~'xy.*', ~'ab.*')")
        ==>
        [ {
          "xy2" : 2,
          "ab2" : 4,
          "zz1" : 6
        }, {
          "xy1" : 1,
          "xy3" : 7
        }, {
          "ab1" : 3,
          "ab3" : 5
        } ]

        josson.getNode("json('{\"xy1\": 1,\"xy2\": 2,\"ab1\": 3,\"ab2\": 4,\"ab3\": 5,\"zz1\": 6,\"xy3\": 7,\"zz2\": 9,\"zz3\": {\"k\":10}}}')" +
                       ".assort(*.[isEven()], ~'xy.*', ~'ab.*', ??)")
        ==>
        [ {
          "xy2" : 2,
          "ab2" : 4,
          "zz1" : 6
        }, {
          "xy1" : 1,
          "xy3" : 7
        }, {
          "ab1" : 3,
          "ab3" : 5
        }, {
          "zz2" : 9
        }, {
          "zz3" : {
            "k" : 10
          }
        } ]

        josson.getNode("json('{\"xy1\": 1,\"xy2\": 2,\"ab1\": 3,\"ab2\": 4,\"ab3\": 5,\"zz1\": 6,\"xy3\": 7,\"zz2\": 9,\"zz3\": {\"k\":10}}}')" +
                       ".assort()")
        ==>
        [ {
          "xy1" : 1
        }, {
          "xy2" : 2
        }, {
          "ab1" : 3
        }, {
          "ab2" : 4
        }, {
          "ab3" : 5
        }, {
          "zz1" : 6
        }, {
          "xy3" : 7
        }, {
          "zz2" : 9
        }, {
          "zz3" : {
            "k" : 10
          }
        } ]

82. Function `assort()` also works for array. The result is an array of arrays.

        josson.getNode("json('[1,2,3,4,5,6,7,8,9,10,11,12]').assort([?<5], [isEven()], [?<9], ?)")
        ==>
        [ [ 1, 2, 3, 4 ], [ 6, 8, 10, 12 ], [ 5, 7 ], [ 9, 11 ] ]

        josson.getNode("json('[1,2,3,4,5,6,7,8,9,10,11,12]').assort([?<5], [isEven()], [?<9], ??)")
        ==>
        [ [ 1, 2, 3, 4 ], [ 6, 8, 10, 12 ], [ 5, 7 ], [ 9 ], [ 11 ] ]

83. Function `eval()` evaluates the value of a text node as a query statement.

        josson.getNode("json('{\"a\":1,\"b\":2,\"statement\":\"calc(a+b*2)\"}').eval(statement)")
        ==>
        5.0

        josson.getNode("json('[{\"a\":3,\"s\":\"calc(a*2)\"},{\"a\":4,\"s\":\"calc(a*2)\"}]')@.eval(s)")
        ==>
        [ 6.0, 8.0 ]

## Josson Functions

There are 246 functions. They are classified into 8 categories:

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
15. [default()](#15-default)
16. [keepAfter()](#16-keepafter)
17. [keepAfterIgnoreCase()](#17-keepafterignorecase)
18. [keepAfterLast()](#18-keepafterlast)
19. [keepAfterLastIgnoreCase()](#19-keepafterlastignorecase)
20. [keepBefore()](#20-keepbefore)
21. [keepBeforeIgnoreCase()](#21-keepbeforeignorecase)
22. [keepBeforeLast()](#22-keepbeforelast)
23. [keepBeforeLastIgnoreCase()](#23-keepbeforelastignorecase)
24. [leftPad()](#24-leftpad)
25. [length()](#25-length)
26. [lowerCase()](#26-lowercase)
27. [notEmpty()](#27-notempty)
28. [notBlank()](#28-notblank)
29. [prepend()](#29-prepend)
30. [prependIfMissing()](#30-prependifmissing)
31. [prependIfMissingIgnoreCase()](#31-prependifmissingignorecase)
32. [removeEnd()](#32-removeend)
33. [removeEndIgnoreCase()](#33-removeendignorecase)
34. [removeStart()](#34-removestart)
35. [removeStartIgnoreCase()](#35-removestartignorecase)
36. [repeat()](#36-repeat)
37. [replace()](#37-replace)
38. [replaceIgnoreCase()](#38-replaceignorecase)
39. [rightPad()](#39-rightpad)
40. [split()](#40-split)
41. [splitMax()](#41-splitmax)
42. [separate()](#42-separate)
43. [separateMax()](#43-separatemax)
44. [strip()](#44-strip)
45. [stripEnd()](#45-stripend)
46. [stripStart()](#46-stripstart)
47. [substr()](#47-substr)
48. [trim()](#48-trim)
49. [uncapitalize()](#49-uncapitalize)
50. [upperCase()](#50-uppercase)
51. [camelCase()](#51-camelcase)
52. [upperCamelCase()](#52-uppercamelcase)
53. [snakeCase()](#53-snakecase)
54. [lowerSnakeCase()](#54-lowersnakecase)
55. [upperSnakeCase()](#55-uppersnakecase)
56. [camelSnakeCase()](#56-camelsnakecase)
57. [singleQuote() / quote() / q()](#57-singlequote--quote--q)
58. [doubleQuote() / qq()](#58-doublequote--qq)

Date Functions

59. [amPmOfDay()](#59-ampmofday)
60. [second()](#60-second)
61. [secondOfDay()](#61-secondofday)
62. [minute()](#62-minute)
63. [minuteOfDay()](#63-minuteofday)
64. [hourOfAmPm()](#64-hourofampm)
65. [hour()](#65-hour)
66. [dayOfWeek()](#66-dayofweek)
67. [day()](#67-day)
68. [dayOfYear()](#68-dayofyear)
69. [month()](#69-month)
70. [year()](#70-year)
71. [plusSeconds()](#71-plusseconds)
72. [plusMinutes()](#72-plusminutes)
73. [plusHours()](#73-plushours)
74. [plusDays()](#74-plusdays)
75. [plusWeeks()](#75-plusweeks)
76. [plusMonths()](#76-plusmonths)
77. [plusYears()](#77-plusyears)
78. [minusSeconds()](#78-minusseconds)
79. [minusMinutes()](#79-minusminutes)
80. [minusHours()](#80-minushours)
81. [minusDays()](#81-minusdays)
82. [minusWeeks()](#82-minusweeks)
83. [minusMonths()](#83-minusmonths)
84. [minusYears()](#84-minusyears)
85. [truncateToMicro()](#85-truncatetomicro)
86. [truncateToMilli()](#86-truncatetomilli)
87. [truncateToSecond()](#87-truncatetosecond)
88. [truncateToMinute()](#88-truncatetominute)
89. [truncateToHour()](#89-truncatetohour)
90. [truncateToDay()](#90-truncatetoday)
91. [truncateToMonth()](#91-truncatetomonth)
92. [truncateToYear()](#92-truncatetoyear)
93. [withNano()](#93-withnano)
94. [withMicro()](#94-withmicro)
95. [withMilli()](#95-withmilli)
96. [withSecond()](#96-withsecond)
97. [withMinute()](#97-withminute)
98. [withHour()](#98-withhour)
99. [withDay()](#99-withday)
100. [withDayOfYear()](#100-withdayofyear)
101. [withMonth()](#101-withmonth)
102. [withYear()](#102-withyear)
103. [dayEnd()](#103-dayend)
104. [monthEnd()](#104-monthend)
105. [yearEnd()](#105-yearend)
106. [lengthOfMonth()](#106-lengthofmonth)
107. [lengthOfYear()](#107-lengthofyear)
108. [untilInSecond()](#108-untilinsecond)
109. [untilInMinute()](#109-untilinminute)
110. [untilInHour()](#110-untilinhour)
111. [untilInDay()](#111-untilinday)
112. [untilInMonth()](#112-untilinmonth)
113. [untilInYear()](#113-untilinyear)
114. [localToOffsetDate()](#114-localtooffsetdate)
115. [offsetToLocalDate()](#115-offsettolocaldate)
116. [localDateToMillis()](#116-localdatetomillis)
117. [millisToLocalDate()](#117-millistolocaldate)
118. [localDateToSeconds()](#118-localdatetoseconds)
119. [secondsToLocalDate()](#119-secondstolocaldate)
120. [offsetDateToMillis()](#120-offsetdatetomillis)
121. [millisToOffsetDate()](#121-millistooffsetdate)
122. [offsetDateToSeconds()](#122-offsetdatetoseconds)
123. [secondsToOffsetDate()](#123-secondstooffsetdate)
124. [now()](#124-now)

Format Functions

125. [b64Encode()](#125-b64encode)
126. [b64EncodeNoPadding()](#126-b64encodenopadding)
127. [b64MimeEncode()](#127-b64mimeencode)
128. [b64MimeEncodeNoPadding()](#128-b64mimeencodenopadding)
129. [b64UrlEncode()](#129-b64urlencode)
130. [b64UrlEncodeNoPadding()](#130-b64urlencodenopadding)
131. [b64Decode()](#131-b64decode)
132. [b64MimeDecode()](#132-b64mimedecode)
133. [b64UrlDecode()](#133-b64urldecode)
134. [urlEncode()](#134-urlencode)
135. [urlDecode()](#135-urldecode)
136. [escapeHtml()](#136-escapehtml)
137. [unescapeHtml()](#137-unescapehtml)
138. [escapeXml()](#138-escapexml)
139. [unescapeXml()](#139-unescapexml)
140. [formatDate()](#140-formatdate)
141. [formatNumber()](#141-formatnumber)
142. [formatText()](#142-formattext)
143. [formatTexts()](#143-formattexts)
144. [toNumber()](#144-tonumber)
145. [toString()](#145-tostring)
146. [toText()](#146-totext)
147. [csv()](#147-csv)
148. [csvShowNull()](#148-csvshownull)
149. [csvParams()](#149-csvparams)

Logical Functions

150. [contains()](#150-contains)
151. [containsIgnoreCase()](#151-containsignorecase)
152. [notContains()](#152-notcontains)
153. [notContainsIgnoreCase()](#153-notcontainsignorecase)
154. [startsWith()](#154-startswith)
155. [startsWithIgnoreCase()](#155-startswithignorecase)
156. [notStartsWith()](#156-notstartswith)
157. [notStartsWithIgnoreCase()](#157-notstartswithignorecase)
158. [endsWith()](#158-endswith)
159. [endsWithIgnoreCase()](#159-endswithignorecase)
160. [notEndsWith()](#160-notendswith)
161. [notEndsWithIgnoreCase()](#161-notendswithignorecase)
162. [equals()](#162-equals)
163. [equalsIgnoreCase()](#163-equalsignorecase)
164. [notEquals()](#164-notequals)
165. [notEqualsIgnoreCase()](#165-notequalsignorecase)
166. [matches()](#166-matches)
167. [notMatches()](#167-notmatches)
168. [in()](#168-in)
169. [inIgnoreCase()](#169-inignorecase)
170. [notIn()](#170-notin)
171. [notInIgnoreCase()](#171-notinignorecase)
172. [isEmpty()](#172-isempty)
173. [isNotEmpty()](#173-isnotempty)
174. [isBlank()](#174-isblank)
175. [isNotBlank()](#175-isnotblank)
176. [isNull()](#176-isnull)
177. [isNotNull()](#177-isnotnull)
178. [isText()](#178-istext)
179. [isBoolean()](#179-isboolean)
180. [isNumber()](#180-isnumber)
181. [isEven()](#181-iseven)
182. [isOdd()](#182-isodd)
183. [isArray()](#183-isarray)
184. [isObject()](#184-isobject)
185. [isEmptyArray()](#185-isemptyarray)
186. [isEmptyObject](#186-isemptyobject)
187. [not()](#187-not)
188. [isWeekday()](#188-isweekday)
189. [isWeekend()](#189-isweekend)
190. [isLeapYear()](#190-isleapyear)

Array Functions

191. [size()](#191-size)
192. [lastIndex()](#192-lastindex)
193. [indexOf()](#193-indexof)
194. [lastIndexOf()](#194-lastindexof)
195. [first()](#195-first)
196. [last()](#196-last)
197. [max()](#197-max)
198. [min()](#198-min)
199. [topN()](#199-topn)
200. [bottomN()](#200-bottomn)
201. [sum()](#201-sum)
202. [avg()](#202-avg)
203. [count()](#203-count)
204. [push()](#204-push)
205. [reverse()](#205-reverse)
206. [slice()](#206-slice)
207. [sort()](#207-sort)
208. [distinct()](#208-distinct)
209. [join()](#209-join)
210. [findAndModify()](#210-findandmodify)
211. [findByMax()](#211-findbymax)
212. [findByMin()](#212-findbymin)
213. [findByNullOrMax()](#213-findbynullormax)
214. [findByNullOrMin()](#214-findbynullormin)
215. [findByMaxOrNull()](#215-findbymaxornull)
216. [findByMinOrNull()](#216-findbyminornull)

Structural Functions

217. [entries()](#217-entries)
218. [keys()](#218-keys)
219. [depthLimit()](#219-depthlimit)
220. [collect()](#220-collect)
221. [cumulateCollect()](#221-cumulatecollect)
222. [wrap()](#222-wrap)
223. [toArray()](#223-toarray)
224. [toObject()](#224-toobject)
225. [mergeArrays()](#225-mergearrays)
226. [mergeObjects()](#226-mergeobjects)
227. [flatten()](#227-flatten)
228. [unflatten()](#228-unflatten)
229. [map()](#229-map)
230. [field()](#230-field)
231. [group()](#231-group)
232. [unwind()](#232-unwind)
233. [assort()](#233-assort)

Programmable Functions

234. [eval()](#234-eval)
235. [json()](#235-json)
236. [if()](#236-if)
237. [ifNot()](#237-ifnot)
238. [coalesce()](#238-coalesce)
239. [caseValue()](#239-casevalue)
240. [caseValueIgnoreCase()](#240-casevalueignorecase)
241. [indexedValue()](#241-indexedvalue)
242. [cycleValue()](#242-cyclevalue)
243. [steps()](#243-steps)
244. [get()](#244-get)
245. [let()](#245-let)
246. [mergeArraysOption()](#246-mergearraysoption)

Following are description of each function.

### Arithmetic Functions

#### 1. abs()

Returns the absolute value of a number.

    -3.14.abs() ==> 3.14

    abs(3.14) ==> 3.14

#### 2. calc()

Evaluates a math expression using mXparser library.

    1.5.calc(? * 2 + ? / 2) ==> 3.75

    calc(2^8) ==> 256.0

    calc(sqrt(a^2 + b^2), a:3, b:4) ==> 5.0

#### 3. ceil()

Rounds a number up to the nearest integer.

    3.14.ceil() ==> 4

    ceil(-3.14) ==> -3

#### 4. floor()

Rounds a number down to the nearest integer.

    3.14.floor() ==> 3

    floor(-3.14) ==> -4

#### 5. mod()

Returns the remainder of a division operation.

    8.mod(3) ==> 2

    8.mod(?, 3) ==> 2

    mod(-8, 3) ==> 1

    3.mod(-8, ?) ==> 1

#### 6. round()

Rounds a number to a specified number of decimal places.

    3.14.round(1) ==> 3.1

    3.14.round(?, 1) ==> 3.1

    round(3.56, 0) ==> 4

### String Functions

#### 7. abbreviate()

Abbreviates a String using ellipses.

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

Appends a string to the end of another string.

    'abc'.append('xyz') ==> "abcxyz"

    'abc'.append(?, 'xyz') ==> "abcxyz"

    append('abcxyz', 'xyz') ==> "abcxyzxyz"

    'xyz'.append('abcXYZ', ?) ==> "abcXYZxyz"

#### 9. appendIfMissing()

Appends the suffix to the end of the string if the string does not already end with the suffix.

    'abc'.appendIfMissing('xyz') ==> "abcxyz"

    'abc'.appendIfMissing(?, 'xyz') ==> "abcxyz"

    appendIfMissing('abcxyz', 'xyz') ==> "abcxyz"

    'xyz'.appendIfMissing('abcXYZ', ?) ==> "abcXYZxyz"

#### 10. appendIfMissingIgnoreCase()

Appends the suffix to the end of the string if the string does not already end, case insensitive, with any of the suffixes.

    'abc'.appendIfMissingIgnoreCase('xyz') ==> "abcxyz"

    'abc'.appendIfMissingIgnoreCase(?, 'xyz') ==> "abcxyz"

    appendIfMissingIgnoreCase('abcxyz', 'xyz') ==> "abcxyz"

    'xyz'.appendIfMissingIgnoreCase('abcXYZ', ?) ==> "abcXYZ"

#### 11. capitalize()

Uppercases the first character of a string.

    'cat'.capitalize() ==> "Cat"

    capitalize('cAt') ==> "CAt"

#### 12. center()

Centers a String in a larger String of a specified size. Uses a supplied String as the value to pad the String with.

    'abc'.center(7) ==> "  abc  "

    'abc'.center(7, 'X') ==> "XXabcXX"

    'abc'.center(?, 7, upperCase(?)) ==> "ABabcAB"

    center('abc', 7, '') ==> "  abc  "

    4.center('a', ?, 'yz') ==> "yayz"

#### 13. concat()

Concatenates multiple strings together.

    'Hello'.concat(2022, '... ', ?, ' World!') ==> "2022... Hello World!"

    json('{"a":"Hello","c":" World!"}').concat(a,b,c) ==> !unresolvable!

#### 14. concatFree()

Concatenates multiple strings together. Ignore null values.

    'Hello'.concatFree(2022, '... ', ?, ' World!') ==> "2022... Hello World!"

    json('{"a":"Hello","c":" World!"}').concatFree(a,b,c) ==> "Hello World!"

#### 15. default()

Returns first non-null value from a list of values. Returns an empty string if none found.

    json('{"a":1,"b":"B","c":null}').default(x) ==> ""

    json('{"a":1,"b":"B","c":null}').default(x,'Hi') ==> "Hi"

    json('{"a":1,"b":"B","c":null}').default(x,null,c,a,b) ==> 1

    json('{"a":1,"b":"B","c":null}').default(x,null,c,b,a) ==> "B"

#### 16. keepAfter()

Returns substring after a given string.

    'abcxmnxyz'.keepAfter('x') ==> "mnxyz"

    'abcxmnxyz'.keepAfter(?, 'X') ==> ""

    keepAfter('abcxmnxyz', 'mn') ==> "xyz"

#### 17. keepAfterIgnoreCase()

Same as above but ignores case.

    'abcxmnxyz'.keepAfterIgnoreCase('x') ==> "mnxyz"

    'abcxmnxyz'.keepAfterIgnoreCase(?, 'X') ==> "mnxyz"

    keepAfterIgnoreCase('abcxmnxyz', 'mn') ==> "xyz"

#### 18. keepAfterLast()

Returns substring after last occurrence of given string.

    'abcxmnxyz'.keepAfterLast('x') ==> "yz"

    'abcxmnxyz'.keepAfterLast(?, 'X') ==> ""

    keepAfterLast('abcxmnxyz', 'mn') ==> "xyz"

#### 19. keepAfterLastIgnoreCase()

Same as above but ignores case.

    'abcxmnxyz'.keepAfterLastIgnoreCase('x') ==> "yz"

    'abcxmnxyz'.keepAfterLastIgnoreCase(?, 'X') ==> "yz"

    keepAfterLastIgnoreCase('abcxmnxyz', 'mn') ==> "xyz"

#### 20. keepBefore()

Returns substring before a given string.

    'abcxmnxyz'.keepBefore('x') ==> "abc"

    'abcxmnxyz'.keepBefore(?, 'X') ==> ""

    keepBefore('abcxmnxyz', 'mn') ==> "abcx"

#### 21. keepBeforeIgnoreCase()

Same as above but ignores case.

    'abcxmnxyz'.keepBeforeIgnoreCase('x') ==> "abc"

    'abcxmnxyz'.keepBeforeIgnoreCase(?, 'X') ==> "abc"

    keepBeforeIgnoreCase('abcxmnxyz', 'mn') ==> "abcx"

#### 22. keepBeforeLast()

Returns substring before last occurrence of given string.

    'abcxmnxyz'.keepBeforeLast('x') ==> "abcxmn"

    'abcxmnxyz'.keepBeforeLast(?, 'X') ==> ""

    keepBeforeLast('abcxmnxyz', 'mn') ==> "abcx"

#### 23. keepBeforeLastIgnoreCase()

Same as above but ignores case.

    'abcxmnxyz'.keepBeforeLastIgnoreCase('x') ==> "abcxmn"

    'abcxmnxyz'.keepBeforeLastIgnoreCase(?, 'X') ==> "abcxmn"

    keepBeforeLastIgnoreCase('abcxmnxyz', 'mn') ==> "abcx"

#### 24. leftPad()

Left pad a String with a specified character.

    'bat'.leftPad(5) ==> "  bat"

    'bat'.leftPad(?, 8, 'yz') ==> "yzyzybat"

    leftPad('bat', 3, 'yz') ==> "bat"

    5.leftPad('bat', ?, '') ==> "  bat"

#### 25. length()

Returns length of a string.

    'Josson'.length() ==> 6

    length('Josson') ==> 6

    length(2022) ==> 4

#### 26. lowerCase()

Converts a String to lower case.

    'Cat'.lowerCase() ==> "cat"

    lowerCase('cAt') ==> "cat"

#### 27. notEmpty()

Returns the node itself if it is a non-empty string, else returns first evaluated non-empty value from a list.

    'abc'.notEmpty('xyz') ==> "abc"

    ''.notEmpty(null, '', 'xyz') ==> "xyz"

    json('{"a":"","b":"","c":"abc"}').notEmpty(a,b,c,'xyz') ==> "abc"

#### 28. notBlank()

Returns the node itself if it is a non-blank string, else returns first evaluated non-blank value from a list.

    'abc'.notBlank('xyz') ==> "abc"

    ' '.notBlank(null, '  ', 'xyz') ==> "xyz"

    json('{"a":" ","b":" ","c":"abc"}').notBlank(a,b,c,'xyz') ==> "abc"

#### 29. prepend()

Prepends the prefix to the start of the string.

    'abc'.prepend('xyz') ==> "xyzabc"

    'abc'.prepend(?, 'xyz') ==> "xyzabc"

    prepend('xyzabc', 'xyz') ==> "xyzxyzabc"

    'xyz'.prepend('XYZabc', ?) ==> "xyzXYZabc"

#### 30. prependIfMissing()

Prepends the prefix to the start of the string if the string does not already start with the prefix.

    'abc'.prependIfMissing('xyz') ==> "xyzabc"

    'abc'.prependIfMissing(?, 'xyz') ==> "xyzabc"

    prependIfMissing('xyzabc', 'xyz') ==> "xyzabc"

    'xyz'.prependIfMissing('XYZabc', ?) ==> "xyzXYZabc"

#### 31. prependIfMissingIgnoreCase()

Prepends the prefix to the start of the string if the string does not already start, case insensitive, with the prefix.

    'abc'.prependIfMissingIgnoreCase('xyz') ==> "xyzabc"

    'abc'.prependIfMissingIgnoreCase(?, 'xyz') ==> "xyzabc"

    prependIfMissingIgnoreCase('xyzabc', 'xyz') ==> "xyzabc"

    'xyz'.prependIfMissingIgnoreCase('XYZabc', ?) ==> "XYZabc"

#### 32. removeEnd()

Removes a substring only if it is at the end of a source string, otherwise returns the source string.

    'www.domain.com'.removeEnd('.com') ==> "www.domain"

    'www.domain.com'.removeEnd(?, '.Com') ==> "www.domain.com"

    removeEnd('www.domain.com', '.com') ==> "www.domain"

#### 33. removeEndIgnoreCase()

Case insensitive removal of a substring if it is at the end of a source string, otherwise returns the source string.

    'www.domain.COM'.removeEndIgnoreCase('.com') ==> "www.domain"

    'www.domain.com'.removeEndIgnoreCase(?, '.Com') ==> "www.domain"

    removeEndIgnoreCase('www.domain.com', '.COM') ==> "www.domain"

#### 34. removeStart()

Removes a substring only if it is at the beginning of a source string, otherwise returns the source string.

    'www.domain.com'.removeStart('www.') ==> "domain.com"

    'www.domain.com'.removeStart(?, '.Www') ==> "www.domain.com"

    removeStart('www.domain.com', 'www.') ==> "domain.com"

#### 35. removeStartIgnoreCase()

Case insensitive removal of a substring if it is at the beginning of a source string, otherwise returns the source string.

    'WWW.domain.com'.removeStartIgnoreCase('www.') ==> "domain.com"

    'www.domain.com'.removeStartIgnoreCase(?, '.Www') ==> "www.domain.com"

    removeStartIgnoreCase('www.domain.com', 'WWW.') ==> "domain.com"

#### 36. repeat()

Repeats a string multiple times to form a new String.

    'a'.repeat(3) ==> "aaa"

    'ab'.repeat(?, 2) ==> "abab"

    repeat('abc', 2) ==> "abcabc"

    3.repeat('abc', ?) ==> "abcabcabc"

#### 37. replace()

Replaces a String with another String inside a larger String, for the first specified number (or -1 if no maximum) of values of the search String.

    'abaa'.replace('a', 'z') ==> "zbzz"

    'abaa'.replace(?, 'a', 'z', -1) ==> "zbzz"

    replace('abaa', 'a', '', -1) ==> "b"

    replace('abaa', 'A', 'z', 1) ==> "abaa"

    'a'.replace('abaa', ?, 'z', 2) ==> "zbza"

#### 38. replaceIgnoreCase()

Same as above but ignores case.

    'abaa'.replaceIgnoreCase('a', 'z') ==> "zbzz"

    'abaa'.replaceIgnoreCase(?, 'a', 'z', -1) ==> "zbzz"

    replaceIgnoreCase('abaa', 'a', '', -1) ==> "b"

    replaceIgnoreCase('abaa', 'A', 'z', 1) ==> "zbaa"

    'a'.replaceIgnoreCase('abaa', ?, 'z', 2) ==> "zbza"

#### 39. rightPad()

Right pad a String with a specified character.

    'bat'.rightPad(5) ==> "bat  "

    'bat'.rightPad(?, 8, 'yz') ==> "batyzyzy"

    rightPad('bat', 3, 'yz') ==> "bat"

    rightPad('bat', 5, '') ==> "bat  "

#### 40. split()

Splits the provided text into an array, separators specified.

    'abc def'.split() ==> [ "abc", "def" ]

    'abc  def'.split(' ') ==> [ "abc", "def" ]

    ' abc  def '.split(?, ' ') ==> [ "abc", "def" ]

    split('ab:cd:ef', ':') ==> [ "ab", "cd", "ef" ]

#### 41. splitMax()

Splits keeping at most N substrings. A zero or negative value implies no limit.

    'ab:cd:ef'.splitMax(':', 0) ==> [ "ab", "cd", "ef" ]

    splitMax('ab:cd:ef', ':', 2, true) ==> [ "ab", "cd:ef" ]

#### 42. separate()

Splits the provided text into an array, separators specified.

    'ab:cd:ef'.separate(':') ==> [ "ab", "cd", "ef" ]

    separate('ab-!-cd-!-ef', '-!-') ==> [ "ab", "cd", "ef" ]

#### 43. separateMax()

Splits keeping at most N substrings. A zero or negative value implies no limit.

    'ab:cd:ef'.separateMax(':', 0) ==> [ "ab", "cd", "ef" ]

    separateMax('ab-!-cd-!-ef', '-!-', 2, true) ==> [ "ab", "cd-!-ef" ]

#### 44. strip()

Strips whitespace from the start and end of a String.

    '  abc  '.strip(' ') ==> "abc"

    '  abcyx'.strip('xyz') ==> "  abc"

    strip('z abcyx', 'xyz') ==> " abc"

#### 45. stripEnd()

Strips any of a set of characters from the end of a String.

    '  abc  '.stripEnd(' ') ==> "  abc"

    'z abcyx'.stripEnd('xyz') ==> "z abc"

    stripEnd('z abcyx', 'xyz') ==> "z abc"

#### 46. stripStart()

Strips any of a set of characters from the start of a String.

    '  abc  '.stripStart(' ') ==> "abc  "

    'z abcyx'.stripStart('xyz') ==> " abcyx"

    stripStart('z abcyx', 'xyz') ==> " abcyx"

#### 47. substr()

Gets a substring from the specified String avoiding exceptions.
A negative start position can be used to start/end N characters from the end of the String.

    'abc'.substr(1) ==> "bc"

    'abc'.substr(0, 2) ==> "ab"

    'abc'.substr(?, 1, 2) ==> "b"

    substr('abc', -2, -1) ==> "b"

    2.substr('abc', -4, ?) ==> "ab"

#### 48. trim()

Removes control characters (char &lt;= 32) from both ends of this String.

    'abc'.trim() ==> "abc"

    trim('  abc  ') ==> "abc"

#### 49. uncapitalize()

Uncapitalizes a String, changing the first character to lower case.

    'Cat'.uncapitalize() ==> "cat"

    uncapitalize('CAt') ==> "cAt"

#### 50. upperCase()

Converts a String to upper case.

    'Cat'.upperCase() ==> "CAT"

    upperCase('cAt') ==> "CAT"

#### 51. camelCase()

Converts words in a String to camel case, with the first word in lower case.

    'i am  a   man .and..i have_a__pen'.camelCase() ==> "iAmAManAndIHaveAPen"

    ' Text  to__c@mel case '.camelCase() ==> "textToC@melCase"

    ' Text  to__c@mel case '.camelCase(' _.@') ==> "textToCMelCase"

#### 52. upperCamelCase()

Converts words in a String to camel case, with the first word also in camel case.

    'i am  a   man .and..i have_a__pen'.camelCase() ==> "IAmAManAndIHaveAPen"

    ' Text  to__c@mel case '.camelCase() ==> "TextToC@melCase"

    ' Text  to__c@mel case '.camelCase(' _.@') ==> "TextToCMelCase"

#### 53. snakeCase()

Converts words in a String to snake case.

    ' Text  to__snake case '.snakeCase() ==> "Text_to_snake_case"

    'camelToSnakeCase'.snakeCase() ==> "camel_To_Snake_Case"

#### 54. lowerSnakeCase()

Converts words in a String to lower snake case.

    ' Text  to__snake case '.lowerSnakeCase() ==> "text_to_snake_case"

    'camelToSnakeCase'.lowerSnakeCase() ==> "camel_to_snake_case"

#### 55. upperSnakeCase()

Converts words in a String to upper snake case.

    ' Text  to__snake case '.upperSnakeCase() ==> "TEXT_TO_SNAKE_CASE"

    'camelToSnakeCase'.upperSnakeCase() ==> "CAMEL_TO_SNAKE_CASE"

#### 56. camelSnakeCase()

Converts words in a String to camel snake case.

    ' Text  to__snake case '.camelSnakeCase() ==> "Text_To_Snake_Case"

    'camelToSnakeCase'.camelSnakeCase() ==> "Camel_To_Snake_Case"

#### 57. singleQuote() / quote() / q()

Adds surrounding quotes with escape handling.

    'Peggy''s cat'.singleQuote() ==> "'Peggy''s cat'"

    123.singleQuote() ==> "'123'"

    quote('Raymond''s dog') ==> "'Raymond''s dog'"

    q(True) ==> "'true'"

#### 58. doubleQuote() / qq()

Adds surrounding double quotes with escape handling.

    'Peggy\"s cat'.doubleQuote() ==> "\"Peggy\\\"s cat\""

    12.3.doubleQuote() ==> "\"12.3\""

    qq('Raymond\"s dog') ==> "\"Raymond\\\"s dog\""

    qq(False) ==> "\"false\""

### Date Functions

#### 59. amPmOfDay()

Returns the AM/PM indicator of a datetime value in uppercase.

    '2022-01-02T03:04:05'.amPmOfDay() ==> "AM"

    amPmOfDay('2022-02-04T13:14:15') ==> "PM"

#### 60. second()

Returns the second component (from 0 to 60) of a datetime.

    '2022-01-02T03:04:05'.second() ==> 5

    second('2022-02-04T13:14:15') ==> 15

#### 61. secondOfDay()

Returns the seconds since midnight (from 0 to 86399) of a datetime.

    '2022-01-02T03:04:05'.secondOfDay() ==> 11045

    secondOfDay('2022-02-04T13:14:15') ==> 47655

#### 62. minute()

Returns the minute component (from 0 to 59) of a datetime.

    '2022-01-02T03:04:05'.minute() ==> 4

    minute('2022-02-04T13:14:15') ==> 14

#### 63. minuteOfDay()

Returns the minutes since midnight (from 0 to 1439) of a datetime.

    '2022-01-02T03:04:05'.minuteOfDay() ==> 184

    minuteOfDay('2022-02-04T13:14:15') ==> 794

#### 64. hourOfAmPm()

Returns the hour (from 1 to 12) component of a datetime adjusted for AM/PM.

    '2022-01-02T03:04:05'.hourOfAmPm() ==> 3

    hourOfAmPm('2022-02-04T13:14:15') ==> 1

#### 65. hour()

Returns the hour (from 0 to 23) component of a datetime.

    '2022-01-02T03:04:05'.hour() ==> 3

    hour('2022-02-04T13:14:15') ==> 13

#### 66. dayOfWeek()

Returns the day of week as number (from 1 to 7) with Monday=1.

    '2022-01-02T03:04:05'.dayOfWeek() ==> 7

    dayOfWeek('2022-02-04T13:14:15') ==> 5

#### 67. day()

Returns the day of month (from 1 to 31) component of a datetime.

    '2022-01-02T03:04:05'.day() ==> 2

    day('2022-02-04T13:14:15') ==> 4

#### 68. dayOfYear()

Returns the day number of the year (from 1 to 366) of a datetime.

    '2022-01-02T03:04:05'.dayOfYear() ==> 2

    dayOfYear('2022-02-04T13:14:15') ==> 35

#### 69. month()

Returns the month (from 1 to 12) component of a datetime.

    '2022-01-02T03:04:05'.month() ==> 1

    month('2022-02-04T13:14:15') ==> 2

#### 70. year()

Returns the year component of a datetime.

    '2022-01-02T03:04:05'.year() ==> 2022

    year('2022-02-04T13:14:15') ==> 2022

#### 71. plusSeconds()

Adds the specified seconds to a datetime value.

    '2022-01-02T03:04:05'.plusSeconds(9) ==> "2022-01-02T03:04:14"

    '2022-01-02T03:04:05'.plusSeconds(?, 10) ==> "2022-01-02T03:04:15"

    plusSeconds('2022-02-04T13:14:15', 9) ==> "2022-02-04T13:14:24"

#### 72. plusMinutes()

Adds the specified minutes to a datetime value.

    '2022-01-02T03:04:05'.plusMinutes(9) ==> "2022-01-02T03:13:05"

    '2022-01-02T03:04:05'.plusMinutes(?, 10) ==> "2022-01-02T03:14:05"

    plusMinutes('2022-02-04T13:14:15', 9) ==> "2022-02-04T13:23:15"

#### 73. plusHours()

Adds the specified hours to a datetime value.

    '2022-01-02T03:04:05'.plusHours(9) ==> "2022-01-02T12:04:05"

    '2022-01-02T03:04:05'.plusHours(?, 10) ==> "2022-01-02T13:04:05"

    plusHours('2022-02-04T13:14:15', 9) ==> "2022-02-04T22:14:15"

#### 74. plusDays()

Adds the specified days to a datetime value.

    '2022-01-02T03:04:05'.plusDays(9) ==> "2022-01-11T03:04:05"

    '2022-01-02T03:04:05'.plusDays(?, 10) ==> "2022-01-12T03:04:05"

    plusDays('2022-02-04T13:14:15', 9) ==> "2022-02-13T13:14:15"

#### 75. plusWeeks()

Adds the specified weeks to a datetime value.

    '2022-01-02T03:04:05'.plusWeeks(9) ==> "2022-03-06T03:04:05"

    '2022-01-02T03:04:05'.plusWeeks(?, 10) ==> "2022-03-13T03:04:05"

    plusWeeks('2022-02-04T13:14:15', 9) ==> "2022-04-08T13:14:15"

#### 76. plusMonths()

Adds the specified months to a datetime value.

    '2022-01-02T03:04:05'.plusMonths(9) ==> "2022-10-02T03:04:05"

    '2022-01-02T03:04:05'.plusMonths(?, 10) ==> "2022-11-02T03:04:05"

    plusMonths('2022-02-04T13:14:15', 9) ==> "2022-11-04T13:14:15"

#### 77. plusYears()

Adds the specified years to a datetime value.

    '2022-01-02T03:04:05'.plusYears(9) ==> "2031-01-02T03:04:05"

    '2022-01-02T03:04:05'.plusYears(?, 10) ==> "2032-01-02T03:04:05"

    plusYears('2022-02-04T13:14:15', 9) ==> "2031-02-04T13:14:15"

#### 78. minusSeconds()

Subtracts the specified seconds from a datetime value.

    '2022-01-02T03:04:05'.minusSeconds(9) ==> "2022-01-02T03:03:56"

    '2022-01-02T03:04:05'.minusSeconds(?, 10) ==> "2022-01-02T03:03:55"

    minusSeconds('2022-02-04T13:14:15', 9) ==> "2022-02-04T13:14:06"

#### 79. minusMinutes()

Subtracts the specified minutes from a datetime value.

    '2022-01-02T03:04:05'.minusMinutes(9) ==> "2022-01-02T02:55:05"

    '2022-01-02T03:04:05'.minusMinutes(?, 10) ==> "2022-01-02T02:54:05"

    minusMinutes('2022-02-04T13:14:15', 9) ==> "2022-02-04T13:05:15"

#### 80. minusHours()

Subtracts the specified hours from a datetime value.

    '2022-01-02T03:04:05'.minusHours(9) ==> "2022-01-01T18:04:05"

    '2022-01-02T03:04:05'.minusHours(?, 10) ==> "2022-01-01T17:04:05"

    minusHours('2022-02-04T13:14:15', 9) ==> "2022-02-04T04:14:15"

#### 81. minusDays()

Subtracts the specified days from a datetime value.

    '2022-01-02T03:04:05'.minusDays(9) ==> "2021-12-24T03:04:05"

    '2022-01-02T03:04:05'.minusDays(?, 10) ==> "2021-12-23T03:04:05"

    minusDays('2022-02-04T13:14:15', 9) ==> "2022-01-26T13:14:15"

#### 82. minusWeeks()

Subtracts the specified weeks from a datetime value.

    '2022-01-02T03:04:05'.minusWeeks(9) ==> "2021-10-31T03:04:05"

    '2022-01-02T03:04:05'.minusWeeks(?, 10) ==> "2021-10-24T03:04:05"

    minusWeeks('2022-02-04T13:14:15', 9) ==> "2021-12-03T13:14:15"

#### 83. minusMonths()

Subtracts the specified months from a datetime value.

    '2022-01-02T03:04:05'.minusMonths(9) ==> "2021-04-02T03:04:05"

    '2022-01-02T03:04:05'.minusMonths(?, 10) ==> "2021-03-02T03:04:05"

    minusMonths('2022-02-04T13:14:15', 9) ==> "2021-05-04T13:14:15"

#### 84. minusYears()

Subtracts the specified years from a datetime value.

    '2022-01-02T03:04:05'.minusYears(9) ==> "2013-01-02T03:04:05"

    '2022-01-02T03:04:05'.minusYears(?, 10) ==> "2012-01-02T03:04:05"

    minusYears('2022-02-04T13:14:15', 9) ==> "2013-02-04T13:14:15"

#### 85. truncateToMicro()

Truncates a datetime to a less precise value.

    '2022-01-02T03:04:05.229390600'.truncateToMicro() ==> "2022-01-02T03:04:05.229390"

    truncateToMicro('2022-02-04T13:14:15.229390600') ==> "2022-02-04T13:14:15.229390"

#### 86. truncateToMilli()

Truncates a datetime to a less precise value.

    '2022-01-02T03:04:05.229390600'.truncateToMilli() ==> "2022-01-02T03:04:05.229"

    truncateToMilli('2022-02-04T13:14:15.229390600') ==> "2022-02-04T13:14:15.229"

#### 87. truncateToSecond()

Truncates a datetime to a less precise value.

    '2022-01-02T03:04:05.229390600'.truncateToSecond() ==> "2022-01-02T03:04:05"

    truncateToSecond('2022-02-04T13:14:15.229390600') ==> "2022-02-04T13:14:15"

#### 88. truncateToMinute()

Truncates a datetime to a less precise value.

    '2022-01-02T03:04:05.229390600'.truncateToMinute() ==> "2022-01-02T03:04"

    truncateToMinute('2022-02-04T13:14:15.229390600') ==> "2022-02-04T13:14"

#### 89. truncateToHour()

Truncates a datetime to a less precise value.

    '2022-01-02T03:04:05.229390600'.truncateToHour() ==> "2022-01-02T03:00"

    truncateToHour('2022-02-04T13:14:15.229390600') ==> "2022-02-04T13:00"

#### 90. truncateToDay()

Truncates a datetime to a less precise value.

    '2022-01-02T03:04:05.229390600'.truncateToDay() ==> "2022-01-02T00:00"

    truncateToDay('2022-02-04T13:14:15.229390600') ==> "2022-02-04T00:00"

#### 91. truncateToMonth()

Truncates a datetime to a less precise value.

    '2022-01-02T03:04:05.229390600'.truncateToMonth() ==> "2022-01-01T00:00"

    truncateToMonth('2022-02-04T13:14:15.229390600') ==> "2022-02-01T00:00"

#### 92. truncateToYear()

Truncates a datetime to a less precise value.

    '2022-01-02T03:04:05.229390600'.truncateToYear() ==> "2022-01-01T00:00"

    truncateToYear('2022-02-04T13:14:15.229390600') ==> "2022-01-01T00:00"

#### 93. withNano()

Sets a component of a datetime value.

    '2022-01-02T03:04'.withNano(789) ==> "2022-01-02T03:04:00.000000789"

    '2022-01-02T03:04'.withNano(?, 789) ==> "2022-01-02T03:04:00.000000789"

    withNano('2022-02-04T13:14', 789) ==> "2022-02-04T13:14:00.000000789"

#### 94. withMicro()

Sets a component of a datetime value.

    '2022-01-02T03:04'.withMicro(789) ==> "2022-01-02T03:04:00.000789"

    '2022-01-02T03:04'.withMicro(?, 789) ==> "2022-01-02T03:04:00.000789"

    withMicro('2022-02-04T13:14', 789) ==> "2022-02-04T13:14:00.000789"

#### 95. withMilli()

Sets a component of a datetime value.

    '2022-01-02T03:04'.withMilli(789) ==> "2022-01-02T03:04:00.789"

    '2022-01-02T03:04'.withMilli(?, 789) ==> "2022-01-02T03:04:00.789"

    withMilli('2022-02-04T13:14', 789) ==> "2022-02-04T13:14:00.789"

#### 96. withSecond()

Sets a component of a datetime value.

    '2022-01-02T03:04'.withSecond(35) ==> "2022-01-02T03:04:35"

    '2022-01-02T03:04'.withSecond(?, 35) ==> "2022-01-02T03:04:35"

    withSecond('2022-02-04T13:14', 35) ==> "2022-02-04T13:14:35"

#### 97. withMinute()

Sets a component of a datetime value.

    '2022-01-02T03:04'.withMinute(35) ==> "2022-01-02T03:35"

    '2022-01-02T03:04'.withMinute(?, 35) ==> "2022-01-02T03:35"

    withMinute('2022-02-04T13:14', 35) ==> "2022-02-04T13:35"

#### 98. withHour()

Sets a component of a datetime value.

    '2022-01-02T03:04'.withHour(16) ==> "2022-01-02T16:04"

    '2022-01-02T03:04'.withHour(?, 16) ==> "2022-01-02T16:04"

    withHour('2022-02-04T13:14', 16) ==> "2022-02-04T16:14"

#### 99. withDay()

Sets a component of a datetime value.

    '2022-01-02T03:04'.withDay(25) ==> "2022-01-25T03:04"

    '2022-01-02T03:04'.withDay(?, 25) ==> "2022-01-25T03:04"

    withDay('2022-02-04T13:14', 25) ==> "2022-02-25T13:14"

#### 100. withDayOfYear()

Sets a component of a datetime value.

    '2022-01-02T03:04'.withDayOfYear(123) ==> "2022-05-03T03:04"

    '2022-01-02T03:04'.withDayOfYear(?, 123) ==> "2022-05-03T03:04"

    withDayOfYear('2022-02-04T13:14', 123) ==> "2022-05-03T13:14"

#### 101. withMonth()

Sets a component of a datetime value.

    '2022-01-02T03:04'.withMonth(7) ==> "2022-07-02T03:04"

    '2022-01-02T03:04'.withMonth(?, 7) ==> "2022-07-02T03:04"

    withMonth('2022-02-04T13:14', 7) ==> "2022-07-04T13:14"

#### 102. withYear()

Sets a component of a datetime value.

    '2022-01-02T03:04'.withYear(2047) ==> "2047-01-02T03:04"

    '2022-01-02T03:04'.withYear(?, 2047) ==> "2047-01-02T03:04"

    withYear('2022-02-04T13:14', 2047) ==> "2047-02-04T13:14"

#### 103. dayEnd()

Returns the last millisecond of a day for a datetime.

    '2022-01-02T03:04'.dayEnd() ==> "2022-01-02T23:59:59.999999999"

    dayEnd('2022-02-04T13:14') ==> "2022-02-04T23:59:59.999999999"

#### 104. monthEnd()

Returns the last millisecond of a month for a datetime.

    '2022-01-02T03:04'.monthEnd() ==> "2022-01-31T23:59:59.999999999"

    monthEnd('2022-02-04T13:14') ==> "2022-02-28T23:59:59.999999999"

#### 105. yearEnd()

Returns the last millisecond of a year for a datetime.

    '2022-01-02T03:04'.yearEnd() ==> "2022-12-31T23:59:59.999999999"

    yearEnd('2022-02-04T13:14') ==> "2022-12-31T23:59:59.999999999"

#### 106. lengthOfMonth()

Returns number of days in the month of a datetime.

    '2022-01-02T03:04'.lengthOfMonth() ==> 31

    lengthOfMonth('2022-02-04T13:14') ==> 28

#### 107. lengthOfYear()

Returns 366 if a datetime is a leap year, else 365.

    '2022-01-02T03:04'.lengthOfYear() ==> 365

    lengthOfYear('2024-02-04T13:14') ==> 366

#### 108. untilInSecond()

Calculates the difference between two datetimes in seconds.

    '2020-01-02T23:04'.untilInSecond('2022-06-11T01:02') ==> 76903080

    untilInSecond('2021-12-12T13:14','2021-03-03T01:00') ==> -24581640

#### 109. untilInMinute()

Calculates the difference between two datetimes in minutes.

    '2020-01-02T23:04'.untilInMinute('2022-06-11T01:02') ==> 1281718

    untilInMinute('2021-12-12T13:14','2021-03-03T01:00') ==> -409694

#### 110. untilInHour()

Calculates the difference between two datetimes in hours.

    '2020-01-02T23:04'.untilInHour('2022-06-11T01:02') ==> 21361

    untilInHour('2021-12-12T13:14','2021-03-03T01:00') ==> -6828

#### 111. untilInDay()

Calculates the difference between two datetimes in days.

    '2020-01-02T23:04'.untilInDay('2022-06-11T01:02') ==> 890

    untilInDay('2021-12-12T13:14','2021-03-03T01:00') ==> -284

#### 112. untilInMonth()

Calculates the difference between two datetimes in months.

    '2020-01-02T23:04'.untilInMonth('2022-06-11T01:02') ==> 29

    untilInMonth('2021-12-12T13:14','2021-03-03T01:00') ==> -9

#### 113. untilInYear()

Calculates the difference between two datetimes in years.

    '2020-01-02T23:04'.untilInYear('2022-06-11T01:02') ==> 2

    untilInYear('2021-12-12T13:14','2021-03-03T01:00') ==> 0

#### 114. localToOffsetDate()

Converts local datetime to offset datetime format.

    '2022-01-02T03:04:05'.localToOffsetDate() ==> "2022-01-02T03:04:05+08:00"

    localToOffsetDate('2022-02-04T13:14:15') ==> "2022-02-04T13:14:15+08:00"

#### 115. offsetToLocalDate()

Converts offset datetime to local datetime format.

    '2022-01-02T03:04:05+08:00'.offsetToLocalDate() ==> "2022-01-02T03:04:05"

    offsetToLocalDate('2022-02-04T13:14:15+08:00') ==> "2022-02-04T13:14:15"

#### 116. localDateToMillis()

Converts local datetime format to milliseconds.

    '2022-01-02T03:04:05.12345'.localDateToMillis() ==> 1641063845123

    localDateToMillis('2022-02-04T13:14:15.12345') ==> 1643951655123

#### 117. millisToLocalDate()

Converts milliseconds to local datetime format.

    1641063845123.millisToLocalDate() ==> "2022-01-02T03:04:05.123"

    millisToLocalDate(1643951655123) ==> "2022-02-04T13:14:15.123"

#### 118. localDateToSeconds()

Converts local datetime format to seconds.

    '2022-01-02T03:04:05.12345'.localDateToSeconds() ==> 1641063845

    localDateToSeconds('2022-02-04T13:14:15.12345') ==> 1643951655

#### 119. secondsToLocalDate()

Converts seconds to local datetime format.

    1641063845.secondsToLocalDate() ==> "2022-01-02T03:04:05"

    secondsToLocalDate(1643951655) ==> "2022-02-04T13:14:15"

#### 120. offsetDateToMillis()

Converts offset datetime format to milliseconds.

    '2022-01-02T03:04:05.12345+08:00'.offsetDateToMillis() ==> 1641063845123

    offsetDateToMillis('2022-02-04T13:14:15.12345+08:00') ==> 1643951655123

#### 121. millisToOffsetDate()

Converts milliseconds to offset datetime format.

    1641063845123.millisToOffsetDate() ==> "2022-01-02T03:04:05.123+08:00"

    millisToOffsetDate(1643951655123) ==> "2022-02-04T13:14:15.123+08:00"

#### 122. offsetDateToSeconds()

Converts offset datetime format to seconds.

    '2022-01-02T03:04:05.12345+08:00'.offsetDateToSeconds() ==> 1641063845

    offsetDateToSeconds('2022-02-04T13:14:15.12345+08:00') ==> 1643951655

#### 123. secondsToOffsetDate()

Converts seconds to offset datetime format.

    1641063845.secondsToOffsetDate() ==> "2022-01-02T03:04:05+08:00"

    secondsToOffsetDate(1643951655) ==> "2022-02-04T13:14:15+08:00"

#### 124. now()

Returns the current date and time in local datetime format.

    now() ==> "2022-10-30T23:52:47.084650900"

    now().localToOffsetDate() ==> 2022-10-31T01:02:27.294741100+08:00s

### Format Functions

#### 125. b64Encode()

Encodes a string to base64 format.

    'abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ'.b64Encode()
    ==>
    "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg=="

#### 126. b64EncodeNoPadding()

Encodes without padding at the end.

    b64EncodeNoPadding('abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ')
    ==>
    "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg"

#### 127. b64MimeEncode()

Encodes for MIME format with newlines every 76 chars.

    'abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ'.b64MimeEncode()
    ==>
    "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9Q\r\nUVJTVFVWV1hZWg=="

#### 128. b64MimeEncodeNoPadding()

Encodes for MIME without padding.

    b64MimeEncodeNoPadding('abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ')
    ==>
    "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9Q\r\nUVJTVFVWV1hZWg"

#### 129. b64UrlEncode()

Encodes for URL format by replacing padding chars.

    'abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ'.b64UrlEncode()
    ==>
    "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp-IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg=="

#### 130. b64UrlEncodeNoPadding()

Encodes for URL without padding.

    b64UrlEncodeNoPadding('abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ')
    ==>
    "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp-IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg"

#### 131. b64Decode()

Decodes a base64 encoded string.

    'YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg=='.b64Decode()
    ==>
    "abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    b64Decode('YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg')
    ==>
    "abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ"

#### 132. b64MimeDecode()

Decodes a base64 encoded string from MIME format.

    'YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9Q\nUVJTVFVWV1hZWg=='.b64MimeDecode()
    ==>
    "abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    b64MimeDecode('YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9Q\r\nUVJTVFVWV1hZWg')
    ==>
    "abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ"

#### 133. b64UrlDecode()

Decodes from URL safe format.

    'YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp-IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg=='.b64UrlDecode()
    ==>
    "abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    b64UrlDecode('YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp-IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg')
    ==>
    "abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ"

#### 134. urlEncode()

URL encodes a string.

    'www.domain.com?a=1+2&b=3+4'.urlEncode() ==> "www.domain.com%3Fa%3D1%2B2%26b%3D3%2B4"

    urlEncode('www.domain.com?a=1+2&b=3+4') ==> "www.domain.com%3Fa%3D1%2B2%26b%3D3%2B4"

#### 135. urlDecode()

Decodes a URL encoded string.

    'www.domain.com%3Fa%3D1%2B2%26b%3D3%2B4'.urlDecode() ==> "www.domain.com?a=1+2&b=3+4"

    urlDecode('www.domain.com%3Fa%3D1%2B2%26b%3D3%2B4') ==> "www.domain.com?a=1+2&b=3+4"

#### 136. escapeHtml()

Translator object for escaping HTML version 4.0.

    '~!@#$%^&*()<>[]{}+-= "''\|_:;,./?'.escapeHtml() ==> "~!@#$%^&amp;*()&lt;&gt;[]{}+-= &quot;'\|_:;,./?"

#### 137. unescapeHtml()

Reverses HTML escaping.

    '~!@#$%^&amp;*()&lt;&gt;[]{}+-= &quot;''\|_:;,./?'.unescapeHtml() ==> "~!@#$%^&*()<>[]{}+-= "'\|_:;,./?"

#### 138. escapeXml()

Translator object for escaping XML 1.1.

    '~!@#$%^&*()<>[]{}+-= "''\|_:;,./?'.escapeXml() ==> "~!@#$%^&amp;*()&lt;&gt;[]{}+-= &quot;&apos;\|_:;,./?"

#### 139. unescapeXml()

Reverses XML escaping.

    '~!@#$%^&amp;*()&lt;&gt;[]{}+-= &quot;&apos;\|_:;,./?'.unescapeXml() ==> "~!@#$%^&*()<>[]{}+-= "'\|_:;,./?"

#### 140. formatDate()

Formats a local datetime to a string.

    '2022-01-02T03:04:05'.formatDate('dd/MM/yyyy HH:mm:ss') ==> "02/01/2022 03:04:05"

    '2022-01-02T03:04:05'.formatDate(?, 'yyyy-MM-dd') ==> "2022-01-02"

    formatDate('2022-01-02T03:04:05', 'EEE, MMM d, yyyy') ==> "Sun, Jan 2, 2022"

#### 141. formatNumber()

Formats a number to string using a format pattern.

    12345.6.formatNumber('HK$#,##0.00') ==> "HK$12,345.60"

    123.formatNumber(?, '#,##0.#') ==> "123"

    formatNumber(123.45, '#,##0.#') ==> "123.5"

#### 142. formatText()

Formats a text using a format pattern.

    'Dog'.formatText('[%-5s]') ==> "[Dog  ]"

    123.formatText(?, '[%5d]') ==> "[  123]"

    formatText('Dog', '[%5s]') ==> "[  Dog]"

#### 143. formatTexts()

Formats multiple texts using a format pattern.

    formatTexts('1:%s 2:%s 3:%s', 'a', 'b', 'c') ==> "1:a 2:b 3:c"

    'b'.formatTexts('1:%s 2:%s 3:%s', 'a', ?, 'c') ==> "1:a 2:b 3:c"

    json('{"A":"a","B":"b"}').formatTexts('1:%s 2:%s 3:%s', A, B, 'c') ==> "1:a 2:b 3:c"

    json('[{"a":1,"b":3},{"a":2,"b":4}]').formatTexts('a=%d b=%d',a,b) ==> [ "a=1 b=3", "a=2 b=4" ]

#### 144. toNumber()

Converts supported types to number.

    '123'.toNumber() ==> 123.0

    toNumber('abc') ==> 0.0

    toNumber(true) ==> 1.0

    toNumber(null) ==> !unresolvable!

    toNumber(json('{"a":1}')) ==> !unresolvable!

    toNumber(json('[1,2.0,"a",true,null]')) ==> [ 1, 2.0, 0.0, 1.0, null ]

#### 145. toString()

Converts supported types, including object and array, to a string.

    123.toString() ==> "123"

    toString(false) ==> "false"

    toString(null) ==> "null"

    toString(json('{"a":1}')) ==> "{\"a\":1}"

    toString(json('[1,2.0,"a",true,null]')) ==> "[1,2.0,\"a\",true,null]"

#### 146. toText()

Converts supported types to string.

    123.toText() ==> "123"

    toText(false) ==> "false"

    toText(null) ==> "null"

    toText(json('{"a":1}')) ==> !unresolvable!

    toText(json('[1,2.0,"a",true,null]')) ==> [ "1", "2.0", "a", "true", "null" ]

#### 147. csv()

Encodes arrays or objects to a CSV string.  Ignores null values.

    json('{"len1":"12.3\"","len2":null,"len3":"64.0\""}').csv() ==> "\"12.3\"\"\",,\"64.0\"\"\""

    csv(json('[[[[1,2],["3","4\""]]],{"a":1,"b":[2.0,8.888],"c":{"d":true,"e":null}}]')) ==> "1,2,3,\"4\"\"\",1,2.0,8.888,true,"

#### 148. csvShowNull()

Encodes arrays or objects to a CSV string.  Includes null values.

    json('{"len1":"12.3\"","len2":null,"len3":"64.0\""}').csvShowNull() ==> "12.3\"\"\",null,\"64.0\"\""

    csvShowNull(json('[[[[1,2],["3","4\""]]],{"a":1,"b":[2.0,8.888],"c":{"d":true,"e":null}}]')) ==> "1,2,3,\"4\"\"\",1,2.0,8.888,true,null"

#### 149. csvParams()

Encodes arrays or objects to a single quoted CSV string.  Includes null values.
It is useful to construct function parameters from object or array values for Jossons nested placeholders feature.

    json('{"len1":"12.3","len2":null,"len3":"64.0\""}').csvParams() ==> "'12.3',null,'64.0\"'"

    csvParams(json('[[[[1,2],["3","4''"]]],{"a":1,"b":[2.0,8.888],"c":{"d":true,"e":null}}]')) ==> "1,2,'3','4''',1,2.0,8.888,true,null"

### Logical Functions

#### 150. contains()

Checks if a string contains another string.

    'abcde'.contains('bc') ==> true

    contains('abcde','B') ==> false

    json('[1.0,2.8,3.0]').contains(?, '1') ==> false

    json('[1.0,2.8,3.0]').contains(1) ==> true

    contains(json('["1","2","3"]'), 2.0) ==> true

    contains(json('[1.0,2.8,3.00]'), '3.0') ==> true

    json('["1.0","2.0","3.0"]').contains(?, '3.0') ==> true

    json('[1,2,null,4]').contains(null) ==> true

    json('{"a":1,"b":2,"c":3}').contains('a') ==> true

#### 151. containsIgnoreCase()

Same as above but ignores case sensitivity.

    'abcde'.containsIgnoreCase('bc') ==> true

    containsIgnoreCase('abcde','B') ==> true

    json('["a","b","c"]').containsIgnoreCase(?, 'B') ==> true

    containsIgnoreCase(json('["a","b","c"]'), 'bc') ==> false

    json('{"a":1,"b":2,"c":3}').containsIgnoreCase('A') ==> true

#### 152. notContains()

Checks if a string does not contain another string.

    'abcde'.notContains('bc') ==> false

    notContains('abcde','B') ==> true

    json('[1.0,2.8,3.0]').notContains(?, 1) ==> false

    json('[1,2,null,4]').notContains(null) ==> false

#### 153. notContainsIgnoreCase()

Same as above but ignores case sensitivity.

    'abcde'.notContainsIgnoreCase('bc') ==> false

    notContainsIgnoreCase('abcde','B') ==> false

    json('["a","b","c"]').notContainsIgnoreCase(?, 'D') ==> true

#### 154. startsWith()

Checks if a string starts with a prefix.

    'abcdef'.startsWith('abc') ==> true

    'ABCDEF'.startsWith(?,'abc') ==> false

    startsWith('ABCDEF','cde') ==> false

#### 155. startsWithIgnoreCase()

Same as above but ignores case sensitivity.

    'abcdef'.startsWithIgnoreCase('abc') ==> true

    'ABCDEF'.startsWithIgnoreCase(?,'abc') ==> true

    startsWithIgnoreCase('ABCDEF','cde') ==> false

#### 156. notStartsWith()

Checks if a string does not start with a prefix.

    'abcdef'.notStartsWith('abc') ==> false

    'ABCDEF'.notStartsWith(?,'abc') ==> true

    notStartsWith('ABCDEF','cde') ==> true

#### 157. notStartsWithIgnoreCase()

Same as above but ignores case sensitivity.

    'abcdef'.notStartsWithIgnoreCase('abc') ==> false

    'ABCDEF'.notStartsWithIgnoreCase(?,'abc') ==> false

    notStartsWithIgnoreCase('ABCDEF','cde') ==> true

#### 158. endsWith()

Checks if a string ends with a suffix.

    'abcdef'.endsWith('def') ==> true

    'ABCDEF'.endsWith(?,'def') ==> false

    endsWith('ABCDEF','cde') ==> false

#### 159. endsWithIgnoreCase()

Same as above but ignores case sensitivity.

    'abcdef'.endsWithIgnoreCase('def') ==> true

    'ABCDEF'.endsWithIgnoreCase(?,'def') ==> true

    endsWithIgnoreCase('ABCDEF','cde') ==> false

#### 160. notEndsWith()

Checks if a string does not end with a suffix.

    'abcdef'.notEndsWith('def') ==> false

    'ABCDEF'.notEndsWith(?,'def') ==> true

    notEndsWith('ABCDEF','cde') ==> true

#### 161. notEndsWithIgnoreCase()

Same as above but ignores case sensitivity.

    'abcdef'.notEndsWithIgnoreCase('def') ==> false

    'ABCDEF'.notEndsWithIgnoreCase(?,'def') ==> false

    notEndsWithIgnoreCase('ABCDEF','cde') ==> true

#### 162. equals()

Checks if two values are equal.

    'abc'.equals('abc') ==> true

    'abc'.equals(?,' abc') ==> false

    equals('ABC','abc') ==> false

#### 163. equalsIgnoreCase()

Same as above but ignores case sensitivity.

    'abc'.equalsIgnoreCase('abc') ==> true

    'abc'.equalsIgnoreCase(?,' abc') ==> false

    equalsIgnoreCase('ABC','abc') ==> true

#### 164. notEquals()

Checks if two values are not equal.

    'abc'.notEquals('abc') ==> false

    'abc'.notEquals(?, ' abc') ==> true

    notEquals('ABC','abc') ==> true

#### 165. notEqualsIgnoreCase()

Same as above but ignores case sensitivity.

    'abc'.notEqualsIgnoreCase('abcd') ==> true

    'abc'.notEqualsIgnoreCase(' abc') ==> true

    notEqualsIgnoreCase('ABC','abc') ==> false

#### 166. matches()

Checks if a string matches a regular expression.

    '123a'.matches('^[0-9]+$') ==> false

    '784238'.matches(?,'^[0-9]+$') ==> true

    matches('63 56','^[0-9]+$') ==> false

#### 167. notMatches()

Checks if a string does not match a regex.

    '1234-123456'.notMatches('\\d{4}-\\d{6}') ==> false

    '888-123456'.notMatches(?,'\\d{4}-\\d{6}') ==> true

    notMatches('4444-5555','\\d{4}-\\d{6}') ==> true

#### 168. in()

Checks if a value exists in an array.

    56.in(12,34,56) ==> true

    '56'.in(12,34,56) ==> true

    'A'.in(json('["a","b","c"]')) ==> false

#### 169. inIgnoreCase()

Same as above but ignores case sensitivity.

    'A'.inIgnoreCase('a','b','c') ==> true

    'a '.inIgnoreCase('a','b','c') ==> false

#### 170. notIn()

Checks if a value does not exist in an array.

    56.notIn(12,34,56) ==> false

    '56'.notIn(12,34,56) ==> false

    'A'.notIn(json('["a","b","c"]')) ==> true

#### 171. notInIgnoreCase()

Same as above but ignores case sensitivity.

    'A'.notInIgnoreCase('a','b','c') ==> false

    'a '.notInIgnoreCase('a','b','c') ==> true

#### 172. isEmpty()

Checks if a value is empty.

    ''.isEmpty() ==> true

    isEmpty(' ') ==> false

    isEmpty(1) ==> false

    isEmpty(true) ==> false

    isEmpty(null) ==> true

    isEmpty(json('[""," ",0,false,null]')) ==> [ true, false, false, false, true ]

#### 173. isNotEmpty()

Checks if a value is not empty.

    ''.isNotEmpty() ==> false

    isNotEmpty(' ') ==> true

    isNotEmpty(1) ==> true

    isNotEmpty(true) ==> true

    isNotEmpty(null) ==> false

    isNotEmpty(json('[""," ",0,false,null]')) ==> [ false, true, true, true, false ]

#### 174. isBlank()

Checks if a value is whitespace, empty or null.

    ''.isBlank() ==> true

    isBlank(' ') ==> true

    isBlank(json('[""," ","X",0,false,null]')) ==> [ true, true, false, false, false, false ]

#### 175. isNotBlank()

Checks if a value is not blank.

    ''.isNotBlank() ==> false

    isNotBlank(' ') ==> false

    isNotBlank(json('[""," ","X",0,false,null]')) ==> [ false, false, true, false, false, false ]

#### 176. isNull()

Checks if a value is null.

    null.isNull() ==> true

    isNull(null) ==> true

    isNull('') ==> false

    isNull(json('["text",1,true,null]')) ==> [ false, false, false, true ]

#### 177. isNotNull()

Checks if a value is not null.

    null.isNotNull() ==> false

    isNotNull(null) ==> false

    isNotNull('') ==> true

    isNotNull(json('["text",1,true,null]')) ==> [ true, true, true, false ]

#### 178. isText()

Checks if a value is a text type.

    'text'.isText() ==> true

    isText(1) ==> false

    isText(true) ==> false

    isText(json('["text",1,true,null]')) ==> [ true, false, false, false ]

#### 179. isBoolean()

Checks if a value is a boolean.

    'text'.isBoolean() ==> false

    isBoolean(1) ==> false

    isBoolean(true) ==> true

    isBoolean(json('["text",1,true,null]')) ==> [ false, false, true, false ]

#### 180. isNumber()

Checks if a value is a number.

    'text'.isNumber() ==> false

    isNumber(1) ==> true

    isNumber(true) ==> false

    isNumber(json('["text",1,true,null]')) ==> [ false, true, false, false ]

#### 181. isEven()

Checks if a number is even.

    1.isEven() ==> false

    isEven(2) ==> true

    isEven(json('["text",1,2,null]')) ==> [ false, false, true, false ]

#### 182. isOdd()

Checks if a number is odd.

    1.isOdd() ==> true

    isOdd(2) ==> false

    isOdd(json('["text",1,2,null]')) ==> [ false, true, false, false ]

#### 183. isArray()

Checks if a value is an array.

    'text'.isArray() ==> false

    isArray(1) ==> false

    isArray(null) ==> false

    json('[1,2]').isArray() ==> true

    isArray(json('{"a":1}')) ==> false

#### 184. isObject()

Checks if a value is an object.

    'text'.isObject() ==> false

    isObject(1) ==> false

    isObject(null) ==> false

    json('[1,2]').isObject() ==> false

    isObject(json('{"a":1}')) ==> true

#### 185. isEmptyArray()

Checks if an array is empty.

    json('[]').isEmptyArray() ==> true

    isEmptyArray(json('[0]')) ==> false

#### 186. isEmptyObject()

Checks if an object is empty.

    json('{}').isEmptyObject() ==> true

    isEmptyObject(json('{"a":1}')) ==> false

#### 187. not()

Inverse a boolean value.

    true.not() ==> false

    not(false) ==> true

    not('false') ==> false

    not(0) ==> false

    not(null) ==> false

#### 188. isWeekday()

Checks if a local datetime is a weekday.

    '2021-12-31T00:00:00'.isWeekday() ==> true

    isWeekday('2022-01-01T00:00:00') ==> false

#### 189. isWeekend()

Checks if a local datetime is a Saturday or Sunday.

    '2021-12-31T00:00:00'.isWeekend() ==> false

    isWeekend('2022-01-01T00:00:00') ==> true

#### 190. isLeapYear()

Checks if a local datetime is a leap year.

    '2020-12-31T00:00:00'.isLeapYear() ==> true

    isLeapYear('2022-01-01T00:00:00') ==> false

### Array Functions

#### 191. size()

Returns the number of elements, including null value, in an array.

    json('[7,1,9,null,5,3]').size() ==> 6

    size(json('[7,1,9,null,5,3]')) ==> 6

#### 192. lastIndex()

Returns the index of the last element in an array.

    json('[7,1,9,null,5,3]').lastIndex() ==> 5

    lastIndex(json('[7,1,9,null,5,3]')) ==> 5

#### 193. indexOf()

Returns the first index of a specific element in an array.

    json('[1,1,3,5,null,3,7,3,9]').indexOf(3) ==> 2

    json('[1,1,3,5,null,3,7,3,9]').indexOf(?, '1') ==> 0

    indexOf(json('[1,1,3,5,null,3,7,3,9]'), null) ==> 4

#### 194. lastIndexOf()

Returns the last index of a specific element in an array.

    json('[1,1,3,5,null,3,7,3,9]').lastIndexOf(3) ==> 7

    json('[1,1,3,5,null,3,7,3,9]').lastIndexOf(?, '1') ==> 1

    lastIndexOf(json('[1,1,3,5,null,3,7,3,9]'), null) ==> 4

#### 195. first()

Returns the first element of an array.

    json('[7,1,9,null,5,3]').first() ==> 7

    first(json('[null,7,1,9,5,3]')) ==> null

#### 196. last()

Returns the last element of an array.

    json('[7,1,9,null,5,3]').last() ==> 3

    last(json('[7,1,9,5,3,null]')) ==> null

#### 197. max()

Returns the element with the maximum value.

    json('[7,1,9,null,5,3]').max() ==> 9

    max(json('[7,1,9,null,5,3]'), 15, 16) ==> 16

#### 198. min()

Returns the element with the minimum value.

    json('[7,1,9,null,5,3]').min() ==> 1

    min(json('[7,1,9,null,5,3]'), 15, 16) ==> 1

#### 199. topN()

Returns the first N elements when sorted in descending order.

    json('[7,1,9,null,5,3]').topN(2) ==> [ 9, 7 ]

    topN(json('[7,1,9,null,5,3]'), 6) ==> [ 9, 7, 5, 3, 1 ]

#### 200. bottomN()

Returns the first N elements when sorted in ascending order.

    json('[7,1,9,null,5,3]').bottomN(2) ==> [ 1, 3 ]

    bottomN(json('[7,1,9,null,5,3]'), 6) ==> [ 1, 3, 5, 7, 9 ]

#### 201. sum()

Returns the sum of all element values.

    json('[7,1,9,null,5,3]').sum() ==> 25.0

    sum(json('[7,1,9,null,5,3]'), 15, 16) ==> 56.0

#### 202. avg()

Returns the average of all element values.  Null value is not counted.

    json('[7,1,9,null,5,3]').avg() ==> 5.0

    avg(json('[7,1,9,null,5,3]'), 15, 16) ==> 8.0

#### 203. count()

Returns the count of non-null elements.

    json('[7,1,9,null,5,3]').count() ==> 5

    count(json('[7,1,9,null,5,3]'), 15, 16) ==> 7

#### 204. push()

Adds new elements to the end of an array.

    json('[7,1,9,null,5,3]').push(10,'X',json('[-1,-2]'),json('{"a":11,"b":12}'))
    ==>
    [ 7, 1, 9, null, 5, 3, 10, "X", [ -1, -2 ], {
      "a" : 11,
      "b" : 12
    } ]

#### 205. reverse()

Reverses the order of elements in an array.

    json('[7,1,9,null,5,3]').reverse() ==> [ 3, 5, null, 9, 1, 7 ]

    reverse(json('[7,1,9,null,5,3]')) ==> [ 3, 5, null, 9, 1, 7 ]

#### 206. slice()

Extracts a portion of an array.

    json('[1,2,3,4,5,6,7,8,9]').slice(3) ==> [ 4, 5, 6, 7, 8, 9 ]

    json('[1,2,3,4,5,6,7,8,9]').slice(2,8) ==> [ 3, 4, 5, 6, 7, 8 ]

    json('[1,2,3,4,5,6,7,8,9]').slice(,5) ==> [ 1, 2, 3, 4, 5 ]

    json('[1,2,3,4,5,6,7,8,9]').slice(-5) ==> [ 5, 6, 7, 8, 9 ]

    json('[1,2,3,4,5,6,7,8,9]').slice(?,1,8,2) ==> [ 2, 4, 6, 8 ]

    json('[1,2,3,4,5,6,7,8,9]').slice(?,2,,2) ==> [ 3, 5, 7, 9 ]

    slice(json('[1,2,3,4,5,6,7,8,9]'),6,2,1) ==> [ 7, 6, 5, 4 ]

    slice(json('[1,2,3,4,5,6,7,8,9]'),,,3) ==> [ 1, 4, 7 ]

    slice(json('[1,2,3,4,5,6,7,8,9]'),,-5,1) ==> [ 1, 2, 3, 4 ]

#### 207. sort()

Sorts the elements of an array.

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

#### 208. distinct()

Removes duplicate elements from an array.

    json('[1,1,3,5,3,7,3,9]').distinct().sort() ==> [ 1.0, 3.0, 5.0, 7.0, 9.0 ]

    distinct(json('["A","Z","a","Z","A","z"]')) ==> [ "A", "a", "Z", "z" ]

    distinct(json('["1","1.0",1,1.0,1.00,true,"true",null,"null"]')) ==> [ "1", "1.0", "null", "true", 1.0, true ]

#### 209. join()

Joins all element values into a string.

    json('["Hello", ",", "World", "!"]').join() ==> "Hello,World!"

    json('[1,2,3]').join('+') ==> "1+2+3"

    join(json('["A",1,"B","2.00","C",3.00,"D",true,null]'),'/') ==> "A/1/B/2.00/C/3.0/D/true"

#### 210. findAndModify()

Finds and modifies matching elements.

    json('[{"code":"A","price":8},{"code":"B","price":8},{"code":"C","price":3}]').findAndModify([code='C'],field(price:99))
    ==>
    [ {
      "code" : "A",
      "price" : 8
    }, {
      "code" : "B",
      "price" : 8
    }, {
      "code" : "C",
      "price" : 99
    } ]

    json('[{"code":"A","price":8},{"code":"B","price":8},{"code":"C","price":3}]').findAndModify([price=8],field(price:99),2)
    ==>
    [ {
      "code" : "A",
      "price" : 99
    }, {
      "code" : "B",
      "price" : 99
    }, {
      "code" : "C",
      "price" : 3
    } ]

#### 211. findByMax()

Finds the element with the maximum value for a field.

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

#### 212. findByMin()

Finds the element with the minimum value for a field.

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

#### 213. findByNullOrMax()

Finds the first element with the field value is null.  If not found, returns the element with the maximum value for a field.

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

#### 214. findByNullOrMin()

Finds the first element with the field value is null.  If not found, returns the element with the minimum value for a field.

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

#### 215. findByMaxOrNull()

Finds the element with the maximum value for a field.  If only null field value exists, returns the first element.

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

#### 216. findByMinOrNull()

Finds the element with the minimum value for a field.  If only null field value exists, returns the first element.

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

#### 217. entries()

Returns an array of key-value entries for an object.

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

#### 218. keys()

Returns an array of keys for an object.

    json('{"a":1,"b":[2,3],"c":{"d":4,"e":5}}').keys() ==> [ "a", "b", "c" ]

    json('{"a":1,"b":[2,3],"c":{"d":4,"e":5}}').keys(2) ==> [ "a", "b", "c", "d", "e" ]

    keys(json('{"a":1,"b":[2,3],"c":{"d":4,"e":5}}'), -1) ==> [ "a", "b", "c", "d", "e" ]

#### 219. depthLimit()

Limits the depth for objects in results.

    json('{"id":1,"array":[{"id":2,"obj":{"id":3,"array":[{"id":4.1},{"id":4.2}]}}]}').depthLimit(1)
    ==>
    {
      "id" : 1
    }

    json('{"id":1,"array":[{"id":2,"obj":{"id":3,"array":[{"id":4.1},{"id":4.2}]}}]}').depthLimit(2)
    ==>
    {
      "id" : 1,
      "array" : [ {
        "id" : 2
      } ]
    }

    depthLimit(json('{"id":1,"array":[{"id":2,"obj":{"id":3,"array":[{"id":4.1},{"id":4.2}]}}]}'),3)
    ==>
    {
      "id" : 1,
      "array" : [ {
        "id" : 2,
        "obj" : {
          "id" : 3
        }
      } ]
    }

    depthLimit(json('{"id":1,"array":[{"id":2,"obj":{"id":3,"array":[{"id":4.1},{"id":4.2}]}}]}'),4)
    ==>
    {
      "id" : 1,
      "array" : [ {
        "id" : 2,
        "obj" : {
          "id" : 3,
          "array" : [ {
            "id" : 4.1
          }, {
            "id" : 4.2
          } ]
        }
      } ]
    }

#### 220. collect()

Collects values into a single array.

    'Hi'.collect(1,?,true,json('[{"a":1,"x":11},{"b":2,"y":12}]'),json('{"c":3,"x":13}'))
    ==>
    [ 1, "Hi", true, [ {
      "a" : 1,
      "x" : 11
    }, {
      "b" : 2,
      "y" : 12
    } ], {
      "c" : 3,
      "x" : 13
    } ]

#### 221. cumulateCollect()

Collects values across nested structures.
The 1st parameter is a query to evaluate a result that will be collected into an array.
The 2nd parameter is a query to evaluate the next dataset that loop back for the 1st parameter evaluation again.
The operation loop will be stopped when the next dataset is null.

    json('{"id":1,"val":11,"item":{"id":2,"val":22,"item":{"id":3,"val":33,"item":{"id":4,"val":44}}}}')
    .cumulateCollect(map(id,val.calc(?*2)), item)
    ==>
    [ {
      "id" : 1,
      "val" : 22.0
    }, {
      "id" : 2,
      "val" : 44.0
    }, {
      "id" : 3,
      "val" : 66.0
    }, {
      "id" : 4,
      "val" : 88.0
    } ]

#### 222. wrap()

Wraps a value into an array.

    json('["Hi"]').wrap() ==> [ [ "Hi" ] ]

    wrap(json('{"a":1}'))
    ==>
    [ {
      "a" : 1
    } ]

#### 223. toArray()

Converts values to a flat array.

    json('{"a":1,"b":[2,3],"c":{"d":4,"e":5}}').toArray()
    ==>
    [ 1, [ 2, 3 ], {
      "d" : 4,
      "e" : 5
    } ]

    json('{"a":1,"b":[2,3],"c":{"d":4,"e":5}}').toArray(c) ==> [ 4, 5 ]

    toArray(json('{"a":1,"b":[2,3],"c":{"d":4,"e":5}}').toArray()) ==> [ 1, 2, 3, 4, 5 ]

#### 224. toObject()

Converts values to an object with named properties.

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

#### 225. mergeArrays()

Merges nested arrays into a single array.

    json('[[1,2],[3,4],[5,6]]').mergeArrays(?) ==> [ 1, 2, 3, 4, 5, 6 ]

    json('[{"a":[1,2]},{"a":[3,4]},{"a":[5,6]}]').mergeArrays(a) ==> [ 1, 2, 3, 4, 5, 6 ]

    json('{"a":[1,2],"b":[3,4],"c":[5,6]}').mergeArrays(a,b,c) ==> [ 1, 2, 3, 4, 5, 6 ]

#### 226. mergeObjects()

Merges objects by replacing common keys.

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

#### 227. flatten()

Flattens nested objects/arrays into a flat structure.

    json('[[[[1,2],[3,4]],[[5,6],[7,8]]],[[[9,10],[11,12]],[[13,14],[15,16]]]]').flatten(1)
    ==>
    [ [ [ 1, 2 ], [ 3, 4 ] ], [ [ 5, 6 ], [ 7, 8 ] ], [ [ 9, 10 ], [ 11, 12 ] ], [ [ 13, 14 ], [ 15, 16 ] ] ]

    json('[[[[1,2],[3,4]],[[5,6],[7,8]]],[[[9,10],[11,12]],[[13,14],[15,16]]]]').flatten(2)
    ==>
    [ [ 1, 2 ], [ 3, 4 ], [ 5, 6 ], [ 7, 8 ], [ 9, 10 ], [ 11, 12 ], [ 13, 14 ], [ 15, 16 ] ]

    json('[[[[1,2],[3,4]],[[5,6],[7,8]]],[[[9,10],[11,12]],[[13,14],[15,16]]]]').flatten()
    ==>
    [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 ]

    flatten(json('[[[[1,2],[3,4]],[[5,6],[7,8]]],[[[9,10],[11,12]],[[13,14],[15,16]]]]'), 3, null)
    ==>
    [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 ]

    flatten(json('{"a":1,"b":[2,3],"c":{"d":4,"e":{"f":5}}}'), '_', null)
    ==>
    {
      "a" : 1,
      "b_0" : 2,
      "b_1" : 3,
      "c_d" : 4,
      "c_e_f" : 5
    }

    json('[0,1,[2,3,[4,{"a":5},6,[7]],8],9]').flatten('_')
    ==>
    {
      "0" : 0,
      "1" : 1,
      "2_0" : 2,
      "2_1" : 3,
      "2_2_0" : 4,
      "2_2_1_a" : 5,
      "2_2_2" : 6,
      "2_2_3_0" : 7,
      "2_3" : 8,
      "3" : 9
    }

    flatten(json('{"a":1,"b":[2,3],"c":{"d":4,"e":{"f":5}}}'), '.', '[%d]')
    ==>
    {
      "a" : 1,
      "b[0]" : 2,
      "b[1]" : 3,
      "c.d" : 4,
      "c.e.f" : 5
    }

    json('[0,1,[2,3,[4,{"a":5},6,[7]],8],9]').flatten('.', '[%d]')
    ==>
    {
      "[0]" : 0,
      "[1]" : 1,
      "[2][0]" : 2,
      "[2][1]" : 3,
      "[2][2][0]" : 4,
      "[2][2][1].a" : 5,
      "[2][2][2]" : 6,
      "[2][2][3][0]" : 7,
      "[2][3]" : 8,
      "[3]" : 9
    }

#### 228. unflatten()

Reverses a flatten operation on objects/arrays.

    flatten(json('{"a":1,"b":[2,3],"c":{"d":4,"e":{"f":5}}}'),'_',null).unflatten('_')
    ==>
    {
      "a" : 1,
      "b" : [ 2, 3 ],
      "c" : {
        "d" : 4,
        "e" : {
          "f" : 5
        }
      }
    }

    json('[0,1,[2,3,[4,{"a":5},6,[7]],8],9]').flatten('_').unflatten('_')
    ==>
    [ 0, 1, [ 2, 3, [ 4, {"a":5}, 6, [ 7 ] ], 8 ], 9 ]

    flatten(json('{"a":1,"b":[2,3],"c":{"d":4,"e":{"f":5}}}'),'.','[%d]').unflatten('.[]')
    ==>
    {
      "a" : 1,
      "b" : [ 2, 3 ],
      "c" : {
        "d" : 4,
        "e" : {
          "f" : 5
        }
      }
    }

    json('[0,1,[2,3,[4,{"a":5},6,[7]],8],9]').flatten('.','[%d]').unflatten('.[]')
    ==>
    [ 0, 1, [ 2, 3, [ 4, {"a":5}, 6, [ 7 ] ], 8 ], 9 ]

#### 229. map()

Projects each element into a new object.

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

#### 230. field()

Add, modify or delete element in an object.

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

#### 231. group()

Groups elements into an object by a field.

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

#### 232. unwind()

Unwinds grouped elements back into an array.

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

#### 233. assort()

Separates elements based on conditional assorting.

    json('{"xy1": 1,"xy2": 2,"ab1": 3,"ab2": 4,"ab3": 5,"zz1": 6,"xy3": 7,"zz2": 9,"zz3": {"k":10}}}').assort(*.[isEven()], ~'xy.*', ~'ab.*', ??)
    ==>
    [ {
      "xy2" : 2,
      "ab2" : 4,
      "zz1" : 6
    }, {
      "xy1" : 1,
      "xy3" : 7
    }, {
      "ab1" : 3,
      "ab3" : 5
    }, {
      "zz2" : 9
    }, {
      "zz3" : {
        "k" : 10
      }
    } ]

    json('[1,2,3,4,5,6,7,8,9,10,11,12]').assort([?<5], [isEven()], [?<9], ?)
    ==>
    [ [ 1, 2, 3, 4 ], [ 6, 8, 10, 12 ], [ 5, 7 ], [ 9, 11 ] ]

### Programmable Functions

#### 234. eval()

Evaluate a string value of a field as a Josson expression.

    json('{"a":1,"b":2,"statement":"calc(a+b*2)"}').eval(statement) ==> 5.0

    json('[{"a":3,"s":"calc(a*2)"},{"a":4,"s":"calc(a*2)"}]')@.eval(s) ==> [ 6.0, 8.0 ]

#### 235. json()

Parses a string as JSON.

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


#### 236. if()

Conditionally returns one value if expression is true, otherwise another value.

    json('{"a":1,"b":2,"c":3}').if(a.isEven(), 'T', 'F') ==> "F"

    json('{"a":1,"b":2,"c":3}').if([a=1], 'T', 'F') ==> "T"

    json('{"a":1,"b":2,"c":3}').if([a=1 & b=3], 'T', 'F') ==> "F"

    json('{"a":1,"b":2,"c":3}').if([a=1 & b=3], 'T') ==> !unresolvable!

    json('{"a":1,"b":2,"c":3}').if([a=b], 'T', if([c=3], 'C', 'F')) ==> "C"

    json('[1,2,3,4,5]').if(isOdd(), calc(?*2), ?) ==> [ 2.0, 2, 6.0, 4, 10.0 ]

#### 237. ifNot()

Reverse of if(), returns value if expression is false.

    json('{"a":1,"b":2,"c":3}').ifNot(a.isEven(), 'T', 'F') ==> "T"

    json('{"a":1,"b":2,"c":3}').ifNot([a=1 & b=3], 'T') ==> "T"

    json('{"a":1,"b":2,"c":3}').ifNot([a=b], 'T', if([c=3], 'C', 'F')) ==> "T"

    json('[1,2,3,4,5]').ifNot(isOdd(), calc(?*2), ?) ==> [ 1, 4.0, 3, 8.0, 5 ]

#### 238. coalesce()

Returns first non-null value from a list of values.

    json('["abc","",123,false,null]').coalesce('xyz') ==> [ "abc", "", 123, false, "xyz" ]

    json('{"a":null,"c":"abc"}').coalesce(a,b,c,'xyz') ==> "abc"

#### 239. caseValue()

Returns value matching case, else default value.

    'a'.caseValue('A',1,'b',2,'a',3,4) ==> 3

    'z'.caseValue('A',1,'b',2,'a',3,4) ==> 4

    'z'.caseValue('A',1,'b',2,'a',3) ==> !unresolvable!

    json('[{"s":1},{"s":null},{"s":3}]').s.caseValue(1,'A',null,'B') ==> [ "A", "B", null ]

#### 240. caseValueIgnoreCase()

Same as above but ignores case sensitivity.

    'a'.caseValue('A',1,'b',2,'a',3,4) ==> 1

    'z'.caseValue('A',1,'b',2,'a',3,4) ==> 4

    'z'.caseValue('A',1,'b',2,'a',3) ==> !unresolvable!

#### 241. indexedValue()

Returns value at an index from an array or list of parameters.

    0.indexedValue('a','b','c','d') ==> "a"

    1.indexedValue(json('["a","b","c","d"]')) ==> "b"

    '3'.indexedValue('a','b','c','d') ==> "d"

    4.indexedValue('a','b','c','d') ==> !unresolvable!

    -1.indexedValue('a','b','c','d') ==> !unresolvable!

#### 242. cycleValue()

Returns value cycling through an array or list of parameters circularly.

    0.cycleValue('a','b','c','d') ==> "a"

    1.cycleValue(json('["a","b","c","d"]')) ==> "b"

    '3'.cycleValue('a','b','c','d') ==> "d"

    4.cycleValue('a','b','c','d') ==> "a"

    -1.cycleValue('a','b','c','d') ==> "d"

    -6.cycleValue('a','b','c','d') ==> "c"

#### 243. steps()

Returns the number of path steps processed.

    json('{"a":{"b":{"c":1}}}').a.steps() ==> 2

    json('{"a":{"b":{"c":1}}}').a.b.steps() ==> 3

    json('{"a":{"b":{"c":1}}}').*().c.steps() ==> 4

    json('{"a":{"b":{"c":1}}}').a.b.calc(c+1).steps() ==> 4

    json('{"a":{"b":{"c":1}}}').a.b.c.calc(?+1).steps() ==> 5

#### 244. get()

Returns the evaluated value of a path based on the current node.

    json('{"decode":[{"code":"A","color":"Red"},{"code":"B","color":"Blue"}],"data":["B","A","B"]}')
    .data@.let($code:?).get(...decode[code=$code].color)
    ==>
    [ "Blue", "Red", "Blue" ]

#### 245. let()

Defines variables for the forthcoming path steps.
This function does not count as a step.

    json('{"a":1,"b":2}').let($x:a, $y:calc(a+b), $z:concat(a,b)).map($x,$y,$z)
    ==>
    {
      "$x" : 1,
      "$y" : 3.0,
      "$z" : "12"
    }

#### 246. mergeArraysOption()

Set the merge arrays method. Available options are the following, case insensitive:
- Append - appends 2nd array elements to the end of the 1st array.
- Integrate - appends 2nd array elements to the end of the 1st array if the element does not exist.
- ReplaceWhole - replaces the whole array by the 2nd array.
- ReplaceByIndex - replaces elements by array index position.

This function does not count as a step.

    json('[[1,2,3,4,5,6],[8,6,4,2]]').mergeArraysOption(Append).mergeArrays() ==> [ 1, 2, 3, 4, 5, 6, 8, 6, 4, 2 ]

    json('[[1,2,3,4,5,6],[8,6,4,2]]').mergeArraysOption(Integrate).mergeArrays() ==> [ 1, 2, 3, 4, 5, 6, 8 ]

    json('[[1,2,3,4,5,6],[8,6,4,2]]').mergeArraysOption(ReplaceWhole).mergeArrays() ==> [ 8, 6, 4, 2 ]

    json('[[1,2,3,4,5,6],[8,6,4,2]]').mergeArraysOption(ReplaceByIndex).mergeArrays() ==> [ 8, 6, 4, 2, 5, 6 ]

    json('[[8,6,4,2],[1,2,3,4,5,6]]').mergeArraysOption(Append).mergeArrays() ==> [ 8, 6, 4, 2, 1, 2, 3, 4, 5, 6 ]

    json('[[8,6,4,2],[1,2,3,4,5,6]]').mergeArraysOption(Integrate).mergeArrays() ==> [ 8, 6, 4, 2, 1, 3, 5 ]

    json('[[8,6,4,2],[1,2,3,4,5,6]]').mergeArraysOption(ReplaceWhole).mergeArrays() ==> [ 1, 2, 3, 4, 5, 6 ]

    json('[[8,6,4,2],[1,2,3,4,5,6]]').mergeArraysOption(ReplaceByIndex).mergeArrays() ==> [ 1, 2, 3, 4, 5, 6 ]

    json('[{\"a\":[1,2,3,4,5,6],\"b\":\"1st\"},{\"a\":[8,6,4,2],\"b\":\"2nd\"}]').mergeArraysOption(Append).mergeObjects()
    ==>
    {
      "a" : [ 1, 2, 3, 4, 5, 6, 8, 6, 4, 2 ],
      "b" : "2nd"
    }

    json('[{\"a\":[1,2,3,4,5,6],\"b\":\"1st\"},{\"a\":[8,6,4,2],\"b\":\"2nd\"}]').mergeArraysOption(Integrate).mergeObjects()
    ==>
    {
      "a" : [ 1, 2, 3, 4, 5, 6, 8 ],
      "b" : "2nd"
    }

    json('[{\"a\":[1,2,3,4,5,6],\"b\":\"1st\"},{\"a\":[8,6,4,2],\"b\":\"2nd\"}]').mergeArraysOption(ReplaceWhole).mergeObjects()
    ==>
    {
      "a" : [ 8, 6, 4, 2 ],
      "b" : "2nd"
    }

    json('[{\"a\":[1,2,3,4,5,6],\"b\":\"1st\"},{\"a\":[8,6,4,2],\"b\":\"2nd\"}]').mergeArraysOption(ReplaceByIndex).mergeObjects()
    ==>
    {
      "a" : [ 8, 6, 4, 2, 5, 6 ],
      "b" : "2nd"
    }

    json('[{\"a\":[8,6,4,2],\"b\":\"2nd\"},{\"a\":[1,2,3,4,5,6],\"b\":\"1st\"}]').mergeArraysOption(Append).mergeObjects()
    ==>
    {
      "a" : [ 8, 6, 4, 2, 1, 2, 3, 4, 5, 6 ],
      "b" : "1st"
    }

    json('[{\"a\":[8,6,4,2],\"b\":\"2nd\"},{\"a\":[1,2,3,4,5,6],\"b\":\"1st\"}]').mergeArraysOption(Integrate).mergeObjects()
    ==>
    {
      "a" : [ 8, 6, 4, 2, 1, 3, 5 ],
      "b" : "1st"
    }

    json('[{\"a\":[8,6,4,2],\"b\":\"2nd\"},{\"a\":[1,2,3,4,5,6],\"b\":\"1st\"}]').mergeArraysOption(ReplaceWhole).mergeObjects()
    ==>
    {
      "a" : [ 1, 2, 3, 4, 5, 6 ],
      "b" : "1st"
    }

    json('[{\"a\":[8,6,4,2],\"b\":\"2nd\"},{\"a\":[1,2,3,4,5,6],\"b\":\"1st\"}]').mergeArraysOption(ReplaceByIndex).mergeObjects()
    ==>
    {
      "a" : [ 1, 2, 3, 4, 5, 6 ],
      "b" : "1st"
    }

---

## Custom Function

You can define your own custom functions to manipulate a node in a Josson query.
Customer function name must starts with "$" and ends with "()".
There are 2 types of custom function:

1. Accept the current node as parameter and return a manipulated node of type JsonNode.
2. Accept the current node and array index as parameters and return a manipulated node of type JsonNode.

__Example__

- $randomUuid() - return a random UUID
- $addIndex() - add the array index value to the `base` integer value


    Josson josson = Josson.fromJsonString(
            "[" +
            "    {\"base\": 2}," +
            "    {\"base\": 3}," +
            "    {\"base\": 5}" +
            "]"
        )
        .customFunction(
            "$randomUuid()",
            (node) -> TextNode.valueOf(UUID.randomUUID().toString())
        )
        .customFunction(
            "$addIndex()",
            (node, index) -> IntNode.valueOf(node.get("base").asInt() + index)
        );
    JsonNode node = josson.getNode("field(id:$randomUuid(), base+index:$addIndex())");
    System.out.println(node.toPrettyString());

__Output__

    [ {
      "base" : 2,
      "id" : "9dbb0ed2-83cb-4faa-9920-b02f4cb1bddb",
      "base+index" : 2
    }, {
      "base" : 3,
      "id" : "5ad685bb-06f4-4574-8e1f-a9fb4a8609cb",
      "base+index" : 4
    }, {
      "base" : 5,
      "id" : "1c499b97-2b9c-494f-b935-0c97c7401890",
      "base+index" : 7
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
    {{boolean ? trueValue ⟬: boolean ? trueValue⟭ⁿ : falseValue}}

If all conditions are evaluated to be false and `falseValue` is not given, it returns an empty string.

    {{boolean ? trueValue}}
    {{boolean ? trueValue : boolean ? trueValue}}
    {{boolean ? trueValue ⟬: boolean ? trueValue⟭ⁿ }}

Syntax `?:` is much like `coalesce()` but the checking conditions of `valueAsText` are unresolvable and empty string in addition to null node.

    {{valueAsText ?: valueAsText}}
    {{valueAsText ⟬?: valueAsText⟭ⁿ }}

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
  If one is object and the other is array, the object will be put into an array before manipulation.

      "leftQuery <+< rightQuery"

- _Right Concatenate_ `>+>`

  Concatenate left into right. Works on two objects or two arrays.
  If one is object and the other is array, the object will be put into an array before manipulation.

      "leftQuery >+> rightQuery"

- _Subtract Right From Left_ `<-<`

  Set of elements in the left set that are not in the right set. Works on two objects or two arrays.
  If one is object and the other is array, the object will be put into an array before manipulation.

      "leftQuery <-< rightQuery"

- _Subtract Left From Right_ `>->`

  Set of elements in the right set that are not in the left set. Works on two objects or two arrays.
  If one is object and the other is array, the object will be put into an array before manipulation.

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

       order → delivery{} → address → split(?) → [""] → [concat(?) ⇒ ""] → join(?[]) ⇒ ""

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

- Parse a JSON string of an object or array directly.

      "{\"B00001\":2, \"A00308\":1, \"A00201\":1}"       // ObjectNode
      "[{\"B00001\":2}, {\"A00308\":1}, {\"A00201\":1}]" // ArrayNode

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
    Round 2 : Matched stocks to data query []?{ignoredQuery}
    Round 2 : Resolved stocks to Array with 5 elements
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

---

### Examples from Stack Overflow Question

#### 1. How to rearrange json objects within json structure itself? Java

(55934043)

    [{
        "file": [{
                "fileRefNo": "AG/CSD/1",
                "status": "Active"
            }],
        "requestNo": "225V49"
    }, {
        "file": [{
                "fileRefNo": "AG/CSD/1",
                "status": "Inactive"
            }],
        "requestNo": "225SRV"
    }, {
        "file": [{
                "fileRefNo": "AG/CSD/2",
                "status": "Active"
            }],
        "requestNo": "225SRV"
    }]

Josson Query

    group(requestNo, file).field(file.flatten(1))

Output

    [ {
      "requestNo" : "225V49",
      "file" : [ {
        "fileRefNo" : "AG/CSD/1",
        "status" : "Active"
      } ]
    }, {
      "requestNo" : "225SRV",
      "file" : [ {
        "fileRefNo" : "AG/CSD/1",
        "status" : "Inactive"
      }, {
        "fileRefNo" : "AG/CSD/2",
        "status" : "Active"
      } ]
    } ]

---

#### 2. Parse JSON to Java object with dynamics objects in JSON-response

(73362526)

    {
        "success": 1,
        "results": [
            {
                "FI": "120986750",
                "event_id": "5164306",
                "cards": {
                    "updated_at": "1660559432",
                    "key": "AAA100",
                    "sp": {
                        "cards": {
                            "id": "1",
                            "name": "Cards",
                            "odds": [
                                {
                                    "id": "101",
                                    "odds": "2.200",
                                    "header": "Over",
                                    "name": "11"
                                },
                                {
                                    "id": "102",
                                    "odds": "8.500",
                                    "header": "Exactly",
                                    "name": "11"
                                },
                                {
                                    "id": "103",
                                    "odds": "1.909",
                                    "header": "Under",
                                    "name": "11"
                                }
                            ]
                        }
                    }
                },
                "corners": {
                    "updated_at": "1660559431",
                    "key": "AAA200",
                    "sp": {
                        "corners": {
                            "id": "2",
                            "name": "Corners",
                            "odds": [
                                {
                                    "id": "201",
                                    "odds": "2.200",
                                    "header": "Over",
                                    "name": "10"
                                },
                                {
                                    "id": "202",
                                    "odds": "8.500",
                                    "header": "Exactly",
                                    "name": "10"
                                },
                                {
                                    "id": "203",
                                    "odds": "1.909",
                                    "header": "Under",
                                    "name": "10"
                                }
                            ]
                        },
                        "total_corners": {
                            "id": "3",
                            "name": "Total Corners",
                            "odds": [
                                {
                                    "id": "204",
                                    "odds": "17.000",
                                    "name": "Under 6"
                                },
                                {
                                    "id": "205",
                                    "odds": "4.333",
                                    "name": "6 - 8"
                                },
                                {
                                    "id": "206",
                                    "odds": "2.750",
                                    "name": "9 - 11"
                                },
                                {
                                    "id": "207",
                                    "odds": "3.400",
                                    "name": "12 - 14"
                                },
                                {
                                    "id": "208",
                                    "odds": "5.500",
                                    "name": "Over 14"
                                }
                            ]
                        }
                    }
                }
            }
        ]
    }

Josson Query

    results.map(
        FI,
        event_id,
        categories: entries().[value.isObject()]*
            .map(category: key,
                 value.updated_at,
                 value.key,
                 details: value.sp.**
                     .map(market_id: id,
                          market: name,
                          odds)
                     .unwind(odds)
            ).unwind(details)
    ).unwind(categories)

Output

    [ {
      "FI" : "120986750",
      "event_id" : "5164306",
      "category" : "cards",
      "updated_at" : "1660559432",
      "key" : "AAA100",
      "market_id" : "1",
      "market" : "Cards",
      "id" : "101",
      "odds" : "2.200",
      "header" : "Over",
      "name" : "11"
    }, {
      "FI" : "120986750",
      "event_id" : "5164306",
      "category" : "cards",
      "updated_at" : "1660559432",
      "key" : "AAA100",
      "market_id" : "1",
      "market" : "Cards",
      "id" : "102",
      "odds" : "8.500",
      "header" : "Exactly",
      "name" : "11"
    }, {
      "FI" : "120986750",
      "event_id" : "5164306",
      "category" : "cards",
      "updated_at" : "1660559432",
      "key" : "AAA100",
      "market_id" : "1",
      "market" : "Cards",
      "id" : "103",
      "odds" : "1.909",
      "header" : "Under",
      "name" : "11"
    }, {
      "FI" : "120986750",
      "event_id" : "5164306",
      "category" : "corners",
      "updated_at" : "1660559431",
      "key" : "AAA200",
      "market_id" : "2",
      "market" : "Corners",
      "id" : "201",
      "odds" : "2.200",
      "header" : "Over",
      "name" : "10"
    }, {
      "FI" : "120986750",
      "event_id" : "5164306",
      "category" : "corners",
      "updated_at" : "1660559431",
      "key" : "AAA200",
      "market_id" : "2",
      "market" : "Corners",
      "id" : "202",
      "odds" : "8.500",
      "header" : "Exactly",
      "name" : "10"
    }, {
      "FI" : "120986750",
      "event_id" : "5164306",
      "category" : "corners",
      "updated_at" : "1660559431",
      "key" : "AAA200",
      "market_id" : "2",
      "market" : "Corners",
      "id" : "203",
      "odds" : "1.909",
      "header" : "Under",
      "name" : "10"
    }, {
      "FI" : "120986750",
      "event_id" : "5164306",
      "category" : "corners",
      "updated_at" : "1660559431",
      "key" : "AAA200",
      "market_id" : "3",
      "market" : "Total Corners",
      "id" : "204",
      "odds" : "17.000",
      "name" : "Under 6"
    }, {
      "FI" : "120986750",
      "event_id" : "5164306",
      "category" : "corners",
      "updated_at" : "1660559431",
      "key" : "AAA200",
      "market_id" : "3",
      "market" : "Total Corners",
      "id" : "205",
      "odds" : "4.333",
      "name" : "6 - 8"
    }, {
      "FI" : "120986750",
      "event_id" : "5164306",
      "category" : "corners",
      "updated_at" : "1660559431",
      "key" : "AAA200",
      "market_id" : "3",
      "market" : "Total Corners",
      "id" : "206",
      "odds" : "2.750",
      "name" : "9 - 11"
    }, {
      "FI" : "120986750",
      "event_id" : "5164306",
      "category" : "corners",
      "updated_at" : "1660559431",
      "key" : "AAA200",
      "market_id" : "3",
      "market" : "Total Corners",
      "id" : "207",
      "odds" : "3.400",
      "name" : "12 - 14"
    }, {
      "FI" : "120986750",
      "event_id" : "5164306",
      "category" : "corners",
      "updated_at" : "1660559431",
      "key" : "AAA200",
      "market_id" : "3",
      "market" : "Total Corners",
      "id" : "208",
      "odds" : "5.500",
      "name" : "Over 14"
    } ]

---

#### 3. How to compare two JSON responses in java irrespective of JSON array sequence?

(73025233)

Dataset `api1`

    {
        "name": "name1",
        "address": "",
        "skillset": [
            {
                "lang": "java",
                "projectName": "project1"
            },
            {
                "lang": "c++",
                "projectName": "project2"
            }
        ]
    }

Dataset `api2`

    {
        "name": "name1",
        "address": "",
        "skillset": [
            {
                "lang": "c++",
                "projectName": "project2"
            },
            {
                "lang": "java",
                "projectName": "project1"
            }
        ]
    }

Jossons Query

    api1 = api2

Output

    true

---

#### 4. Splitting json object

(73674131)

    {
       "idno":6473853,
       "user":"GCA_GB",
       "operation":"U",
       "timestamp":"2022-08-22T13:14:48",
       "first_name":{
          "old":"rak",
          "new":"raki"
       },
       "fam_name":{
          "old":"gow",
          "new":"gowda"
       }
    }

Josson Query

    map(idno, user, operation, timestamp,
        changes: entries().[value.isObject()]*.map(key::value))
    .unwind(changes)

Output

    [ {
      "idno" : 6473853,
      "user" : "GCA_GB",
      "operation" : "U",
      "timestamp" : "2022-08-22T13:14:48",
      "first_name" : {
        "old" : "rak",
        "new" : "raki"
      }
    }, {
      "idno" : 6473853,
      "user" : "GCA_GB",
      "operation" : "U",
      "timestamp" : "2022-08-22T13:14:48",
      "fam_name" : {
        "old" : "gow",
        "new" : "gowda"
      }
    } ]

---

#### 5. Java - Need to convert Flat Map<String, Object> to Nested Object

(73456238)

    {
        "test": "t",
        "testInt": 1,
        "b": {
            "testDouble": 1.1,
            "c": [{
                "testFloat": 1.2
            }]
        }
    }

Josson Query

    flatten('_')

Output `flattened `

    {
      "test" : "t",
      "testInt" : 1,
      "b_testDouble" : 1.1,
      "b_c_0_testFloat" : 1.2
    }

Josson Query

    unflatten('_')

Output `unflatten`

    {
      "test" : "t",
      "testInt" : 1,
      "b" : {
        "testDouble" : 1.1,
        "c" : [ {
          "testFloat" : 1.2
        } ]
      }
    }

---

#### 6. Filter by inner value of a property (not the name)

(73598200)

    {
      "id": 11,
      "item": [
        {
          "id": "11_1",
          "action": "add",
          "type": {
            "id": "11_1_xx",
            "typeName": "xx"
          },
          "item": [
            {
              "id": "11_1_1",
              "action": "add",
              "type": {
                "id": "11_1_1_zz",
                "typeName": "zz"
              },
              "item": [
                {
                  "id": "11_1_1_1",
                  "action": "add",
                  "type": {
                    "id": "11_1_1_1_xx",
                    "typeName": "xx"
                  }
                }
              ]
            },
            {
              "id": "11_1_2",
              "action": "add",
              "type": {
                "id": "11_1_2_xx",
                "typeName": "xx"
              },
              "item": [
                {
                  "id": "11_1_2_1",
                  "action": "add",
                  "type": {
                    "id": "11_1_2_1_zz",
                    "typeName": "zz"
                  }
                }
              ]
            }
          ]
        }
      ]
    }

Josson Query

    cumulateCollect(item[type.typeName='xx']*.field(item:), item).flatten(1)

Output

    [ {
      "id" : "11_1",
      "action" : "add",
      "type" : {
        "id" : "11_1_xx",
        "typeName" : "xx"
      }
    }, {
      "id" : "11_1_2",
      "action" : "add",
      "type" : {
        "id" : "11_1_2_xx",
        "typeName" : "xx"
      }
    }, {
      "id" : "11_1_1_1",
      "action" : "add",
      "type" : {
        "id" : "11_1_1_1_xx",
        "typeName" : "xx"
      }
    } ]

---

#### 7. Change output name

(72426983)

    {
        "userId": "1",
        "age": "20",
        "firstName": "firstname1",
        "lastname": "lastname1",
        "zipCode": "zipcode1",
        "street": "street1",
        "city": "city1",
        "country": "country",
        "gender": "gender1",
        "grade": "grade1",
        "birthday": "birthday1"
    }

Josson Query

    map(ID:userId, age, firstName, lastname,
        address:collect(
                  map(code:'custom-field1',value:zipCode),
                  map(code:'custom-field2',value:street),
                  map(code:'custom-field3',value:city),
                  map(code:'custom-field4',value:country)),
        gender, grade, birthday)

Output

    {
      "ID" : "1",
      "age" : "20",
      "firstName" : "firstname1",
      "lastname" : "lastname1",
      "address" : [ {
        "code" : "custom-field1",
        "value" : "zipcode1"
      }, {
        "code" : "custom-field2",
        "value" : "street1"
      }, {
        "code" : "custom-field3",
        "value" : "city1"
      }, {
        "code" : "custom-field4",
        "value" : "country"
      } ],
      "gender" : "gender1",
      "grade" : "grade1",
      "birthday" : "birthday1"
    }

---

#### 8. Transform If Else Condition on keys?

(72701610)

    {
      "treasure": [
        {
          "aname": "FOO",
          "bname": "BAR"
        }
      ]
    }

Josson Query

    treasure
    .entries()
    .map(if([key='aname'], if([value='FOO'],'fname',key), 'sname')::value)
    .mergeObjects()

Output

    {
      "fname" : "FOO",
      "sname" : "BAR"
    }

---

#### 9. Refer mapping value of others array base on "id"

(73945727)

    {
      "status": [
        {
          "id": "online",
          "state": "valid"
        },
        {
          "id": "busy",
          "state": "unknown"
        },
        {
          "id": "any",
          "state": "unknow",
          "moreInfo": "unavailable"
        }
      ],
      "users": [
        {
          "title": "foo",
          "availability": [
            "online",
            "busy"
          ]
        },
        {
          "title": "bar",
          "availability": [
            "busy",
            "any"
          ]
        },
        {
          "title": "baz",
          "availability": [
            "any"
          ]
        }
      ]
    }

Josson Query

    map(users.field(availability@.let($id:?).get($.status[id=$id])))

Output

    {
      "users" : [ {
        "title" : "foo",
        "availability" : [ {
          "id" : "online",
          "state" : "valid"
        }, {
          "id" : "busy",
          "state" : "unknown"
        } ]
      }, {
        "title" : "bar",
        "availability" : [ {
          "id" : "busy",
          "state" : "unknown"
        }, {
          "id" : "any",
          "state" : "unknow",
          "moreInfo" : "unavailable"
        } ]
      }, {
        "title" : "baz",
        "availability" : [ {
          "id" : "any",
          "state" : "unknow",
          "moreInfo" : "unavailable"
        } ]
      } ]
    }

---

#### 10. Spring query convert to a nested JSON structure

(73616066)

    [
        {
            "names": "Car",
            "values": "Toyota"
        },
        {
            "names": "Bike",
            "values": "Schwinn"
        },
        {
            "names": "Scooter",
            "values": "Razor"
        },
        {
            "names": "A0",
            "values": "11"
        },
        {
            "names": "A1",
            "values": "12"
        },
        {
            "names": "A2",
            "values": "13"
        },
        {
            "names": "B0",
            "values": "2000"
        },
        {
            "names": "B1",
            "values": "4000"
        },
        {
            "names": "B2",
            "values": "22000"
        }
    ]

Josson Query

    @collect([names !=~ '\D\d+']*
             .map(names::values)
            ,[names =~ '\D\d+']*
             .group(names.substr(1), map(names::values))@
             .elements
             .mergeObjects()
             .@toObject('Data')
    )
    .flatten(1)
    .mergeObjects()

Output

    {
      "Car" : "Toyota",
      "Bike" : "Schwinn",
      "Scooter" : "Razor",
      "Data" : [ {
        "A0" : "11",
        "B0" : "2000"
      }, {
        "A1" : "12",
        "B1" : "4000"
      }, {
        "A2" : "13",
        "B2" : "22000"
      } ]
    }

---

#### 11. How to merge two JSON string

(72272928)

Dataset `resp1`

    {
        "data": {
            "values": {
                "name": "kiran",
                "age": "24"
            }
        }
    }

Dataset `resp2`

    {
        "data": {
            "values": {
                "name": "Mamu",
                "age": "26"
            }
        }
    }

Jossons Query

    resp1->data.toArray() <+< resp2->data.toArray()

Output `mergeResult`

    [ {
      "name" : "kiran",
      "age" : "24"
    }, {
      "name" : "Mamu",
      "age" : "26"
    } ]

Josson Query

    toObject('values').toObject('data')

Output

    {
      "data" : {
        "values" : [ {
          "name" : "kiran",
          "age" : "24"
        }, {
          "name" : "Mamu",
          "age" : "26"
        } ]
      }
    }

---

#### 12. How to rearrange the json array based on id in java

(47427518)

    [
        {
            "router_id": "1101",
            "floor_id": "20",
            "building_id": "2",
            "router_name": "1101"
        },
        {
            "router_id": "1102",
            "floor_id": "20",
            "building_id": "2",
            "router_name": "1102"
        },
        {
            "router_id": "0",
            "floor_id": "20",
            "building_id": "2",
            "router_name": "pancoordinator"
        },
        {
            "router_id": "1104",
            "floor_id": "20",
            "building_id": "2",
            "router_name": "1104"
        }
    ]

Josson Query

    group(building_id).map(
        building_id,
        floors: elements.group(floor_id).map(
            floor_id,
            routers: elements.map(
                router_id, router_name)
        )
    )
    .toObject('buildings')

Output

    {
      "buildings" : [ {
        "building_id" : "2",
        "floors" : [ {
          "floor_id" : "20",
          "routers" : [ {
            "router_id" : "1101",
            "router_name" : "1101"
          }, {
            "router_id" : "1102",
            "router_name" : "1102"
          }, {
            "router_id" : "0",
            "router_name" : "pancoordinator"
          }, {
            "router_id" : "1104",
            "router_name" : "1104"
          } ]
        } ]
      } ]
    }

---

#### 13. Json object, convert json array into a json object in java

(51576987)

    {
        "values": [
            {
                "locale": "en_US",
                "source_key": "book_format",
                "value": "Hardback",
                "display_attr_name": "Book Format",
                "source_value": "Hardback",
                "isPrimary": "true"
            },
            {
                "isFacetValue": "true",
                "facet_version": "1.1",
                "locale": "en_US",
                "value": "Hardcover"
            }
        ]
    }

Josson Query

    values.mergeObjects()

Output

    {
      "locale" : "en_US",
      "source_key" : "book_format",
      "value" : "Hardcover",
      "display_attr_name" : "Book Format",
      "source_value" : "Hardback",
      "isPrimary" : "true",
      "isFacetValue" : "true",
      "facet_version" : "1.1"
    }

---

#### 14. How to use "set" with a predicate

(73506183)

    [ {
      "count" : 15,
      "_id" : {
        "DB User Name" : "admin",
        "Session Activity Type" : "LOGOFF"
      }
    }, {
      "count" : 16,
      "_id" : {
        "DB User Name" : "dbuser",
        "Session Activity Type" : "LOGON"
      }
    }, {
      "count" : 17,
      "_id" : {
        "DB User Name" : "dbuser",
        "Session Activity Type" : "LOGOFF"
      }
    }, {
      "count" : 18,
      "_id" : {
        "DB User Name" : "admin",
        "Session Activity Type" : "LOGON"
      }
    } ]

Josson Query

    field(_id.field(DB User Name.if(equals('dbuser'),'updated-Admin',?)))

Output

    [ {
      "count" : 15,
      "_id" : {
        "DB User Name" : "admin",
        "Session Activity Type" : "LOGOFF"
      }
    }, {
      "count" : 16,
      "_id" : {
        "DB User Name" : "updated-Admin",
        "Session Activity Type" : "LOGON"
      }
    }, {
      "count" : 17,
      "_id" : {
        "DB User Name" : "updated-Admin",
        "Session Activity Type" : "LOGOFF"
      }
    }, {
      "count" : 18,
      "_id" : {
        "DB User Name" : "admin",
        "Session Activity Type" : "LOGON"
      }
    } ]

---

#### 15. How to groupby json value like python in java?

(72790475)

    [
        {
            "ID": "id1",
            "ref": "ref1",
            "categ": "CATEG_A",
            "pagenb": 1
        },
        {
            "ID": "id2",
            "ref": "ref1",
            "categ": "CATEG_A",
            "pagenb": 2
        },
        {
            "ID": "id3",
            "ref": "ref1",
            "categ": "CATEG_B",
            "pagenb": 3
        }
    ]

Josson Query

    group(map(ref, categ), ID).map(ID, key.ref, key.categ)

Output

    [ {
      "ID" : [ "id1", "id2" ],
      "ref" : "ref1",
      "categ" : "CATEG_A"
    }, {
      "ID" : [ "id3" ],
      "ref" : "ref1",
      "categ" : "CATEG_B"
    } ]

---

#### 16. Correct way to transform a response into DTO

(73405994)

    {
      "cnpj": {
        "numeroCNPJ": "string"
      },
      "codigoCNES": {
        "codigo": "string"
      },
      "codigoUnidade": {
        "codigo": "string"
      },
      "diretor": {
        "cpf": {
          "numeroCPF": "string"
        },
        "nome": {
          "nome": "string"
        }
      },
      "nomeEmpresarial": {
        "nome": "string"
      },
      "nomeFantasia": {
        "nome": "string"
      }
    }

Josson Query

    entries().map(
        key::value.if([size()=1], *, **.mergeObjects())
    )

Output

    {
      "cnpj" : "string",
      "codigoCNES" : "string",
      "codigoUnidade" : "string",
      "diretor" : {
        "numeroCPF" : "string",
        "nome" : "string"
      },
      "nomeEmpresarial" : "string",
      "nomeFantasia" : "string"
    }

---

#### 17. How transform a json in another json using java

(35438323)

    [
        {
            "name": "Team Wolf",
            "www": "http://www.teamwolf.qqq",
            "department": "department1",
            "team1": "team1"
        },
        {
            "name": "Team Fox",
            "www": "http://www.teamfox.qqq",
            "department": "department1",
            "team2": "team2"
        },
        {
            "name": "Team Falcon",
            "www": "http://www.teamfalcon.qqq",
            "department": "department1",
            "team3": "team3"
        }
    ]

Josson Query

        group(department).map(
            department:: elements.map(
                ~'^team.*':: map(
                    name, www
                )
            )
            .mergeObjects()
        )

Output

    [ {
      "department1" : {
        "team1" : {
          "name" : "Team Wolf",
          "www" : "http://www.teamwolf.qqq"
        },
        "team2" : {
          "name" : "Team Fox",
          "www" : "http://www.teamfox.qqq"
        },
        "team3" : {
          "name" : "Team Falcon",
          "www" : "http://www.teamfalcon.qqq"
        }
      }
    } ]

---

#### 18. Variable api JSON responses to bind to Java Object using Jackson

(73397763)

    {
        "errorCount": 2,
        "errorIndices": [
            0,
            1
        ],
        "data": [
            {
                "errorCode": 901,
                "errorMessage": "IBad data: Check the data",
                "errorData": "xxxx"
            },
            {
                "errorCode": 901,
                "errorMessage": "IBad data: Check the data",
                "errorData": "XZY"
            },
            "fun now"
        ]
    }

Josson Query

    field(data[isText()]*, errors: data[isObject()]*)

Output

    {
      "errorCount" : 2,
      "errorIndices" : [ 0, 1 ],
      "data" : [ "fun now" ],
      "errors" : [ {
        "errorCode" : 901,
        "errorMessage" : "IBad data: Check the data",
        "errorData" : "xxxx"
      }, {
        "errorCode" : 901,
        "errorMessage" : "IBad data: Check the data",
        "errorData" : "XZY"
      } ]
    }

---

#### 19. How to convert JSONArray of objects to a json string message

(73224582)

    {
        "header": [
            {
                "key": "numberOfRecords",
                "value": "122",
                "valueDataType": "string"
            },
            {
                "key": "g_udit",
                "value": "1",
                "valueDataType": "string"
            },
            {
                "key": "userNameId",
                "value": "155",
                "valueDataType": "string"
            }
        ]
    }

Josson Query

    map(header.map(key::value).mergeObjects())

Output

    {
      "header" : {
        "numberOfRecords" : "122",
        "g_udit" : "1",
        "userNameId" : "155"
      }
    }

---

#### 20. Java copy properties with condition

(73200231)

    {
      "data": {
        "myProp": true,
        "myAnother": true,
        "myAnotherOne": false
      }
    }

Josson Query

    map(values: data.entries().[value]*.key.upperSnakeCase())

Output

    {
      "values" : [ "MY_PROP", "MY_ANOTHER" ]
    }

---

#### 21. Array input JSON

(73190751)

    {
        "data": [
            {
                "fieldname": "Name",
                "fieldvalue": [
                    "John Doe"
                ]
            },
            {
                "fieldname": "Title",
                "fieldvalue": [
                    "Manager"
                ]
            },
            {
                "fieldname": "Company",
                "fieldvalue": [
                    "Walmart"
                ]
            }
        ]
    }

Josson Query

    map(
      finalPayload: map(
        PI: map(
          EmpName: data[fieldname='Name'].fieldvalue[0],
          EmpRole: data[fieldname='Title'].fieldvalue[0]
        ),
        Company: data[fieldname='Company'].fieldvalue[0]
      )
    )

Output

    {
      "finalPayload" : {
        "PI" : {
          "EmpName" : "JohnDoe",
          "EmpRole" : "Manager"
        },
        "Company" : "Walmart"
      }
    }

---

#### 22. Compare nested JSON object with Java with differences

(73131799)

Dataset `left`

    {
        "package": {
            "institution": [
                {
                    "school": "TP"
                }
            ],
            "people": [
                {
                    "age": 32,
                    "name": "Bob"
                },
                {
                    "age": 16,
                    "name": "Amanda"
                }
            ],
            "details": [
                {
                    "course": "Humanities",
                    "duration": 4,
                    "description": "Students in Computer Sci"
                }
            ]
        }
    }

Dataset `right`

    {
        "package": {
            "institution": [
                {
                    "school": "MIT"
                }
            ],
            "people": [
                {
                    "age": 32,
                    "name": "Bob"
                },
                {
                    "age": 16,
                    "name": "Samantha"
                },
                {
                    "age": 20,
                    "name": "Dylan"
                }
            ],
            "details": [
                {
                    "course": "Computer Sci",
                    "duration": 4,
                    "description": "Students in Computer Sci"
                }
            ]
        }
    }

Jossons Query

    left->package >-> right->package

Output

    {
      "institution" : [ {
        "school" : "MIT"
      } ],
      "people" : [ {
        "age" : 16,
        "name" : "Samantha"
      }, {
        "age" : 20,
        "name" : "Dylan"
      } ],
      "details" : [ {
        "course" : "Computer Sci",
        "duration" : 4,
        "description" : "Students in Computer Sci"
      } ]
    }

---

#### 23. Build array of objects grouping attributes in the same object

(72442001)

    {
      "new": {
        "bc_sku_partner": [
          "Amazon",
          "Ebay"
        ],
        "bc_sku_channel": [
          "Partner",
          "Online",
          "Store"
        ],
        "cc_sku_channel": [
          "Store"
        ]
      }
    }

Josson Query

    new
    .entries()
    .unwind(value)
    .[key='bc_sku_partner' | value!='Partner']*
    .map(catalog:key.substr(0,2).upperCase(),
         channel:if([key='bc_sku_partner'],'Partner',value),
         partner:if([key='bc_sku_partner'],value))
    .toObject('catalogs')

Output

    {
      "catalogs" : [ {
        "catalog" : "BC",
        "channel" : "Partner",
        "partner" : "Amazon"
      }, {
        "catalog" : "BC",
        "channel" : "Partner",
        "partner" : "Ebay"
      }, {
        "catalog" : "BC",
        "channel" : "Online"
      }, {
        "catalog" : "BC",
        "channel" : "Store"
      }, {
        "catalog" : "CC",
        "channel" : "Store"
      } ]
    }

---

#### 24. Flatten array and nested object with default values for missing array elements

(72442443)

    {
      "id": "1234",
      "recordType": "E",
      "receiveDate": "2009-01-01",
      "receiveTime": "09:55:00",
      "releaseDate": "2009-01-02",
      "releaseTime": "08:30:00",
      "classifications": [
        {
          "reportType": 1031435,
          "description": {
            "string": "Progress Report"
          }
        },
        {
          "reportType": 8888888,
          "description": {
            "string": "Net Tangible Asset Backing"
          }
        }
      ],
      "type": "ASX"
    }

Josson Query

    map(id,
        recordType,
        classifications
         .map(reportType, description:description.string)
         .wrap()
         .collect([0],
                  ifNot([reportType=8888888], json('{"reportType":8888888,"description":"Default item"}')),
                  ifNot([reportType=9999999], json('{"reportType":9999999,"description":"Default item"}')))
         .flatten(1),
        type)

Output

    {
      "id" : "1234",
      "recordType" : "E",
      "classifications" : [ {
        "reportType" : 1031435,
        "description" : "Progress Report"
      }, {
        "reportType" : 8888888,
        "description" : "Net Tangible Asset Backing"
      }, {
        "reportType" : 9999999,
        "description" : "Default item"
      } ],
      "type" : "ASX"
    }

---

#### 25. Merge Array of multi Objects

(70753154)

    {
      \"group\": [
        {
          \"schema\": \"file\"
        },
        {
          \"key1\": \"val1\",
          \"key2\": \"val2\"
        },
        {
          \"schema\": \"folder\"
        },
        {
          \"key1\": \"val1\",
          \"key2\": \"val2\",
          \"key3\": \"val3\"
        },
        {
          \"schema\": \"dir\"
        },
        {
          \"key1\": \"val1\",
          \"key2\": \"val2\",
          \"key3\": \"val3\",
          \"key4\": \"val4\"
        }
      ]
    }

Josson Query

    group
    .group(#.floor(calc(?/2)))@
    .mergeObjects(elements)
    .@toObject('group')

Output

    {
      "group" : [ {
        "schema" : "file",
        "key1" : "val1",
        "key2" : "val2"
      }, {
        "schema" : "folder",
        "key1" : "val1",
        "key2" : "val2",
        "key3" : "val3"
      }, {
        "schema" : "dir",
        "key1" : "val1",
        "key2" : "val2",
        "key3" : "val3",
        "key4" : "val4"
      } ]
    }

---

#### 26. Transform by condition

(54502431)

    {
      "entry": [
        {
          "resource": {
            "id": "car-1",
            "type": "vehicle",
            "color": "red",
            "owner": {
              "ref": "Person/person-1"
            }
          }
        },
        {
          "resource": {
            "id": "car-2",
            "type": "vehicle",
            "color": "blue",
            "owner": {
              "ref": "Person/person-2"
            }
          }
        },
        {
          "resource": {
            "id": "person-1",
            "type": "Person",
            "name": "john"
          }
        },
        {
          "resource": {
            "id": "person-2",
            "type": "Person",
            "name": "wick"
          }
        }
      ]
    }

Josson Query

    entry
    .resource
    .group(if([type='vehicle'], owner.ref.removeStart('Person/'), id),
           if([type='vehicle'], field(owner:), map(ownername:name)))@
    .elements
    .mergeObjects()

Output

    [ {
      "id" : "car-1",
      "type" : "vehicle",
      "color" : "red",
      "ownername" : "john"
    }, {
      "id" : "car-2",
      "type" : "vehicle",
      "color" : "blue",
      "ownername" : "wick"
    } ]

---

#### 27. Apply some condition on jsonnode and filter resultset in Java

(73884462)

    [
     {
       "coupon": "VAR",
       "currency": "USD",
       "sip": "94989WAX5",
       "lastModifiedDate": "2022-09-23T08:16:25Z"
      },
      {
       "coupon": "VAR1",
       "currency": "USD",
       "sip": "94989WAX5",
       "lastModifiedDate": "2022-09-21T08:16:25Z"
      },
      {
       "coupon": "VAR3",
       "currency": "USD",
       "sip": "XHBRYWEB1",
       "lastModifiedDate": "2022-09-20T08:16:25Z"
      }
     ]

Josson Query

    group(sip)@.elements.findByMax(lastModifiedDate)

Output

    [ {
      "coupon" : "VAR",
      "currency" : "USD",
      "sip" : "94989WAX5",
      "lastModifiedDate" : "2022-09-23T08:16:25Z"
    }, {
      "coupon" : "VAR3",
      "currency" : "USD",
      "sip" : "XHBRYWEB1",
      "lastModifiedDate" : "2022-09-20T08:16:25Z"
    } ]

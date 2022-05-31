# Josson & Jossons

- _Josson_ is a query language for JSON.
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
        <version>1.3.9</version>
    </dependency>

### Gradle

    implementation group: 'com.octomix.josson', name: 'josson', version: '1.3.9'

## Features and Capabilities

### Josson

- Query a JSON dataset.
- Restructure JSON data.
- Has many functions to format text output.
- Has many functions to manipulate date values.
- Has many functions to work on array node.
- Can be used as an API parameter to trim down the response JSON result.

### Jossons

- Query data from multiple JSON datasets.
- Join two JSON datasets to build a new dataset.
- Resolve template placeholder from external data source on demand.
- I used Jossons to generate millions of SMS/Email notifications during the first year.
- I used Jossons to generate reports that retrieve data from MongoDB directly without writing a line of code.
- Can be used to build a rule engine.

## Table of Contents

- [Josson Basic](#josson-basic)

- [Josson Query Language](#josson-query-language)
  - [Path Steps](#path-steps)
  - [Operators](#operators)
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
  - [Implicit Variables](#implicit-variables)
  - [Fill In](#fill-in)

- [Jossons Resolver](#jossons-resolver)
  - [Dictionary Finder](#dictionary-finder)
  - [Data Finder](#data-finder)
  - [Join Datasets](#join-datasets)
  - [Dictionary Function](#dictionary-function)
  - [Put Together](#put-together)

- [Appendix](#appendix)
  - [MongoDB Adapter](#mongodb-adapter)

---

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

### Path Steps

| Step                 | Description                                                           |
|:---------------------|:----------------------------------------------------------------------|
| `array[number]`      | An array element by zero-based index                                  |
| `array[expression]`  | A filter to find the first matching element                           |
| `array[expression]*` | A filter to query all matching elements                               |
| `array[expression]@` | A filter to divert each matching element to separate branch           |
| `array@`             | Divert each element to separate branch                                |
| `[]@`                | Divert each element of the current array node to separate branch      |
| `object[expression]` | A validation filter                                                   |
| `function()`         | A Josson function                                                     |
| `@function()`        | Merge all branch results into a single array before manipulation      |
| `@.`                 | The array node itself if the current node is an array node            |
| `?`                  | The node itself for non-array node or an array element for array node |
| `#`                  | Zero-based index of an array element                                  |
| `##`                 | One-based index of an array element                                   |
| `#A`                 | Uppercase alphabetic index of an array element                        |
| `#a`                 | Lowercase alphabetic index of an array element                        |
| `#R`                 | Uppercase roman numerals index of an array element                    |
| `#r`                 | Lowercase roman numerals index of an array element                    |

### Operators

| Operator | Description                             |
|:---------|:----------------------------------------|
| =        | Is equal to (support object and array)  |
| !=       | Not equal to (support object and array) |
| &gt;     | Greater than                            |
| >=       | Greater than or equal to                |
| &lt;     | Less than                               |
| <=       | Less than or equal to                   |
| =~       | Left matches regular expression         |
| !        | Logical NOT                             |
| &        | Logical AND                             |
| &#124;   | Logical OR                              |

### Path Chart Elements

Josson path chart shows data type changes and data flow along the path.
Data filtering, transformation and formatting details are not included.

         %              Any JSON node

         [%]            Array node

         {}             Object node

         ""             Text node

         $I             Integer node

         $D             Double node

         $TF            Boolean node

         null           Null node

         $V             Any value node ""|$I|$D|$TF|null

         $V...          Multiple value nodes inside function arguments

         ->             A path step progress

         obj{}          A named object node

         {}.%           Object parent-child relation is connected by a "." (Except the branch's 1st step)

       {}[]->{}         An object with validation filter

       [%]*->[%]        An array node proceeded one step with all its elements

       [%][]->%         An array node with find-first filter

       [%][]*->[%]      An array node with find-all filter

              %->
             /          An array node with find-all filter and
       [%][]@           divert each filtered element to separate branch
             \
              %->

       ->func()->       A function that manipulate the current node

     ->[%->func()]->    A function that manipulate each element of an array node

        func(?)         Function parameter symbol "?" represents the current node

       .--->---.
      /         \       Function parameter symbol "@" represents the parent array node
    []->[%->func(@)]

       [[%]]->[%]       Nested array is flattened each step

            %->
           /
      ->[]@             Divert each array element to separate branch
           \
            %->
      ->%
         \
          @->[%]        Merge branches into an array
         /
      ->%

         ==>%           Final result

      ->%
         \
          ==>[%]        Merge branches to a final result
         /
      ->%

          !!            The position where the step is unresolvable 

    ==>!unresolvable!   Unable to resolve the result (Returns a Java null value)

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

        {}->salesPerson ==>""

2. Node name is case-sensitive. Josson returns null value if the path is unresolvable.

        josson.getNode("salesperson")
        ==>
        !unresolvable!

   _Path chart_

        {}->salesperson!! ==>!unresolvable!

3. To query an object node.

        josson.getNode("customer")
        ==>
        {
            "customerId" : "CU0001",
            "name" : "Peggy",
            "phone" : "+852 62000610"
        }

   _Path chart_

        {}->customer{} ==>{}

4. Parent node and child node are connected by a `.`.

        josson.getNode("customer.name")
        ==>
        "Peggy"

   _Path chart_

        {}->customer.name ==>""

5. Function is constructed by a function name followed by parentheses with optional comma-separated arguments.  
   A function manipulate the current node and produce an output along the path.

        josson.getNode("customer.name.upperCase()")
        ==>
        "PEGGY"

   _Path chart_

        {}->customer.name->upperCase() ==>""

6. Function name is case-insensitive. Function parameter can refer to a child node of the step.

        josson.getNode("customer.UPPERCase(name)")
        ==>
        "PEGGY"

   _Path chart_

        {}->customer{}->UPPERCase($V) ==>""

7. If the function is the first path step, it works on the root node.

        josson.getNode("upperCase(customer.name)")
        ==>
        "PEGGY"

   _Path chart_

        {}->upperCase($V) ==>""

8. Functions can be nested and the parameters can refer to those child nodes of the same step.

        josson.getNode("customer.concat(upperCase(name), ' / ', phone)")
        ==>
        "PEGGY / +852 62000610"

   _Path chart_

        {}->customer{}->concat($V...) ==>""

9. A path start with numbers override the data and produces an integer node.

        josson.getNode("123")
        ==>
        123

   _Path chart_

        $I ==>$I

10. A path start with numbers and has `.` produces a double node.

        josson.getNode("123.40")
        ==>
        123.4

    _Path chart_

        $D ==>$D

11. A path start and end with single quote `'`override the data and produces a text string node.  
    If the string literal contains a single quote, it is replaced by two single quotes.

        josson.getNode("'She said, ''Go ahead''.'")
        ==>
        "She said, 'Go ahead'."

    _Path chart_

        "" ==>""

12. A path start with `true` or `false` override the data and produces a boolean node.

        josson.getNode("true.not()")
        ==>
        false

    _Path chart_

        $TF->not() ==>$TF

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

        {}->items* ==>[{}]

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

        {}->items[0] ==>{}

15. To query a child value node of an array element.

        josson.getNode("items[1].name")
        ==>
        "OctoPlus Tennis Racket - Star"

    _Path chart_

        {}->items[1].name ==>""

16. To query a child object node of an array element.

        josson.getNode("items[2].property")
        ==>
        {
          "size" : "35",
          "colors" : [ "RED" ]
        }

    _Path chart_

        {}->items[2].property{} ==>{}

17. To query all the elements of an array node and output them inside an array node.

        josson.getNode("items.qty")
        ==>
        [ 2, 1, 1 ]

    _Path chart_

        {}->items*->[qty] ==>[$I]

18. A function that manipulates each array element and output all results inside an array node.

        josson.getNode("items.concat('Qty=',qty)")
        ==>
        [ "Qty=2", "Qty=1", "Qty=1" ]

    _Path chart_

        {}->items*->[{}->concat($V)] ==>[""]

19. If a step is working on an object or value node, `?` represents that node.

        josson.getNode("items.qty.concat('Qty=',?)")
        ==>
        [ "Qty=2", "Qty=1", "Qty=1" ]

    _Path chart_

        {}->items*->[qty]->[$I->concat(?)] ==>[""]

20. A function that manipulates an array node and produce a value node.

        josson.getNode("items.qty.sum()")
        ==>
        4.0

    _Path chart_

        {}->items*->[qty]->sum() ==>$D

21. Uses Java standard formatting pattern.

        josson.getNode("items.sum(qty).formatNumber('#,##0')")
        ==>
        "4"

    _Path chart_

        {}->items*->[{}]->sum([$V])->formatNumber() ==>""

22. Find the first matching element by array filter.

        josson.getNode("items.itemCode[!startsWith('A')]")
        ==>
        "B00001"

    _Path chart_

        {}->items*->[itemCode][] ==>""

23. Filter using relational operators `=`, `!=`, `>`, `>=`, `<` and `<=`.

        josson.getNode("items[unitDiscount > 0].name")
        ==>
        "OctoPlus Tennis Racket - Star"

    _Path chart_

        {}->items[].name ==>""

24. Returns null value if nothing matches the array filter.

        josson.getNode("items[unitDiscount > 100].name")
        ==>
        !unresolvable!

    _Path chart_

        {}->items[]!!.name ==>!unresolvable!

25. To query all matching elements, add a modifier `*` after the array filter.

        josson.getNode("items[unitDiscount > 0]*.name")
        ==>
        [ "OctoPlus Tennis Racket - Star", "WinWin Sport Shoe - Super" ]

    _Path chart_

        {}->items[]*->[name] ==>[""]

26. If a step is working on an array node, `#` denotes the zero-based index of an array element.

        josson.getNode("items[#.isEven()]*.itemCode")
        ==>
        [ "B00001", "A00201" ]

    _Path chart_

        {}->items[]*->[itemCode] ==>[""]

27. For each path step, a nested array is flattened once.

        josson.getNode("items[true]*.tags[true]*")
        ==>
        [ "SHIRT", "WOMEN", "TENNIS", "SPORT", "RACKET", "SHOE", "SPORT", "WOMEN" ]

    _Path chart_

        {}->items[]*->[tags[]*->[""]] ==>[""]

28. Path step `array.` is the same as `array[true]*.`.

        josson.getNode("items.tags")
        ==>
        [ "SHIRT", "WOMEN", "TENNIS", "SPORT", "RACKET", "SHOE", "SPORT", "WOMEN" ]

    _Path chart_

        {}->items*->[tags*->[""]] ==>[""]

29. If a step is working on an array node, `?` represents an array element.  
    `=~` matches a regular expression.

        josson.getNode("items.tags[? =~ '^S.*O.+']*")
        ==>
        [ "SPORT", "SHOE", "SPORT" ]

    _Path chart_

        {}->items*->[tags[]*->[""]] ==>[""]

30. The matching criteria supports logical operators and parentheses.

    >not `!`  
     and `&`  
     or `|`

        josson.getNode("items[(unitDiscount=null | unitDiscount=0) & !(qty<=1)]*.name")
        ==>
        [ "WinWin TShirt Series A - 2022" ]

    _Path chart_

        {}->items[]*->[name] ==>[""]

31. Example of a find-all filter operation with flattened array result.

        josson.getNode("items[tags.contains('SPORT')]*.tags")
        ==>
        [ "TENNIS", "SPORT", "RACKET", "SHOE", "SPORT", "WOMEN" ]

    _Path chart_

        {}->items[]*->[tags*->[""]] ==>[""]

32. An array filter modifier `@` divert each element to separate branch for upcoming manipulation.  
    The final output merges branches into an array.

        josson.getNode("items[tags.containsIgnoreCase('Women')]@.tags")
        ==>
        [ [ "SHIRT", "WOMEN" ], [ "SHOE", "SPORT", "WOMEN" ] ]

    _Path chart_

                     {}->tags*->[""]
                    /               \
        {}->items[]@                 ==>[[""]]
                    \               /
                     {}->tags*->[""]

33. Some functions work on an array node and produce a value node.

        josson.getNode("items.tags.join('+')")
        ==>
        SHIRT+WOMEN+TENNIS+SPORT+RACKET+SHOE+SPORT+WOMEN

    _Path chart_

        {}->items*->[tags*->[""]]->[""]->join() ==>""

34. An array node can apply the modifier `@` that divert each element to separate branch.

        josson.getNode("items@.tags.join('+')")
        ==>
        [ "SHIRT+WOMEN", "TENNIS+SPORT+RACKET", "SHOE+SPORT+WOMEN" ]

    _Path chart_

                   {}->tags*->[""]->join()->""
                  /                           \
        {}->items@                             ==>[""]
                  \                           /
                   {}->tags*->[""]->join()->""

35. Syntax `[]@` diverts each element of the current array node.

        josson.getNode("items.join([]@.tags.join('+'),' / ')")
        ==>
        "SHIRT+WOMEN / TENNIS+SPORT+RACKET / SHOE+SPORT+WOMEN"

    _Path chart_

                                   {}->tags*->[""]->join()->""
                                  /                           \
        {}->items*->[{}]->join([]@                             =>[""]) ==>""
                                  \                           /
                                   {}->tags*->[""]->join()->""

36. Modifier `@` before a function name merges all branch results into a single array before manipulation.

        josson.getNode("items@.tags.join('+').@join(' / ')")
        ==>
        "SHIRT+WOMEN / TENNIS+SPORT+RACKET / SHOE+SPORT+WOMEN"

    _Path chart_

                   {}->tags*->[""]->join()->""
                  /                           \
        {}->items@                             @->[""]->join() ==>""
                  \                           /
                   {}->tags*->[""]->join()->""

37. Syntax `[]@` can divert the array output of function.

        josson.getNode("'1+2 | 3+4 | 5+6'.split('|').[]@.split('+').calc(?*2).round(0).join('+').concat('(',?,')/2').@join(' | ')")
        ==>
        "(2+4)/2 | (6+8)/2 | (10+12)/2"

    _Path chart_

                           ""->split()->[""->calc(?)]->[$D->round()]->[$I]->join()->""->concat()
                          /                                                                     \
        ""->split()->[""]@                                                                       @->[""]->join()==>""
                          \                                                                     /
                           ""->split()->[""->calc(?)]->[$D->round()]->[$I]->join()->""->concat()

38. All function parameters can refer to a child node of the step.

        josson.getNode("items@.repeat(concat('[',brand,'] ',name,'\n'), qty).@join()")
        ==>
        "[WinWin] WinWin TShirt Series A - 2022\n" +
        "[WinWin] WinWin TShirt Series A - 2022\n" +
        "[OctoPlus] OctoPlus Tennis Racket - Star\n" +
        "[WinWin] WinWin Sport Shoe - Super\n"

    _Path chart_

                   {}->repeat($V...)->""
                  /                     \
        {}->items@                       @->[""]->join()==>""
                  \                     /
                   {}->repeat($V...)->""

39. Some functions work on array and produce an array, such as `concat()`, manipulate on each element.

        josson.getNode("items.concat('Item ',#,': [',itemCode,'] ',qty,unit,' x ',name,' <',property.colors.join(','),'>').join('\n')")
        ==>
        "Item 0: [B00001] 2Pcs x WinWin TShirt Series A - 2022 <WHITE,RED>\n" +
        "Item 1: [A00308] 1Pcs x OctoPlus Tennis Racket - Star <BLACK>\n" +
        "Item 2: [A00201] 1Pair x WinWin Sport Shoe - Super <RED>"

    _Path chart_

        {}->items*->[{}->concat(#, $V...)]->join() ==>""

40. If a step is working on an array node, `@` represents that array node.  
    `##` denotes the one-based index of an array element.

        josson.getNode("items.sort(itemCode).concat('Item ',##,'/',@.size(),': [',itemCode,'] ',qty,unit,' x ',name,' <',property.colors.join(','),'>').join('\n')")
        ==>
        "Item 1/3: [A00201] 1Pair x WinWin Sport Shoe - Super <RED>\n" +
        "Item 2/3: [A00308] 1Pcs x OctoPlus Tennis Racket - Star <BLACK>\n" +
        "Item 3/3: [B00001] 2Pcs x WinWin TShirt Series A - 2022 <WHITE,RED>"

    _Path chart_

                                   .----->----.
                                  /            \
        {}->items*->[{}]->sort($V)->[{}->concat(@, ##, $V...)]->join() ==>""

41. An object node with a validation filter.

        josson.getNode("customer[name='Peggy']")
        ==>
        {
          "customerId" : "CU0001",
          "name" : "Peggy",
          "phone" : "+852 62000610"
        }

    _Path chart_

        {}->customer[] ==>{}

42. An object node with a validation filter.

        josson.getNode("customer[name='Raymond']")
        ==>
        !unresolvable!

    _Path chart_

        {}->customer[]!! ==>!unresolvable!

43. Function `json()` parse a JSON string.

        josson.getNode("json('[1,2,"3"]')")
        ==>
        [ 1, 2, "3" ]

    _Path chart_

        [] ==>[]

44. Relational operator `=` and `!=` support object comparison.

        josson.getNode("[customer = json('{"name":"Peggy","phone":"+852 62000610","customerId":"CU0001"}')].isNotNull()")
        ==>
        true

    _Path chart_

        {}->{}[]->{}->isNotNull() ==>$TF

45. Relational operator `=` and `!=` support root level array values comparison where the position ordering is allowed to be different.

        josson.getNode("[items[0].property.colors = json('["RED","WHITE"]')].isNotNull()")
        ==>
        true

    _Path chart_

        {}->{}[]->{}->isNotNull() ==>$TF

46. Function `calc()` uses MathParser.org-mXparser library <http://mathparser.org/> to perform calculation.

        josson.getNode("items.calc(qty * (unitPrice-unitDiscount)).concat(##,'=',?)")
        ==>
        [ null, "2=140.0", "3=100.0" ]

    _Path chart_

        {}->items*->[{}->calc($V...)]->[$D->concat(?, ##)] ==>[""]

47. Non-array manipulate functions preserve null element.

        josson.getNode("items.calc(qty * (unitPrice-unitDiscount)).[##<=2]*.concat(##,'=',?)")
        ==>
        [ null, "2=140.0" ]

    _Path chart_

        {}->items*->[{}->calc($V...)]->[$D][]*->[$D->concat(?, ##)] ==>[""]

48. An array-to-value transformation function throws away null nodes automatically.

        josson.getNode("items.calc(qty * (unitPrice-unitDiscount)).concat(##,'=',?).join(' / ')")
        ==>
        "2=140.0 / 3=100.0"

    _Path chart_

        {}->items*->[{}->calc($V...)]->[$D->concat(?, ##)]->join() ==>""

49. Array filter can filter out null nodes.

        josson.getNode("items.calc(qty * (unitPrice-unitDiscount)).[isNotNull()]*.concat(##,'=',?)")
        ==>
        [ "1=140.0", "2=100.0" ]

    _Path chart_

        {}->items*->[{}->calc($V...)]->[$D][]*->[$D->concat(?, ##)] ==>[""]

50. An argument `#A` denotes the uppercase alphabetic array index.

        josson.getNode("items.calc(qty * (unitPrice-unitDiscount)).[?!=null]*.concat(#A,'=',?).join(' / ')")
        ==>
        "A=140.0 / B=100.0"

    _Path chart_

        {}->items*->[{}->calc($V...)]->[$D][]*->[$D->concat(?, #A)]->join() ==>""

51. Merge Diverted branches throws away null nodes automatically.
    An argument `#a` denotes the lowercase alphabetic array index.

        josson.getNode("items@.calc(qty * (unitPrice-unitDiscount)).@concat(#a,'=',?)")
        ==>
        [ "a=140.0", "b=100.0" ]

    _Path chart_

                   {}->calc($V...)->$D
                  /                   \
        {}->items@                     @->[$D->concat(?, #a)] ==>[""]
                  \                   /
                   {}->calc($V...)->$D

52. mXparser expression accepts single-step path only.
    To apply multi-steps path, function or filter, append arguments with syntax `newVariable:path`.

        josson.getNode("items.calc(qty * (unitPrice-x), x:coalesce(unitDiscount,0)).formatNumber('US$#,##0.00')")
        ==>
        [ "US$30.00", "US$140.00", "US$100.00" ]

    _Path chart_

        {}->items*->[{}->calc($V...)]->[$D->formatNumber()] ==>[""]

53. An argument `#r` and `#R` denotes the lowercase and uppercase roman numerals array index.

        josson.getNode("items.unitPrice.calc(? * 2).concat(#r,'=',?)")
        ==>
        [ "i=30.0", "ii=300.0", "iii=220.0" ]

    _Path chart_

        {}->items*->[unitPrice]->[$D->calc(?)]->[$D->concat(?)] ==>[""]

54. Function `entries()` returns an array of an object's string-keyed property `[key, value]` pairs.

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

        {}->items[0]->{}->entries() ==>[{}]

55. Function `keys()` lists an object's key names.

        josson.getNode("keys()")
        ==>
        [ "salesOrderId", "salesDate", "salesPerson", "customer", "items", "totalAmount" ]

    _Path chart_

        {}->keys() ==>[""]

56. `keys()` can retrieve nested child object keys for a given levels.

        josson.getNode("keys(?, 2)")
        ==>
        [ "salesOrderId", "salesDate", "salesPerson", "customer", "customerId", "name", "phone", "items", "totalAmount" ]

    _Path chart_

        {}->keys(?) ==>[""]

57. Function `toArray()` puts an object's values into an array.

        josson.getNode("customer.toArray()")
        ==>
        [ "CU0001", "Peggy", "+852 62000610" ]

    _Path chart_

        {}->customer->toArray() ==>[""]

58. Furthermore, function `toArray()` puts all arguments (values, object's values, array elements) into a single array.

        josson.getNode("toArray('Hello',customer,items.itemCode.sort())")
        ==>
        [ "Hello", "CU0001", "Peggy", "+852 62000610", "A00201", "A00308", "B00001" ]

    _Path chart_

        {}->customer->toArray($V...) ==>[""]

59. Function `map()` constructs a new object node.
    For multi-steps path, the last element name will become the new element name.
    To rename an element, use syntax `newFieldName:path`.

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

        {}->map($V...) ==>{}

60. Function `field()` adds, removes and renames field on the current object node.
    To remove an element, use syntax `fieldName:`.

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

        {}->items[]->{}->field($V...) ==>{}

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

        {}->items*->[{}->field($V...)] ==>[{}]

62. Function `flatten()` flatten an array same as the default path step behavior. But more readable.

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

                   {}->tags*->[""]
                  /               \
        {}->items@                 @->[[""]]->flatten() ==>[""]
                  \               /
                   {}->tags*->[""]

## Josson Functions

There are over 180 functions. They are classified into 7 categories:

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
14. [keepAfter()](#14-keepafter)
15. [keepAfterIgnoreCase()](#15-keepafterignorecase)
16. [keepAfterLast()](#16-keepafterlast)
17. [keepAfterLastIgnoreCase()](#17-keepafterlastignorecase)
18. [keepBefore()](#18-keepbefore)
19. [keepBeforeIgnoreCase()](#19-keepbeforeignorecase)
20. [keepBeforeLast()](#20-keepbeforelast)
21. [keepBeforeLastIgnoreCase()](#21-keepbeforelastignorecase)
22. [leftPad()](#22-leftpad)
23. [length()](#23-length)
24. [lowerCase()](#24-lowercase)
25. [notEmpty()](#25-notempty)
26. [notBlank()](#26-notblank)
27. [prepend()](#27-prepend)
28. [prependIfMissing()](#28-prependifmissing)
29. [prependIfMissingIgnoreCase()](#29-prependifmissingignorecase)
30. [removeEnd()](#30-removeend)
31. [removeEndIgnoreCase()](#31-removeendignorecase)
32. [removeStart()](#32-removestart)
33. [removeStartIgnoreCase()](#33-removestartignorecase)
34. [repeat()](#34-repeat)
35. [replace()](#35-replace)
36. [replaceIgnoreCase()](#36-replaceignorecase)
37. [rightPad()](#37-rightpad)
38. [split()](#38-split)
39. [strip()](#39-strip)
40. [stripEnd()](#40-stripend)
41. [stripStart()](#41-stripstart)
42. [substr()](#42-substr)
43. [trim()](#43-trim)
44. [uncapitalize()](#44-uncapitalize)
45. [upperCase()](#45-uppercase)
46. [singleQuote() / quote()](#46-singlequote--quote)
47. [doubleQuote()](#47-doublequote)

Date Functions

48. [amPmOfDay()](#48-ampmofday)
49. [second()](#49-second)
50. [secondOfDay()](#50-secondofday)
51. [minute()](#51-minute)
52. [minuteOfDay()](#52-minuteofday)
53. [hourOfAmPm()](#53-hourofampm)
54. [hour()](#54-hour)
55. [dayOfWeek()](#55-dayofweek)
56. [day()](#56-day)
57. [dayOfYear()](#57-dayofyear)
58. [month()](#58-month)
59. [year()](#59-year)
60. [plusSeconds()](#60-plusseconds)
61. [plusMinutes()](#61-plusminutes)
62. [plusHours()](#62-plushours)
63. [plusDays()](#63-plusdays)
64. [plusWeeks()](#64-plusweeks)
65. [plusMonths()](#65-plusmonths)
66. [plusYears()](#66-plusyears)
67. [minusSeconds()](#67-minusseconds)
68. [minusMinutes()](#68-minusminutes)
69. [minusHours()](#69-minushours)
70. [minusDays()](#70-minusdays)
71. [minusWeeks()](#71-minusweeks)
72. [minusMonths()](#72-minusmonths)
73. [minusYears()](#73-minusyears)
74. [truncateToMicro()](#74-truncatetomicro)
75. [truncateToMilli()](#75-truncatetomilli)
76. [truncateToSecond()](#76-truncatetosecond)
77. [truncateToMinute()](#77-truncatetominute)
78. [truncateToHour()](#78-truncatetohour)
79. [truncateToDay()](#79-truncatetoday)
80. [truncateToMonth()](#80-truncatetomonth)
81. [truncateToYear()](#81-truncatetoyear)
82. [withNano()](#82-withnano)
83. [withMicro()](#83-withmicro)
84. [withMilli()](#84-withmilli)
85. [withSecond()](#85-withsecond)
86. [withMinute()](#86-withminute)
87. [withHour()](#87-withhour)
88. [withDay()](#88-withday)
89. [withDayOfYear()](#89-withdayofyear)
90. [withMonth()](#90-withmonth)
91. [withYear()](#91-withyear)
92. [dayEnd()](#92-dayend)
93. [monthEnd()](#93-monthend)
94. [yearEnd()](#94-yearend)
95. [lengthOfMonth()](#95-lengthofmonth)
96. [lengthOfYear()](#96-lengthofyear)
97. [localToOffsetDate()](#97-localtooffsetdate)
98. [offsetToLocalDate()](#98-offsettolocaldate)

Format Functions

99. [b64Encode()](#99-b64encode)
100. [b64EncodeNoPadding()](#100-b64encodenopadding)
101. [b64MimeEncode()](#101-b64mimeencode)
102. [b64MimeEncodeNoPadding()](#102-b64mimeencodenopadding)
103. [b64UrlEncode()](#103-b64urlencode)
104. [b64UrlEncodeNoPadding()](#104-b64urlencodenopadding)
105. [b64Decode()](#105-b64decode)
106. [b64MimeDecode()](#106-b64mimedecode)
107. [b64UrlDecode()](#107-b64urldecode)
108. [urlEncode()](#108-urlencode)
109. [urlDecode()](#109-urldecode)
110. [if()](#110-if)
111. [caseValue()](#111-casevalue)
112. [cycleValue()](#112-cyclevalue)
113. [indexedValue()](#113-indexedvalue)
114. [formatDate()](#114-formatdate)
115. [formatNumber()](#115-formatnumber)
116. [formatText()](#116-formattext)
117. [formatTexts()](#117-formattexts)
118. [toNumber()](#118-tonumber)
119. [toString()](#119-tostring)
120. [toText()](#120-totext)
121. [csv()](#121-csv)
122. [csvShowNull()](#122-csvshownull)

Logical Functions

123. [contains()](#123-contains)
124. [containsIgnoreCase()](#124-containsignorecase)
125. [notContains()](#125-notcontains)
126. [notContainsIgnoreCase()](#126-notcontainsignorecase)
127. [startsWith()](#127-startswith)
128. [startsWithIgnoreCase()](#128-startswithignorecase)
129. [notStartsWith()](#129-notstartswith)
130. [notStartsWithIgnoreCase()](#130-notstartswithignorecase)
131. [endsWith()](#131-endswith)
132. [endsWithIgnoreCase()](#132-endswithignorecase)
133. [notEndsWith()](#133-notendswith)
134. [notEndsWithIgnoreCase()](#134-notendswithignorecase)
135. [equals()](#135-equals)
136. [equalsIgnoreCase()](#136-equalsignorecase)
137. [notEquals()](#137-notequals)
138. [notEqualsIgnoreCase()](#138-notequalsignorecase)
139. [matches()](#139-matches)
140. [notMatches()](#140-notmatches)
141. [in()](#141-in)
142. [inIgnoreCase()](#142-inignorecase)
143. [notIn()](#143-notin)
144. [notInIgnoreCase()](#144-notinignorecase)
145. [isEmpty()](#145-isempty)
146. [isNotEmpty()](#146-isnotempty)
147. [isBlank()](#147-isblank)
148. [isNotBlank()](#148-isnotblank)
149. [isNull()](#149-isnull)
150. [isNotNull()](#150-isnotnull)
151. [isText()](#151-istext)
152. [isBoolean()](#152-isboolean)
153. [isNumber()](#153-isnumber)
154. [isEven()](#154-iseven)
155. [isOdd()](#155-isodd)
156. [not()](#156-not)
157. [isWeekday()](#157-isweekday)
158. [isWeekend()](#158-isweekend)
159. [isLeapYear()](#159-isleapyear)

Array Functions

160. [size()](#160-size)
161. [lastIndex()](#161-lastindex)
162. [indexOf()](#162-indexof)
163. [lastIndexOf()](#163-lastindexof)
164. [first()](#164-first)
165. [last()](#165-last)
166. [max()](#166-max)
167. [min()](#167-min)
168. [sum()](#168-sum)
169. [avg()](#169-avg)
170. [count()](#170-count)
171. [reverse()](#171-reverse)
172. [slice()](#172-slice)
173. [sort()](#173-sort)
174. [distinct()](#174-distinct)
175. [join()](#175-join)
176. [findByMax()](#176-findbymax)
177. [findByMin()](#177-findbymin)
178. [findByNullOrMax()](#178-findbynullormax)
179. [findByNullOrMin()](#179-findbynullormin)
180. [findByMaxOrNull()](#180-findbymaxornull)
181. [findByMinOrNull()](#181-findbyminornull)

Structural Functions

182. [json()](#182-json)
183. [entries()](#183-entries)
184. [keys()](#184-keys)
185. [toArray()](#185-toarray)
186. [flatten()](#186-flatten)
187. [map()](#187-map)
188. [field()](#188-field)
189. [coalesce()](#189-coalesce)

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

#### 14. keepAfter()

    'abcxmnxyz'.keepAfter('x') ==> "mnxyz"

    'abcxmnxyz'.keepAfter(?, 'X') ==> ""

    keepAfter('abcxmnxyz', 'mn') ==> "xyz"

#### 15. keepAfterIgnoreCase()

    'abcxmnxyz'.keepAfterIgnoreCase('x') ==> "mnxyz"

    'abcxmnxyz'.keepAfterIgnoreCase(?, 'X') ==> "mnxyz"

    keepAfterIgnoreCase('abcxmnxyz', 'mn') ==> "xyz"

#### 16. keepAfterLast()

    'abcxmnxyz'.keepAfterLast('x') ==> "yz"

    'abcxmnxyz'.keepAfterLast(?, 'X') ==> ""

    keepAfterLast('abcxmnxyz', 'mn') ==> "xyz"

#### 17. keepAfterLastIgnoreCase()

    'abcxmnxyz'.keepAfterLastIgnoreCase('x') ==> "yz"

    'abcxmnxyz'.keepAfterLastIgnoreCase(?, 'X') ==> "yz"

    keepAfterLastIgnoreCase('abcxmnxyz', 'mn') ==> "xyz"

#### 18. keepBefore()

    'abcxmnxyz'.keepBefore('x') ==> "abc"

    'abcxmnxyz'.keepBefore(?, 'X') ==> ""

    keepBefore('abcxmnxyz', 'mn') ==> "abcx"

#### 19. keepBeforeIgnoreCase()

    'abcxmnxyz'.keepBeforeIgnoreCase('x') ==> "abc"

    'abcxmnxyz'.keepBeforeIgnoreCase(?, 'X') ==> "abc"

    keepBeforeIgnoreCase('abcxmnxyz', 'mn') ==> "abcx"

#### 20. keepBeforeLast()

    'abcxmnxyz'.keepBeforeLast('x') ==> "abcxmn"

    'abcxmnxyz'.keepBeforeLast(?, 'X') ==> ""

    keepBeforeLast('abcxmnxyz', 'mn') ==> "abcx"

#### 21. keepBeforeLastIgnoreCase()

    'abcxmnxyz'.keepBeforeLastIgnoreCase('x') ==> "abcxmn"

    'abcxmnxyz'.keepBeforeLastIgnoreCase(?, 'X') ==> "abcxmn"

    keepBeforeLastIgnoreCase('abcxmnxyz', 'mn') ==> "abcx"

#### 22. leftPad()

    'bat'.leftPad(5) ==> "  bat"

    'bat'.leftPad(?, 8, 'yz') ==> "yzyzybat"

    leftPad('bat', 3, 'yz') ==> "bat"

    5.leftPad('bat', ?, '') ==> "  bat"

#### 23. length()

    'Josson'.length() ==> 6

    length('Josson') ==> 6

    length(2022) ==> 4

#### 24. lowerCase()

    'Cat'.lowerCase() ==> "cat"

    lowerCase('cAt') ==> "cat"

#### 25. notEmpty()

    'abc'.notEmpty('xyz') ==> "abc"

    ''.notEmpty(null, '', 'xyz') ==> "xyz"

    json('{"a":"","b":"","c":"abc"}').notEmpty(a,b,c,'xyz') ==> "abc"

#### 26. notBlank()

    'abc'.notBlank('xyz') ==> "abc"

    ' '.notBlank(null, '  ', 'xyz') ==> "xyz"

    json('{"a":" ","b":" ","c":"abc"}').notBlank(a,b,c,'xyz') ==> "abc"

#### 27. prepend()

    'abc'.prepend('xyz') ==> "xyzabc"

    'abc'.prepend(?, 'xyz') ==> "xyzabc"

    prepend('xyzabc', 'xyz') ==> "xyzxyzabc"

    'xyz'.prepend('XYZabc', ?) ==> "xyzXYZabc"

#### 28. prependIfMissing()

    'abc'.prependIfMissing('xyz') ==> "xyzabc"

    'abc'.prependIfMissing(?, 'xyz') ==> "xyzabc"

    prependIfMissing('xyzabc', 'xyz') ==> "xyzabc"

    'xyz'.prependIfMissing('XYZabc', ?) ==> "xyzXYZabc"

#### 29. prependIfMissingIgnoreCase()

    'abc'.prependIfMissingIgnoreCase('xyz') ==> "xyzabc"

    'abc'.prependIfMissingIgnoreCase(?, 'xyz') ==> "xyzabc"

    prependIfMissingIgnoreCase('xyzabc', 'xyz') ==> "xyzabc"

    'xyz'.prependIfMissingIgnoreCase('XYZabc', ?) ==> "XYZabc"

#### 30. removeEnd()

    'www.domain.com'.removeEnd('.com') ==> "www.domain"

    'www.domain.com'.removeEnd(?, '.Com') ==> "www.domain.com"

    removeEnd('www.domain.com', '.com') ==> "www.domain"

#### 31. removeEndIgnoreCase()

    'www.domain.COM'.removeEndIgnoreCase('.com') ==> "www.domain"

    'www.domain.com'.removeEndIgnoreCase(?, '.Com') ==> "www.domain"

    removeEndIgnoreCase('www.domain.com', '.COM') ==> "www.domain"

#### 32. removeStart()

    'www.domain.com'.removeStart('www.') ==> "domain.com"

    'www.domain.com'.removeStart(?, '.Www') ==> "www.domain.com"

    removeStart('www.domain.com', 'www.') ==> "domain.com"

#### 33. removeStartIgnoreCase()

    'WWW.domain.com'.removeStartIgnoreCase('www.') ==> "domain.com"

    'www.domain.com'.removeStartIgnoreCase(?, '.Www') ==> "www.domain.com"

    removeStartIgnoreCase('www.domain.com', 'WWW.') ==> "domain.com"

#### 34. repeat()

    'a'.repeat(3) ==> "aaa"

    'ab'.repeat(?, 2) ==> "abab"

    repeat('abc', 2) ==> "abcabc"

    3.repeat('abc', ?) ==> "abcabcabc"

#### 35. replace()

    'abaa'.replace('a', 'z') ==> "zbzz"

    'abaa'.replace(?, 'a', 'z', -1) ==> "zbzz"

    replace('abaa', 'a', '', -1) ==> "b"

    replace('abaa', 'A', 'z', 1) ==> "abaa"

    'a'.replace('abaa', ?, 'z', 2) ==> "zbza"

#### 36. replaceIgnoreCase()

    'abaa'.replaceIgnoreCase('a', 'z') ==> "zbzz"

    'abaa'.replaceIgnoreCase(?, 'a', 'z', -1) ==> "zbzz"

    replaceIgnoreCase('abaa', 'a', '', -1) ==> "b"

    replaceIgnoreCase('abaa', 'A', 'z', 1) ==> "zbaa"

    'a'.replaceIgnoreCase('abaa', ?, 'z', 2) ==> "zbza"

#### 37. rightPad()

    'bat'.rightPad(5) ==> "bat  "

    'bat'.rightPad(?, 8, 'yz') ==> "batyzyzy"

    rightPad('bat', 3, 'yz') ==> "bat"

    rightPad('bat', 5, '') ==> "bat  "

#### 38. split()

    'abc def'.split() ==> [ "abc", "def" ]

    'abc  def'.split(' ') ==> [ "abc", "def" ]

    ' abc  def '.split(?, ' ') ==> [ "abc", "def" ]

    split('ab:cd:ef', ':') ==> [ "ab", "cd", "ef" ]

#### 39. strip()

    '  abc  '.strip(' ') ==> "abc"

    '  abcyx'.strip('xyz') ==> "  abc"

    strip('z abcyx', 'xyz') ==> " abc"

#### 40. stripEnd()

    '  abc  '.stripEnd(' ') ==> "  abc"

    'z abcyx'.stripEnd('xyz') ==> "z abc"

    stripEnd('z abcyx', 'xyz') ==> "z abc"

#### 41. stripStart()

    '  abc  '.stripStart(' ') ==> "abc  "

    'z abcyx'.stripStart('xyz') ==> " abcyx"

    stripStart('z abcyx', 'xyz') ==> " abcyx"

#### 42. substr()

    'abc'.substr(1) ==> "bc"

    'abc'.substr(0, 2) ==> "ab"

    'abc'.substr(?, 1, 2) ==> "b"

    substr('abc', -2, -1) ==> "b"

    2.substr('abc', -4, ?) ==> "ab"

#### 43. trim()

    'abc'.trim() ==> "abc"

    trim('  abc  ') ==> "abc"

#### 44. uncapitalize()

    'Cat'.uncapitalize() ==> "cat"

    uncapitalize('CAt') ==> "cAt"

#### 45. upperCase()

    'Cat'.upperCase() ==> "CAT"

    upperCase('cAt') ==> "CAT"

#### 46. singleQuote() / quote()

    'Peggy''s cat'.singleQuote() ==> "'Peggy''s cat'"
    123.singleQuote() ==> "'123'"
    quote('Raymond''s dog') ==> "'Raymond''s dog'"
    quote(True) ==> "'true'"

#### 47. doubleQuote()

    'Peggy\"s cat'.doubleQuote() ==> "\"Peggy\\\"s cat\""
    12.3.doubleQuote() ==> "\"12.3\""
    doubleQuote('Raymond\"s dog') ==> "\"Raymond\\\"s dog\""
    doubleQuote(False) ==> "\"false\""

### Date Functions

#### 48. amPmOfDay()

    '2022-01-02T03:04:05'.amPmOfDay() ==> "AM"

    amPmOfDay('2022-02-04T13:14:15') ==> "PM"

#### 49. second()

    '2022-01-02T03:04:05'.second() ==> 5

    second('2022-02-04T13:14:15') ==> 15

#### 50. secondOfDay()

    '2022-01-02T03:04:05'.secondOfDay() ==> 11045

    secondOfDay('2022-02-04T13:14:15') ==> 47655

#### 51. minute()

    '2022-01-02T03:04:05'.minute() ==> 4

    minute('2022-02-04T13:14:15') ==> 14

#### 52. minuteOfDay()

    '2022-01-02T03:04:05'.minuteOfDay() ==> 184

    minuteOfDay('2022-02-04T13:14:15') ==> 794

#### 53. hourOfAmPm()

    '2022-01-02T03:04:05'.hourOfAmPm() ==> 3

    hourOfAmPm('2022-02-04T13:14:15') ==> 1

#### 54. hour()

    '2022-01-02T03:04:05'.hour() ==> 3

    hour('2022-02-04T13:14:15') ==> 13

#### 55. dayOfWeek()

    '2022-01-02T03:04:05'.dayOfWeek() ==> 7

    dayOfWeek('2022-02-04T13:14:15') ==> 5

#### 56. day()

    '2022-01-02T03:04:05'.day() ==> 2

    day('2022-02-04T13:14:15') ==> 4

#### 57. dayOfYear()

    '2022-01-02T03:04:05'.dayOfYear() ==> 2

    dayOfYear('2022-02-04T13:14:15') ==> 35

#### 58. month()

    '2022-01-02T03:04:05'.month() ==> 1

    month('2022-02-04T13:14:15') ==> 2

#### 59. year()

    '2022-01-02T03:04:05'.year() ==> 2022

    year('2022-02-04T13:14:15') ==> 2022

#### 60. plusSeconds()

    '2022-01-02T03:04:05'.plusSeconds(9) ==> "2022-01-02T03:04:14"

    '2022-01-02T03:04:05'.plusSeconds(?, 10) ==> "2022-01-02T03:04:15"

    plusSeconds('2022-02-04T13:14:15', 9) ==> "2022-02-04T13:14:24"

#### 61. plusMinutes()

    '2022-01-02T03:04:05'.plusMinutes(9) ==> "2022-01-02T03:13:05"

    '2022-01-02T03:04:05'.plusMinutes(?, 10) ==> "2022-01-02T03:14:05"

    plusMinutes('2022-02-04T13:14:15', 9) ==> "2022-02-04T13:23:15"

#### 62. plusHours()

    '2022-01-02T03:04:05'.plusHours(9) ==> "2022-01-02T12:04:05"

    '2022-01-02T03:04:05'.plusHours(?, 10) ==> "2022-01-02T13:04:05"

    plusHours('2022-02-04T13:14:15', 9) ==> "2022-02-04T22:14:15"

#### 63. plusDays()

    '2022-01-02T03:04:05'.plusDays(9) ==> "2022-01-11T03:04:05"

    '2022-01-02T03:04:05'.plusDays(?, 10) ==> "2022-01-12T03:04:05"

    plusDays('2022-02-04T13:14:15', 9) ==> "2022-02-13T13:14:15"

#### 64. plusWeeks()

    '2022-01-02T03:04:05'.plusWeeks(9) ==> "2022-03-06T03:04:05"

    '2022-01-02T03:04:05'.plusWeeks(?, 10) ==> "2022-03-13T03:04:05"

    plusWeeks('2022-02-04T13:14:15', 9) ==> "2022-04-08T13:14:15"

#### 65. plusMonths()

    '2022-01-02T03:04:05'.plusMonths(9) ==> "2022-10-02T03:04:05"

    '2022-01-02T03:04:05'.plusMonths(?, 10) ==> "2022-11-02T03:04:05"

    plusMonths('2022-02-04T13:14:15', 9) ==> "2022-11-04T13:14:15"

#### 66. plusYears()

    '2022-01-02T03:04:05'.plusYears(9) ==> "2031-01-02T03:04:05"

    '2022-01-02T03:04:05'.plusYears(?, 10) ==> "2032-01-02T03:04:05"

    plusYears('2022-02-04T13:14:15', 9) ==> "2031-02-04T13:14:15"

#### 67. minusSeconds()

    '2022-01-02T03:04:05'.minusSeconds(9) ==> "2022-01-02T03:03:56"

    '2022-01-02T03:04:05'.minusSeconds(?, 10) ==> "2022-01-02T03:03:55"

    minusSeconds('2022-02-04T13:14:15', 9) ==> "2022-02-04T13:14:06"

#### 68. minusMinutes()

    '2022-01-02T03:04:05'.minusMinutes(9) ==> "2022-01-02T02:55:05"

    '2022-01-02T03:04:05'.minusMinutes(?, 10) ==> "2022-01-02T02:54:05"

    minusMinutes('2022-02-04T13:14:15', 9) ==> "2022-02-04T13:05:15"

#### 69. minusHours()

    '2022-01-02T03:04:05'.minusHours(9) ==> "2022-01-01T18:04:05"

    '2022-01-02T03:04:05'.minusHours(?, 10) ==> "2022-01-01T17:04:05"

    minusHours('2022-02-04T13:14:15', 9) ==> "2022-02-04T04:14:15"

#### 70. minusDays()

    '2022-01-02T03:04:05'.minusDays(9) ==> "2021-12-24T03:04:05"

    '2022-01-02T03:04:05'.minusDays(?, 10) ==> "2021-12-23T03:04:05"

    minusDays('2022-02-04T13:14:15', 9) ==> "2022-01-26T13:14:15"

#### 71. minusWeeks()

    '2022-01-02T03:04:05'.minusWeeks(9) ==> "2021-10-31T03:04:05"

    '2022-01-02T03:04:05'.minusWeeks(?, 10) ==> "2021-10-24T03:04:05"

    minusWeeks('2022-02-04T13:14:15', 9) ==> "2021-12-03T13:14:15"

#### 72. minusMonths()

    '2022-01-02T03:04:05'.minusMonths(9) ==> "2021-04-02T03:04:05"

    '2022-01-02T03:04:05'.minusMonths(?, 10) ==> "2021-03-02T03:04:05"

    minusMonths('2022-02-04T13:14:15', 9) ==> "2021-05-04T13:14:15"

#### 73. minusYears()

    '2022-01-02T03:04:05'.minusYears(9) ==> "2013-01-02T03:04:05"

    '2022-01-02T03:04:05'.minusYears(?, 10) ==> "2012-01-02T03:04:05"

    minusYears('2022-02-04T13:14:15', 9) ==> "2013-02-04T13:14:15"

#### 74. truncateToMicro()

    '2022-01-02T03:04:05.229390600'.truncateToMicro() ==> "2022-01-02T03:04:05.229390"

    truncateToMicro('2022-02-04T13:14:15.229390600') ==> "2022-02-04T13:14:15.229390"

#### 75. truncateToMilli()

    '2022-01-02T03:04:05.229390600'.truncateToMilli() ==> "2022-01-02T03:04:05.229"

    truncateToMilli('2022-02-04T13:14:15.229390600') ==> "2022-02-04T13:14:15.229"

#### 76. truncateToSecond()

    '2022-01-02T03:04:05.229390600'.truncateToSecond() ==> "2022-01-02T03:04:05"

    truncateToSecond('2022-02-04T13:14:15.229390600') ==> "2022-02-04T13:14:15"

#### 77. truncateToMinute()

    '2022-01-02T03:04:05.229390600'.truncateToMinute() ==> "2022-01-02T03:04"

    truncateToMinute('2022-02-04T13:14:15.229390600') ==> "2022-02-04T13:14"

#### 78. truncateToHour()

    '2022-01-02T03:04:05.229390600'.truncateToHour() ==> "2022-01-02T03:00"

    truncateToHour('2022-02-04T13:14:15.229390600') ==> "2022-02-04T13:00"

#### 79. truncateToDay()

    '2022-01-02T03:04:05.229390600'.truncateToDay() ==> "2022-01-02T00:00"

    truncateToDay('2022-02-04T13:14:15.229390600') ==> "2022-02-04T00:00"

#### 80. truncateToMonth()

    '2022-01-02T03:04:05.229390600'.truncateToMonth() ==> "2022-01-01T00:00"

    truncateToMonth('2022-02-04T13:14:15.229390600') ==> "2022-02-01T00:00"

#### 81. truncateToYear()

    '2022-01-02T03:04:05.229390600'.truncateToYear() ==> "2022-01-01T00:00"

    truncateToYear('2022-02-04T13:14:15.229390600') ==> "2022-01-01T00:00"

#### 82. withNano()

    '2022-01-02T03:04'.withNano(789) ==> "2022-01-02T03:04:00.000000789"

    '2022-01-02T03:04'.withNano(?, 789) ==> "2022-01-02T03:04:00.000000789"

    withNano('2022-02-04T13:14', 789) ==> "2022-02-04T13:14:00.000000789"

#### 83. withMicro()

    '2022-01-02T03:04'.withMicro(789) ==> "2022-01-02T03:04:00.000789"

    '2022-01-02T03:04'.withMicro(?, 789) ==> "2022-01-02T03:04:00.000789"

    withMicro('2022-02-04T13:14', 789) ==> "2022-02-04T13:14:00.000789"

#### 84. withMilli()

    '2022-01-02T03:04'.withMilli(789) ==> "2022-01-02T03:04:00.789"

    '2022-01-02T03:04'.withMilli(?, 789) ==> "2022-01-02T03:04:00.789"

    withMilli('2022-02-04T13:14', 789) ==> "2022-02-04T13:14:00.789"

#### 85. withSecond()

    '2022-01-02T03:04'.withSecond(35) ==> "2022-01-02T03:04:35"

    '2022-01-02T03:04'.withSecond(?, 35) ==> "2022-01-02T03:04:35"

    withSecond('2022-02-04T13:14', 35) ==> "2022-02-04T13:14:35"

#### 86. withMinute()

    '2022-01-02T03:04'.withMinute(35) ==> "2022-01-02T03:35"

    '2022-01-02T03:04'.withMinute(?, 35) ==> "2022-01-02T03:35"

    withMinute('2022-02-04T13:14', 35) ==> "2022-02-04T13:35"

#### 87. withHour()

    '2022-01-02T03:04'.withHour(16) ==> "2022-01-02T16:04"

    '2022-01-02T03:04'.withHour(?, 16) ==> "2022-01-02T16:04"

    withHour('2022-02-04T13:14', 16) ==> "2022-02-04T16:14"

#### 88. withDay()

    '2022-01-02T03:04'.withDay(25) ==> "2022-01-25T03:04"

    '2022-01-02T03:04'.withDay(?, 25) ==> "2022-01-25T03:04"

    withDay('2022-02-04T13:14', 25) ==> "2022-02-25T13:14"

#### 89. withDayOfYear()

    '2022-01-02T03:04'.withDayOfYear(123) ==> "2022-05-03T03:04"

    '2022-01-02T03:04'.withDayOfYear(?, 123) ==> "2022-05-03T03:04"

    withDayOfYear('2022-02-04T13:14', 123) ==> "2022-05-03T13:14"

#### 90. withMonth()

    '2022-01-02T03:04'.withMonth(7) ==> "2022-07-02T03:04"

    '2022-01-02T03:04'.withMonth(?, 7) ==> "2022-07-02T03:04"

    withMonth('2022-02-04T13:14', 7) ==> "2022-07-04T13:14"

#### 91. withYear()

    '2022-01-02T03:04'.withYear(2047) ==> "2047-01-02T03:04"

    '2022-01-02T03:04'.withYear(?, 2047) ==> "2047-01-02T03:04"

    withYear('2022-02-04T13:14', 2047) ==> "2047-02-04T13:14"

#### 92. dayEnd()

    '2022-01-02T03:04'.dayEnd() ==> "2022-01-02T23:59:59.999999999"

    dayEnd('2022-02-04T13:14') ==> "2022-02-04T23:59:59.999999999"

#### 93. monthEnd()

    '2022-01-02T03:04'.monthEnd() ==> "2022-01-31T23:59:59.999999999"

    monthEnd('2022-02-04T13:14') ==> "2022-02-28T23:59:59.999999999"

#### 94. yearEnd()

    '2022-01-02T03:04'.yearEnd() ==> "2022-12-31T23:59:59.999999999"

    yearEnd('2022-02-04T13:14') ==> "2022-12-31T23:59:59.999999999"

#### 95. lengthOfMonth()

    '2022-01-02T03:04'.lengthOfMonth() ==> 31

    lengthOfMonth('2022-02-04T13:14') ==> 28

#### 96. lengthOfYear()

    '2022-01-02T03:04'.lengthOfYear() ==> 365

    lengthOfYear('2024-02-04T13:14') ==> 366

#### 97. localToOffsetDate()

    '2022-01-02T03:04:05'.localToOffsetDate() ==> "2022-01-02T03:04:05+08:00"

    localToOffsetDate('2022-02-04T13:14:15') ==> "2022-02-04T13:14:15+08:00"

#### 98. offsetToLocalDate()

    '2022-01-02T03:04:05+08:00'.offsetToLocalDate() ==> "2022-01-02T03:04:05"

    offsetToLocalDate('2022-02-04T13:14:15+08:00') ==> "2022-02-04T13:14:15"

### Format Functions

#### 99. b64Encode()

    'abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ'.b64Encode()
    ==>
    "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg=="

#### 100. b64EncodeNoPadding()

    b64EncodeNoPadding('abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ')
    ==>
    "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg"

#### 101. b64MimeEncode()

    'abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ'.b64MimeEncode()
    ==>
    "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9Q\r\nUVJTVFVWV1hZWg=="

#### 102. b64MimeEncodeNoPadding()

    b64MimeEncodeNoPadding('abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ')
    ==>
    "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9Q\r\nUVJTVFVWV1hZWg"

#### 103. b64UrlEncode()

    'abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ'.b64UrlEncode()
    ==>
    "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp-IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg=="

#### 104. b64UrlEncodeNoPadding()

    b64UrlEncodeNoPadding('abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ')
    ==>
    "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp-IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg"

#### 105. b64Decode()

    'YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg=='.b64Decode()
    ==>
    "abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    b64Decode('YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg')
    ==>
    "abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ"

#### 106. b64MimeDecode()

    'YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9Q\nUVJTVFVWV1hZWg=='.b64MimeDecode()
    ==>
    "abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    b64MimeDecode('YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp+IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9Q\r\nUVJTVFVWV1hZWg')
    ==>
    "abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ"

#### 107. b64UrlDecode()

    'YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp-IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg=='.b64UrlDecode()
    ==>
    "abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    b64UrlDecode('YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXp-IUAjJCVeJiooKV8rLT1BQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWg')
    ==>
    "abcdefghijklmnopqrstuvwxyz~!@#$%^&*()_+-=ABCDEFGHIJKLMNOPQRSTUVWXYZ"

#### 108. urlEncode()

    'www.domain.com?a=1+2&b=3+4'.urlEncode() ==> "www.domain.com%3Fa%3D1%2B2%26b%3D3%2B4"

    urlEncode('www.domain.com?a=1+2&b=3+4') ==> "www.domain.com%3Fa%3D1%2B2%26b%3D3%2B4"

#### 109. urlDecode()

    'www.domain.com%3Fa%3D1%2B2%26b%3D3%2B4'.urlDecode() ==> "www.domain.com?a=1+2&b=3+4"

    urlDecode('www.domain.com%3Fa%3D1%2B2%26b%3D3%2B4') ==> "www.domain.com?a=1+2&b=3+4"

#### 110. if()

    json('{"a":1,"b":2,"c":3}').if(a.isEven(), 'T', 'F') ==> "F"

    json('{"a":1,"b":2,"c":3}').if([a=1], 'T', 'F') ==> "T"

    json('{"a":1,"b":2,"c":3}').if([a=1 & b=3], 'T', 'F') ==> "F"

    json('{"a":1,"b":2,"c":3}').if([a=1 & b=3], 'T') ==> !unresolvable!

    json('{"a":1,"b":2,"c":3}').if([a=b], 'T', if([c=3], 'C', 'F')) ==> "C"

    json('[1,2,3,4,5]').if(?.isOdd(), calc(?*2), ?) ==> [ 2.0, 2, 6.0, 4, 10.0 ]

#### 111. caseValue()

    'a'.caseValue('c',1,'b',2,'a',3,4) ==> 3

    'z'.caseValue('c',1,'b',2,'a',3,4) ==> 4

    'z'.caseValue('c',1,'b',2,'a',3) ==> !unresolvable!

    json('[{"s":1},{"s":null},{"s":3}]').s.caseValue(1,'A',null,'B') ==> [ "A", "B", null ]

#### 112. cycleValue()

    0.cycleValue('a','b','c','d') ==> "a"

    1.cycleValue(json('["a","b","c","d"]')) ==> "b"

    '3'.cycleValue('a','b','c','d') ==> "d"

    4.cycleValue('a','b','c','d') ==> "a"

    -1.cycleValue('a','b','c','d') ==> "d"

    -6.cycleValue('a','b','c','d') ==> "c"

#### 113. indexedValue()

    0.indexedValue('a','b','c','d') ==> "a"

    1.indexedValue(json('["a","b","c","d"]')) ==> "b"

    '3'.indexedValue('a','b','c','d') ==> "d"

    4.indexedValue('a','b','c','d') ==> !unresolvable!

    -1.indexedValue('a','b','c','d') ==> !unresolvable!

#### 114. formatDate()

    '2022-01-02T03:04:05'.formatDate('dd/MM/yyyy HH:mm:ss') ==> "02/01/2022 03:04:05"

    '2022-01-02T03:04:05'.formatDate(?, 'yyyy-MM-dd') ==> "2022-01-02"

    formatDate('2022-01-02T03:04:05', 'EEE, MMM d, yyyy') ==> "Sun, Jan 2, 2022"

#### 115. formatNumber()

    12345.6.formatNumber('HK$#,##0.00') ==> "HK$12,345.60"

    123.formatNumber(?, '#,##0.#') ==> "123"

    formatNumber(123.45, '#,##0.#') ==> "123.5"

#### 116. formatText()

    'Dog'.formatText('[%-5s]') ==> "[Dog  ]"

    123.formatText(?, '[%5d]') ==> "[  123]"

    formatText('Dog', '[%5s]') ==> "[  Dog]"

#### 117. formatTexts()

    formatTexts('1:%s 2:%s 3:%s', 'a', 'b', 'c') ==> "1:a 2:b 3:c"

    'b'.formatTexts('1:%s 2:%s 3:%s', 'a', ?, 'c') ==> "1:a 2:b 3:c"

    json('{"A":"a","B":"b"}').formatTexts('1:%s 2:%s 3:%s', A, B, 'c') ==> "1:a 2:b 3:c"

#### 118. toNumber()

    '123'.toNumber() ==> 123.0

    toNumber('abc') ==> 0.0

    toNumber(true) ==> 1.0

    toNumber(null) ==> !unresolvable!

    toNumber(json('{"a":1}')) ==> !unresolvable!

    toNumber(json('[1,2.0,"a",true,null]')) ==> [ 1, 2.0, 0.0, 1.0, null ]

#### 119. toString()

    123.toString() ==> "123"

    toString(false) ==> "false"

    toString(null) ==> "null"

    toString(json('{"a":1}')) ==> "{\"a\":1}"

    toString(json('[1,2.0,"a",true,null]')) ==> "[1,2.0,\"a\",true,null]"

#### 120. toText()

    123.toText() ==> "123"

    toText(false) ==> "false"

    toText(null) ==> "null"

    toText(json('{"a":1}')) ==> !unresolvable!

    toText(json('[1,2.0,"a",true,null]')) ==> [ "1", "2.0", "a", "true", "null" ]

#### 121. csv()

    json('{"len1":"12.3\\"","len2":null,"len3":"64.0\\""}').csv() ==> "12.3""",,"64.0"""

    csv(json('[[[[1,2],["3","4\\""]]],{"a":1,"b":[2.0,8.888],"c":{"d":true,"e":null}}]')) ==> 1,2,3,"4""",1,2.0,8.888,true,

#### 122. csvShowNull()

    json('{"len1":"12.3\\"","len2":null,"len3":"64.0\\""}').csvShowNull() ==> "12.3""",null,"64.0"""

    csvShowNull(json('[[[[1,2],["3","4\\""]]],{"a":1,"b":[2.0,8.888],"c":{"d":true,"e":null}}]')) ==> 1,2,3,"4""",1,2.0,8.888,true,null

### Logical Functions

#### 123. contains()

    'abcde'.contains('bc') ==> true

    contains('abcde','B') ==> false

    json('[1.0,2.8,3.0]').contains(?, '1') ==> false

    json('[1.0,2.8,3.0]').contains(1) ==> true

    contains(json('["1","2","3"]'), 2.0) ==> true

    contains(json('[1.0,2.8,3.00]'), '3.0') ==> true

    json('["1.0","2.0","3.0"]').contains(?, '3.0') ==> true

    json('[1,2,null,4]').contains(null) ==> true

    json('{"a":1,"b":2,"c":3}').contains('a') ==> true

#### 124. containsIgnoreCase()

    'abcde'.containsIgnoreCase('bc') ==> true

    containsIgnoreCase('abcde','B') ==> true

    json('["a","b","c"]').containsIgnoreCase(?, 'B') ==> true

    containsIgnoreCase(json('["a","b","c"]'), 'bc') ==> false

    json('{"a":1,"b":2,"c":3}').containsIgnoreCase('A') ==> true

#### 125. notContains()

    'abcde'.notContains('bc') ==> false

    notContains('abcde','B') ==> true

    json('[1.0,2.8,3.0]').notContains(?, 1) ==> false

    json('[1,2,null,4]').notContains(null) ==> false

#### 126. notContainsIgnoreCase()

    'abcde'.notContainsIgnoreCase('bc') ==> false

    notContainsIgnoreCase('abcde','B') ==> false

    json('["a","b","c"]').notContainsIgnoreCase(?, 'D') ==> true

#### 127. startsWith()

    'abcdef'.startsWith('abc') ==> true

    'ABCDEF'.startsWith(?,'abc') ==> false

    startsWith('ABCDEF','cde') ==> false

#### 128. startsWithIgnoreCase()

    'abcdef'.startsWithIgnoreCase('abc') ==> true

    'ABCDEF'.startsWithIgnoreCase(?,'abc') ==> true

    startsWithIgnoreCase('ABCDEF','cde') ==> false

#### 129. notStartsWith()

    'abcdef'.notStartsWith('abc') ==> false

    'ABCDEF'.notStartsWith(?,'abc') ==> true

    notStartsWith('ABCDEF','cde') ==> true

#### 130. notStartsWithIgnoreCase()

    'abcdef'.notStartsWithIgnoreCase('abc') ==> false

    'ABCDEF'.notStartsWithIgnoreCase(?,'abc') ==> false

    notStartsWithIgnoreCase('ABCDEF','cde') ==> true

#### 131. endsWith()

    'abcdef'.endsWith('def') ==> true

    'ABCDEF'.endsWith(?,'def') ==> false

    endsWith('ABCDEF','cde') ==> false

#### 132. endsWithIgnoreCase()

    'abcdef'.endsWithIgnoreCase('def') ==> true

    'ABCDEF'.endsWithIgnoreCase(?,'def') ==> true

    endsWithIgnoreCase('ABCDEF','cde') ==> false

#### 133. notEndsWith()

    'abcdef'.notEndsWith('def') ==> false

    'ABCDEF'.notEndsWith(?,'def') ==> true

    notEndsWith('ABCDEF','cde') ==> true

#### 134. notEndsWithIgnoreCase()

    'abcdef'.notEndsWithIgnoreCase('def') ==> false

    'ABCDEF'.notEndsWithIgnoreCase(?,'def') ==> false

    notEndsWithIgnoreCase('ABCDEF','cde') ==> true

#### 135. equals()

    'abc'.equals('abc') ==> true

    'abc'.equals(?,' abc') ==> false

    equals('ABC','abc') ==> false

#### 136. equalsIgnoreCase()

    'abc'.equalsIgnoreCase('abc') ==> true

    'abc'.equalsIgnoreCase(?,' abc') ==> false

    equalsIgnoreCase('ABC','abc') ==> true

#### 137. notEquals()

    'abc'.notEquals('abc') ==> false

    'abc'.notEquals(?, ' abc') ==> true

    notEquals('ABC','abc') ==> true

#### 138. notEqualsIgnoreCase()

    'abc'.notEqualsIgnoreCase('abcd') ==> true

    'abc'.notEqualsIgnoreCase(' abc') ==> true

    notEqualsIgnoreCase('ABC','abc') ==> false

#### 139. matches()

    '123a'.matches('^[0-9]+$') ==> false

    '784238'.matches(?,'^[0-9]+$') ==> true

    matches('63 56','^[0-9]+$') ==> false

#### 140. notMatches()

    '1234-123456'.notMatches('\\d{4}-\\d{6}') ==> false

    '888-123456'.notMatches(?,'\\d{4}-\\d{6}') ==> true

    notMatches('4444-5555','\\d{4}-\\d{6}') ==> true

#### 141. in()

    56.in(12,34,56) ==> true

    '56'.in(12,34,56) ==> true

    'A'.in(json('["a","b","c"]')) ==> false

#### 142. inIgnoreCase()

    'A'.inIgnoreCase('a','b','c') ==> true

    'a '.inIgnoreCase('a','b','c') ==> false

#### 143. notIn()

    56.notIn(12,34,56) ==> false

    '56'.notIn(12,34,56) ==> false

    'A'.notIn(json('["a","b","c"]')) ==> true

#### 144. notInIgnoreCase()

    'A'.notInIgnoreCase('a','b','c') ==> false

    'a '.notInIgnoreCase('a','b','c') ==> true

#### 145. isEmpty()

    ''.isEmpty() ==> true

    isEmpty(' ') ==> false

    isEmpty(1) ==> false

    isEmpty(0) ==> false

    isEmpty(true) ==> false

    isEmpty(false) ==> false

    isEmpty(null) ==> true

    json('[]').isEmpty() ==> true

    isEmpty(json('[0]')) ==> false

    json('{}').isEmpty() ==> true

    isEmpty(json('{"a":1}')) ==> false

#### 146. isNotEmpty()

    ''.isNotEmpty() ==> false

    isNotEmpty(' ') ==> true

    isNotEmpty(1) ==> true

    isNotEmpty(0) ==> true

    isNotEmpty(true) ==> true

    isNotEmpty(false) ==> true

    isNotEmpty(null) ==> false

    json('[]').isNotEmpty() ==> false

    isNotEmpty(json('[0]')) ==> true

    json('{}').isNotEmpty() ==> false

    isNotEmpty(json('{"a":1}')) ==> true

#### 147. isBlank()

    ''.isBlank() ==> true

    isBlank(' ') ==> true

#### 148. isNotBlank()

    ''.isNotBlank() ==> false

    isNotBlank(' ') ==> false

#### 149. isNull()

    null.isNull() ==> !unresolvable!

    isNull(null) ==> true

    isNull('') ==> false

#### 150. isNotNull()

    null.isNotNull() ==> !unresolvable!

    isNotNull(null) ==> false

    isNotNull('') ==> true

#### 151. isText()

    'text'.isText() ==> true

    isText(1) ==> false

    isText(true) ==> false

    isText(null) ==> false

#### 152. isBoolean()

    'text'.isBoolean() ==> false

    isBoolean(1) ==> false

    isBoolean(true) ==> true

    isBoolean(null) ==> false

#### 153. isNumber()

    'text'.isNumber() ==> false

    isNumber(1) ==> true

    isNumber(true) ==> false

    isNumber(null) ==> false

#### 154. isEven()

    1.isEven() ==> false

    isEven(2) ==> true

#### 155. isOdd()

    1.isOdd() ==> true

    isOdd(2) ==> false

#### 156. not()

    true.not() ==> false

    not(false) ==> true

    not('false') ==> false

    not(0) ==> false

    not(null) ==> false

#### 157. isWeekday

    '2021-12-31T00:00:00'.isWeekday() ==> true

    isWeekday('2022-01-01T00:00:00') ==> false

#### 158. isWeekend

    '2021-12-31T00:00:00'.isWeekend() ==> false

    isWeekend('2022-01-01T00:00:00') ==> true

#### 159. isLeapYear

    '2020-12-31T00:00:00'.isLeapYear() ==> true

    isLeapYear('2022-01-01T00:00:00') ==> false

### Array Functions

#### 160. size()

    json('[7,1,9,null,5,3]').size() ==> 6

    size(json('[7,1,9,null,5,3]')) ==> 6

#### 161. lastIndex()

    json('[7,1,9,null,5,3]').lastIndex() ==> 5

    lastIndex(json('[7,1,9,null,5,3]')) ==> 5

#### 162. indexOf()

    json('[1,1,3,5,null,3,7,3,9]').indexOf(3) ==> 2

    json('[1,1,3,5,null,3,7,3,9]').indexOf(?, 1) ==> 0

    indexOf(json('[1,1,3,5,null,3,7,3,9]'), null) ==> 4

#### 163. lastIndexOf()

    json('[1,1,3,5,null,3,7,3,9]').lastIndexOf(3) ==> 7

    json('[1,1,3,5,null,3,7,3,9]').lastIndexOf(?, 1) ==> 1

    lastIndexOf(json('[1,1,3,5,null,3,7,3,9]'), null) ==> 4

#### 164. first()

    json('[7,1,9,null,5,3]').first() ==> 7

    first(json('[null,7,1,9,5,3]')) ==> null

#### 165. last()

    json('[7,1,9,null,5,3]').last() ==> 3

    last(json('[7,1,9,5,3,null]')) ==> null

#### 166. max()

    json('[7,1,9,null,5,3]').max() ==> 9

    max(json('[7,1,9,null,5,3]'), 15, 16) ==> 16

#### 167. min()

    json('[7,1,9,null,5,3]').min() ==> 1

    min(json('[7,1,9,null,5,3]'), 15, 16) ==> 1

#### 168. sum()

    json('[7,1,9,null,5,3]').sum() ==> 25.0

    sum(json('[7,1,9,null,5,3]'), 15, 16) ==> 56.0

#### 169. avg()

    json('[7,1,9,null,5,3]').avg() ==> 5.0

    avg(json('[7,1,9,null,5,3]'), 15, 16) ==> 8.0

#### 170. count()

    json('[7,1,9,null,5,3]').count() ==> 5

    count(json('[7,1,9,null,5,3]'), 15, 16) ==> 7

#### 171. reverse()

    json('[7,1,9,null,5,3]').reverse() ==> [ 3, 5, null, 9, 1, 7 ]

    reverse(json('[7,1,9,null,5,3]')) ==> [ 3, 5, null, 9, 1, 7 ]

#### 172. slice()

    json('[1,2,3,4,5,6,7,8,9]').slice(3) ==> [ 4, 5, 6, 7, 8, 9 ]

    json('[1,2,3,4,5,6,7,8,9]').slice(2,8) ==> [ 3, 4, 5, 6, 7, 8 ]

    json('[1,2,3,4,5,6,7,8,9]').slice(,5) ==> [ 1, 2, 3, 4, 5 ]

    json('[1,2,3,4,5,6,7,8,9]').slice(-5) ==> [ 5, 6, 7, 8, 9 ]

    json('[1,2,3,4,5,6,7,8,9]').slice(?,1,8,2) ==> [ 2, 4, 6, 8 ]

    json('[1,2,3,4,5,6,7,8,9]').slice(?,2,,2) ==> [ 3, 5, 7, 9 ]

    slice(json('[1,2,3,4,5,6,7,8,9]'),6,2,1) ==> [ 7, 6, 5, 4 ]

    slice(json('[1,2,3,4,5,6,7,8,9]'),,,3) ==> [ 1, 4, 7 ]

    slice(json('[1,2,3,4,5,6,7,8,9]'),,-5,1) ==> [ 1, 2, 3, 4 ]

#### 173. sort()

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

#### 174. distinct()

    json('[1,1,3,5,3,7,3,9]').distinct().sort() ==> [ 1.0, 3.0, 5.0, 7.0, 9.0 ]

    distinct(json('["A","Z","a","Z","A","z"]')) ==> [ "A", "a", "Z", "z" ]

    distinct(json('["1","1.0",1,1.0,1.00,true,"true",null,"null"]')) ==> [ "1", "1.0", "null", "true", 1.0, true ]

#### 175. join()

    json('["Hello", ",", "World", "!"]').join() ==> "Hello,World!"

    json('[1,2,3]').join('+') ==> "1+2+3"

    join(json('["A",1,"B","2.00","C",3.00,"D",true,null]'),'/') ==> "A/1/B/2.00/C/3.0/D/true"

#### 176. findByMax()

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

#### 177. findByMin()

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

#### 178. findByNullOrMax()

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

#### 179. findByNullOrMin()

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

#### 180. findByMaxOrNull()

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

#### 181. findByMinOrNull()

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

#### 182. json()

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

#### 183. entries()

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

#### 184. keys()

    json('{"a":1,"b":[2,3],"c":{"d":4,"e":5}}').keys() ==> [ "a", "b", "c" ]

    json('{"a":1,"b":[2,3],"c":{"d":4,"e":5}}').keys(2) ==> [ "a", "b", "c", "d", "e" ]

    keys(json('{"a":1,"b":[2,3],"c":{"d":4,"e":5}}'), -1) ==> [ "a", "b", "c", "d", "e" ]

#### 185. toArray()

    json('{"a":1,"b":[2,3],"c":{"d":4,"e":5}}').toArray()
    ==>
    [ 1, [ 2, 3 ], {
      "d" : 4,
      "e" : 5
    } ]

    json('{"a":1,"b":[2,3],"c":{"d":4,"e":5}}').toArray(c) ==> [ 4, 5 ]

    toArray(json('{"a":1,"b":[2,3],"c":{"d":4,"e":5}}').toArray()) ==> [ 1, 2, 3, 4, 5 ]

#### 186. flatten()

    json('[[[[1,2],[3,4]],[[5,6],[7,8]]],[[[9,10],[11,12]],[[13,14],[15,16]]]]').flatten()
    ==>
    [ [ [ 1, 2 ], [ 3, 4 ] ], [ [ 5, 6 ], [ 7, 8 ] ], [ [ 9, 10 ], [ 11, 12 ] ], [ [ 13, 14 ], [ 15, 16 ] ] ]

    json('[[[[1,2],[3,4]],[[5,6],[7,8]]],[[[9,10],[11,12]],[[13,14],[15,16]]]]').flatten(2)
    ==>
    [ [ 1, 2 ], [ 3, 4 ], [ 5, 6 ], [ 7, 8 ], [ 9, 10 ], [ 11, 12 ], [ 13, 14 ], [ 15, 16 ] ]

    flatten(json('[[[[1,2],[3,4]],[[5,6],[7,8]]],[[[9,10],[11,12]],[[13,14],[15,16]]]]'), 3)
    ==>
    [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 ]

#### 187. map()

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

#### 188. field()

    json('{"a":1,"b":[2,3],"c":{"d":4,"e":5}}').field(f:6,c:)
    ==>
    {
      "a" : 1,
      "b" : [ 2, 3 ],
      "f" : 6
    }

#### 189. coalesce()

    json('["abc","",123,false,null]').coalesce('xyz') ==> [ "abc", "", 123, false, "xyz" ]

    json('{"a":null,"c":"abc"}').coalesce(a,b,c,'xyz') ==> "abc"

---

## Jossons Basic

Jossons stores JSON datasets in a map of type `Map<String, Josson>` for placeholder resolution.

To create a Jossons object with given Jackson ObjectNode.
Each entry under root of the ObjectNode will become a member of the default dataset mapping.

    Jossons jossons = Jossons.create(jsonNode);

To create a Jossons object with given JSON string that deserialized to a Jackson ObjectNode.
Each entry under root of the ObjectNode will become a member of the default dataset mapping.

    Jossons jossons = Jossons.fromJsonString("...");

To create a Jossons object with given text-based dataset mapping `Map<String, String>`.

    Jossons jossons = Jossons.fromMap(mapping);

To create a Jossons object with given integer-based dataset mapping `Map<String, Integer>`.

    Jossons jossons = Jossons.fromMapOfInt(mapping);

To add more default dataset entry to a Jossons object afterward.

    jossons.putDataset("key", josson);

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

Example:

    |<----------------------------------------------- 3 -------------------------------->|
    |                  |<---------------------------- 2 ------------------------->|      |
    |                  |                     |<------ 1 ----->|                   |      |
    |                  |                     |                |                   |      |
    {{stock->[itemCode={{order->items[qrCode={{qrCode.quote()}}].itemCode.quote()}}].qty}}

1. `{{qrCode.quote()}}` is resolved to `'1234567890'`
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

       order->delivery.address->split()->[""->concat()]->join() ==>""

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

       order->items*->[{}->concat(##, $V...)]->join() ==>""

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

- A Jossons query with ternary syntax.

      "statement ? otherKey1->jossonQuery : otherKey2->jossonQuery"

- A database query statement, please refer to [Data Finder](#data-finder).

      "collectionName ? {findStatement}"

- A join operation query to merge two datasets, please refer to [Join Datasets](#join-datasets).

      "leftQuery{keyL1,keyL2...} <=< rightQuery{keyR1,keyR2...}"

All kinds of statement can contain implicit dictionary function parameter variables,
please refer to [Dictionary Function](#dictionary-function).

Resolved result will be cached with the key name except for key name starts with `$` or `*`.
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

### Join Datasets

Josson query works on single JSON dataset.
In order to let a placeholder output to include data from two datasets.
It is required to use join operation to build a new dataset for the placeholder.

There are two join types.

- _Join One_ - Find the first matched object node and merge the object elements.
- _Join Many_ - Find all matched nodes and embed into the object as an array node.

At least one matching key must be given and the number of key on both side must be the same.
Join operations match `keyL1` with `keyR1`, `keyL2` with `keyR2` and so on.

For Join Many operations, the `arrayName:` is optional.
If `arrayName` is not given, the last element name of the query is used.

- _Inner Join One_ `>=<`

      "leftQuery{keyL1,keyL2...} >=< rightQuery{keyR1,keyR2...}"

- _Left Join One_ `<=<`

      "leftQuery{keyL1,keyL2...} <=< rightQuery{keyR1,keyR2...}"

- _Right Join One_ `>=>`

      "leftQuery{keyL1,keyL2...} >=> rightQuery{keyR1,keyR2...}"

- _Left Join Many_ `<=<<`

      "leftQuery{keyL1,keyL2...} <=<< rightQuery{arrayName:keyR1,keyR2...}"

- _Right Join Many_ `>>=>`

      "leftQuery{arrayName:keyL1,keyL2...} >>=> rightQuery{keyR1,keyR2...}"

### Dictionary Function

If a `dictionaryFinder` key ends with `()`, then it is a dictionary function.
It's resolution statement can contain the following implicit variables.

- `$params` the calling statement's parameters in an array.
- `$0`, `$1`, `$2`... the calling statement's individual parameter naming in zero-based index.

#### Examples

_Dictionary finder entries_

    "double()" : "$0->calc(?*2)"

    "sum2num()" : "$->calc({{$0}} + {{$1}})"

    "sum2numThenDouble()" : "double({{sum2num({{$0}},{{$1}})}})->formatText('({{$0}}+{{$1}})x2 = %.1f')"

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

    Map<String, String> dictionaryFinder = new LinkedHashMap<>();
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

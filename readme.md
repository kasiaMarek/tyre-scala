TyRE - a typed regex parser
====

[![GitHub Actions Workflow Status](https://img.shields.io/github/actions/workflow/status/kasiaMarek/tyre-scala/ci.yml)](https://github.com/kasiaMarek/tyre-scala/actions/workflows/ci.yml)
[![Maven Central](https://img.shields.io/maven-central/v/net.marek/tyre-scala_3)](https://central.sonatype.com/artifact/net.marek/tyre-scala_3)
[![javadoc](https://javadoc.io/badge2/net.marek/tyre-scala_3/javadoc.svg?label=scaladoc&color=orange)](https://javadoc.io/doc/net.marek/tyre-scala_3)

*TyRE* provides the following features compared to standard Java regex matcher:
  1. compile time validation
    - employing Scala 3 macros, TyRE parses provided regex pattern at compile-time,
  1. syntax tree result
    - as a result of parsing TyRE returns a syntax tree, which at core is a combination on `Unit`, `Char`, `Tuple`, `List`, and `Either` types, but it can be freely transformed by the user.

Goals: Main goal of this library is to provide safer regex parsing compared to Java regex matcher. This is achieved through compile time regex validation and refined return type compared to `java.util.regex.Matcher` with its arbitrary number of capture groups and possible `null` captures.

Currently, *TyRE* is not production ready. However, we welcome you to download the released library and play around.

Understanding *TyRE*
---------------
`Tyre` is a type constructor parameterized by the type of the parse tree. The result of parsing a `word: String` using a `pattern: Tyre[R]` will be `Option[R]`, where `None` denotes that `word` did not match `pattern`.

*TyRE* patterns can be build through smart constructors. The following set of constructors is complete:
1. two primitive patterns,
    ```Scala
    // matches an empty string
    Tyre.epsilon: Tyre[Unit]
    // matches a single character satisfying predicate `f`
    Pred.pred(f: Char => Boolean): Tyre[Char]
    ```
1. three parser combinators,
    ```Scala
    // sequence of patterns
    Tyre[R]#<*>[S](re: Tyre[S]): Tyre[(R, S)]
    // alternative of patterns
    Tyre[R]#<|>[S](re: Tyre[S]): Tyre[Either[R, S]]
    // one or more repetitions of a pattern (Kleene star)
    Tyre[R]#rep: Tyre[List[R]]
    ```
1. and a map function (Tyre is a functor).
    ```Scala
    Tyre[R]#map[S](f: R => S): Tyre[S]
    ```

Please note that recursive definitions cannot be used.

Though these constructors can be used explicitly, it's usually more convenient to use string literals for creating patterns. The syntax of string literals is at large standard, its full description can be found in the [Supported syntax](#supported-syntax) section.

Basic usage
-----------
To use *TyRE* for pattern matching you have to first define your `Tyre` pattern. This can be done using string interpolation, e.g.
```Scala
val example: Tyre[(Char, Char)] = tyre"[a-z]t"
```
The syntax of Tyre patterns is very similar to the standard regex syntax, with a few exceptions, of which the most relevant are:
  1. "!\<OP\>" is a special TyRE operator for conversions
      - "!s" - converts the parse tree to a string
        ```Scala
        tyre"([a-z]t)!s" : Tyre[String]
        ```
  1. both "|" and "||" serve as alternation but differ in the result parse tree
      ```Scala
      // `|` corresponds to a union
      val union: Tyre[String | Char] = tyre"([a-z]t)!s|a"
      // `||` corresponds to a tagged union
      val taggedUnion: Tyre[Either[String, Char]] = tyre"([a-z]t)!s||a"
      ```
  1. not all standard syntax is supported, the full syntax can be found in the [Supported syntax](#supported-syntax) section.

Next you need to compile the pattern into our enriched automaton:
```Scala
val parser: Automaton[(Char, Char)] = example.compile()
```
The `Automaton[T]` trait has a single public method `def run(str: String): Option[T]`, which parses the input string. Note that the method will only return the parse tree if the whole input string matches the pattern.
```Scala
parser.run("zt") // = Some(('z', 't'))
parser.run("zx") // = None
```

### Examples
```Scala
val example: Tyre[List[Char]] = tyre"[a-z]||[0-9]*".map:
  case Left(c) => List(c)
  case Right(list) => list
val parser = example.compile()

parser.run("c") // = Some(List('c'))
parser.run("123") // = Some(List('1', '2', '3'))
parser.run("z23") // = None
```

```Scala
val t1: Tyre[Char] = tyre"[a-z]|[0-9]"
val t2: Tyre[String] = tyre"(${t1}*)!s ?:\)".map(_(0))
val parser = t2.compile()

parser.run("a12b :)") // = Some("a12b")
parser.run(":)") // = Some("")
parser.run("Ab :)") // = None
```

Supported syntax
----------------

TyRE syntax reflects standard regular expressions, albeit with several limitations. Some of them might get eliminated as the library grows. Others, like capture groups, are at odds with TyRE design objectives.
Some specific syntax extensions have also been introduced to make writing TyRE expressions more convenient.

### Summary of the TyRE syntax

| token&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; | match |
|:------|:--------|
`x` | the character `x`
`[abc]` | a single character of `a`, `b` or `c`
`[^abc]` | any character except `a`, `b` and `c`
`[a-zA-Z]` | a character in the range `a`-`z` or `A`-`Z` (inclusive)
`[^a-zA-Z]` | a character not in the range `a`-`z` and `A`-`Z` (inclusive)
`.` | any single character
`\uhhhh` | the Unicode character with hexadecimal value `hhhh`
`\w` | any word character (`[a-zA-Z0-9_]`)
`\W` | any non-word character (`[^a-zA-Z0-9_]`)
`\s` | any whitespace character (`[ \t\n\r\f\u000B]`)
`\S` | a non-whitespace character (`[^\s]`)
`\h` | any horizontal whitespace character (`[ \t\u00A0\u1680\u180E\u2000-\u200A\u202F\u205F\u3000]`)
`\H` | not a horizontal whitespace character (`[^\h]`)
`\v` | any vertical whitespace character (`[\n\r\f\u000B\u0085\u2028\u2029]`)
`\V` | not a vertical whitespace character (`[^\v]`)
`\d` | any digit (`[0-9]`)
`\D` | a non-digit (`[^0-9]`)
`\t` | the horizontal tab character
`\n` | the line feed (new line) character
`\r` | the carriage return character
`\f` | the form feed (new page) character (`\u000C`)
`\` | nothing but quotes (escapes) the following character if it is a special one, triggers error otherwise
`XY` | `X` followed by `Y` (not in `[]`)
`X\|Y` | `X` or `Y` - union (not in `[]`)
`X\|\|Y` | either `X` or `Y` - tagged union, see below (not in `[]`)
`(X)` | group `X` to override precedence (not in `[]`)
`X*` | `X` zero or more times (not in `[]`)
`X+` | `X` at least once (one or more times) (not in `[]`)
`X?` | `X` optionally (zero or one time) (not in `[]`)
`X!s` | converts `X` to string (not in `[]`)

Patterns other than the above (e.g. exact quantifiers, intersection with `&&`, hexadecimal or Unicode characters) are not supported (yet).

Regular alternation (`|`) corresponds to a union type while tagged one (`||`) corresponds to `Either`.
E.g. `ab|cd` results in `(Char, Char)` while `ab||cd` yields `Either[(Char, Char), (Char, Char)]`;
`ab|c` yields `(Char, Char) | Char` while `ab||c` - `Either[(Char, Char), Char]`.

TyRE always parses the whole input - you can think of it like `^` and `$` from traditional regular expressions being inserted implicitly at the beginning and end of the pattern. No boundary matchers are thus supported.

TyRE doesn't support any flags / options (yet). In particular, no case insensitive or Unicode character class matching is supported.

### Special characters and escaping (quoting)

Special characters must be escaped with a backslash, e.g. `\.` matches a dot and `\+` matches a plus. Although different sets of special characters are in use outside and inside brackets, escaping is allowed for all of them in both contexts. It is not allowed to use escape sequences for non-special characters.

TyRE is more regular in escaping requirements than traditional regexes - characters have to be always escaped if they have special meaning in the given context. E.g. dash is not allowed at the beginning or end of `[]` character class. So this `[+-]` won't work. You have to write `[+\-]`.

|special character(s)|outside `[]`|inside `[]`|purpose|
|:------------------|:-----------|:----------|:------|
`\` | yes | yes | escape (quote) next character (not allowed for non-special characters) or mark predefined character or character class
`.` | yes | no | match any character
`-` | no | yes | range
`^` | yes[^1] | yes | negate (complement) a `[]` character class (allowed only after opening bracket)
`[]` | yes | yes | character class
`()` | yes | no | grouping
`*+?` | yes | no | quantifiers (not allowed directly after conversion operator)
`!` | yes | no | conversion (allowed only with predefined operators, currently `s`)
`\|` | yes | no | alternation (including strict alternation `\|\|`)

[^1]: Although not used by TyRE, `^` is treated as a special character outside brackets to stick with traditional regular expressions.

### Precedence and grouping

TyRE operator precedence follows the standard of regular expressions - the order of special characters in the above table mirrors it roughly.
1. The highest precedence has backslash (`\`), the escape operator - it yields a single character while parsing.
1. Next are the operators for character class definition - range (`-`), negation (`^`), and character class itself (`[]`) - they translate to the single character as well.
1. Then comes grouping - putting any pattern in parentheses (`()`) converts it to a single token - the operators in the rest of this list are applied to it as a whole.
1. Next are quantifiers (`*+?`) and conversions (`!`) - they are applied to the preceding character, character class or a whole pattern if it is put in parentheses.
1. At last comes the sequence, which has no symbol - it is the default one.
1. At the end comes the alternation (`|`).

Note that you can put the conversion operator directly after the quantifier but not vice-versa. This `\w*!s` is legal, but this `\w!s*` isn't (although you can write `(\w!s)*`).

Grouping in TyRE doesn't define a capture group. A capture group is deemed unnecessary because the TyRE return type fully mirrors the expression structure.

### Conversions

Complex TyRE patterns result in complex return types. For example, `\w+@[A-Za-z0-9\-]+\.com` yields `((Char, List[Char), Char, (Char, List[Char]), Char, Char, Char, Char)`. Quite often, we only need some of that information. One method to simplify this is using the `map()` method on `Tyre`, building it gradually. The other is using the conversion operator.

`!s` converts the preceding token into a string. `\w+!s` yields a `String` instead of `(Char, List[Char]`. If we are interested in the user and domain part in the above example, we may refactor it into `\w+!s@([A-Za-z0-9\-]+\.com)!s` which yields `(String, Char, String)`.

`!s` is the only current conversion available, but additional are considered.

Credits
-------

*TyRE* makes use of the following tools, languages and libraries (in alphabetical order):
*   [Git](https://git-scm.com/) licensed under [GPL-2.0](https://git-scm.com/about/free-and-open-source) /D
*   [GitHub](https://github.com/) available under following [Terms of Service](https://help.github.com/en/github/site-policy/github-terms-of-service) /D
*   [Metals](https://scalameta.org/metals/) licensed under [Apache-2.0](https://github.com/scalameta/metals/blob/main/LICENSE) /D
*   [Eclipse Temurin OpenJDK](https://adoptium.net/) licensed under [GPL-2.0 with CE](https://openjdk.java.net/legal/gplv2+ce.html) /C
*   [sbt](https://www.scala-sbt.org/) licensed under [BSD-2-Clause](https://www.lightbend.com/legal/licenses) /D
*   [Scala 3](https://www.scala-lang.org/download/) licensed under [Apache-2.0](https://www.scala-lang.org/license/) /C
*   [scala-parser-combinators](https://github.com/scala/scala-parser-combinators) licensed under [Apache-2.0](https://github.com/scalameta/metals/blob/main/LICENSE) /C
*   [Scalafmt](https://scalameta.org/scalafmt/docs/installation.html#sbt) licensed under [Apache-2.0](https://github.com/scalameta/scalafmt/blob/master/LICENCE.md) /D
*   [ScalaTest](http://www.scalatest.org/) licensed under [Apache-2.0](http://www.scalatest.org/about) /T
*   [Visual Studio Code](https://code.visualstudio.com/) licensed under [Microsoft Software License Terms](https://code.visualstudio.com/license) /D

**/C** means compile/runtime dependency,
**/T** means test dependency,
**/D** means development tool.
Only direct dependencies are presented on the above list.

TyRE
====

*TyRE* is a typed regex parser library.

Goals: TODO

Getting started
---------------

TODO

Basic usage
-----------

TODO

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

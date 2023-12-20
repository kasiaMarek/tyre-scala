TODO
----

### HIGH PRIORITY

- [X] ```java.lang.ClassCastException: class scala.collection.immutable.$colon$colon cannot be cast to class scala.Tuple3``` (MMConstruction.scala:103)
- [X] Handle `+` (at least one element)
- [X] Improve tyre map executions - limit them
- [X] Add handling of character classes (\s . \w etc.)
- [X] Allow special character in brackets without escaping (eg. .)
- [X] Allow character classes in brackets (eg. [^\s])
- [X] Write readme
- [X] Stringify - allow easily flatten matched data to string
- [X] Separate API from internals (packages)

### BACKLOG

- [X] Check if TyRE matching is greedy
- [X] Support for Unicode character values (\uhhhh)
- [X] Allow escaped characters in ranges (eg. [\t-s]) and generally in brackets (eg. [^\s])
- [ ] Support for singleton types
- [ ] Helper functions for handling digits and numbers
- [ ] Unicode mode
- [ ] Scaladoc
- [ ] Prepare deployment

### TO CONSIDER

- [ ] Externalize conversions to futher limit map executions

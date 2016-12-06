A simple package implementing a DSL for generating format strings.

- [Introduction](#introduction)
- [Api Reference](#api-reference)
- [DSL Reference](#dsl-reference)

# Introduction

```lisp

(make-format-string '(:str)) #| ==> "~a" |#
(make-format-string '((:map () :str))) #| ==> "~{~a~}" |#

(define-message hello (name)
  "Hello " :str)
(define-message print-comma-separated (values)
  (:map () :str))

```

# Api Reference

```lisp
(make-format-string spec) #| function |#
```

Takes a format string specification and turns it into a string.

```lisp
(format* stream spec &rest args) #| macro |#
```

Use like CL:FORMAT, except translate a format specification to a string at macroexpansion time.

```lisp
(define-message (stream-symbol &rest format-args) &body spec) #| macro |#
```

Defines a function that takes a stream and the arguments to be
formatted and then formats the arguments to the stream. The spec is
compiled to a string at macroexpansion time, so this should be
reasonably efficient.

TODO: document the API for defining directives.

# DSL Reference

A spec consists of operators and literals.  Literals are either
strings, characters or integers and they are formatted as-is via
princ. There are two kinds of operators: simple operators and compound
ones. Simple operators correspond to format control directives and
represented in the spec by keywords  such as `~A` or by lists
`(keyword . modifiers)` and they expand to the corresponding
directives. Compound operators correspond to format directives that
can contain other directives such as `~{~}`.  In a spec, these are
formatted like flet function definitions:

```lisp
(keyword (&rest modifiers) &body spec)
```

Compound operators are further divided into sectioned operators and
non-sectioned ones.  The difference is that, in non sectioned
operators, the body is treated just as a normal spec. In sectioned
ones, the body is treated as a list of items to be divided with `~;`.
See CLHS 22.3 for a full guide to the modifiers for the various format
directives.

## Simple Format Operations

- :str --- Translates to ~a, format a lisp value for humans
- :repr --- Translates to ~s, format a lisp value in a way that can be read by the reader (?)
- :float --- Translates to ~f, format a float.
- :dec --- Translates to ~d, format a number as a base 10 number.
- :decimal --- Translates to ~d, format a number as a base 10 number.
- :hex --- Translates to ~x, format a number as a base 16 number.
- :hexadecimal --- Translates to ~x, format a number as a base 16 number.
- :oct --- Translates to ~o, format a number as a base 8 number.
- :octal --- Translates to ~o, format a number as a base 8 number.
- :currency --- Translates to ~$, format a number in a manner suitable for currency
- :exit --- Translates to ~^, leaves a iteration construct
- :end-section --- Translates to ~;, divides sections of a construct (TODO: maybe this will go away?)
- :goto --- Translates to ~*, moves within the list of arguments
- :fresh-line --- Translates to ~&, ensures we're at the beginning of a line and, possibly adds Modifier-1 linebreaks
- :ensure-line --- Translates to ~7, alias for :fresh-line
- :new-line --- Adds a linebreak

## Compound Format Operations

### Iteration

- :map --- Translates to ~{~}, iterate over a list passed in
- :rest ---  Translates to ~@{~}, iterate over the rest of the arguments
- :ap --- Translates to ~:{~}, apply a list to the corresponding enclosed format directives
- :apply --- Alias for :ap
- :aprest --- Translates to ~:@{~}, apply the rest of the arguments to the corresponding enclosed format directives
- :apply-rest --- Translates to ~:@{~}, apply the rest of the arguments to the corresponding enclosed format directives

### Conditional Output (Sectioned Operators)

- :y-or-n --- Translates to ~:[~], if the argument is nil, print first spec otherwise print second
- TODO: add others here...

### Case Control

- :lowercase --- Translates to ~(~), lowercase all alphabetic characters.
- :downcase --- Translates to ~(~), alias for :lowercase
- :uppercase --- Translates to ~:@(~), uppercase all alphabetic characters.
- :upcase --- Translates to ~:@(~), alias for :uppercase
- :titlecase --- Translates to ~:(~), titlecase all alphabetic characters.
- :capitalize --- Translates to ~:(~), alias for :titlecase
- :initialcap --- Translates to ~@(~), uppercase first character

### Justification
TODO: finish documenting this and switch them to sectioned operators.

- :spread --- Translates to ~<~>
- :ljust --- Translates to ~@<~>
- :left --- Translates to ~@<~>
- :rjust --- Translates to ~:<~>
- :right --- Translates to ~:<~>
- :cjust --- Translates to ~:@<~>
- :center --- Translates to ~:@<~>

### Miscellaneous

These are not part of Format, but are defined just to be helpful

- :own-line --- Translates to ~&~%, ensure that the included text is on its own line, without unnecessary gaps.

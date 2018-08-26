# nonok

Haskell templating language.

## Overview

### Syntax

nonok uses curly brackets for tag blocks `{{ block }}` and `{- expr }}`
for rendering expressions

### Variables

There are two types of variables:
* local - bound to scope, can be created in template, not inherited to included templates, proceeded with `$`
* global - passed to template renderer, automatically inherited to included templates, proceeded with `@`


### Blocks

#### Declarations

`{{ let $age=21, $name="Andrew" }}`

Variable bound to scope. It will be removed after its scope is popped from the stack.

#### For loops

`{{ for $it in $iterable }} ... {{ endfor }}`

Allows iterating over list expressions or strings.
For loop body creates new scope on stack.

#### If statements

`{{ if expr1 }} ... {{ elif expr2 }} ... {{ else }} ... {{ endif }}`

Elif and else blocks are optional. Expressions must be evaluable to bool.
If statement body creates new scope on stack.

#### Include

`{{ include 'includes/header.html' }}`

All global variables are automatically inherited, however they can be overwritten or new can be added. For example:

`{{ for $post in @posts }}
    {{ include 'partials/post.html', {'post' :  $post} }}
{{ endfor }}`


#### Comments

`{{ comment }} ... {{ endcomment }}`

Body of comment tag will be ignored.

#### Raw tag

`{{ raw }} ... {{ endraw }}`

Body of raw tag will be interpreted as plain text, ignoring all special characters.

### Rendering

All displaying is done in `{- _ }}`. This tag simply evaluates passed expression and displays it.

### Types

#### Literals

* integer `(2, -1, 0, 5124)`
* float `(0.0, -2.324, 4.5)`
* bool `(True, False)`
* string `('', 'wololo', 'x')`

All literals are evaluable to bool.

#### Maps

Declared in similar fashion as in Javascript. Key must be a string, value can be any expression.

`{{ let $person={'name' : 'John', 'age' : 21} }}`

Accessing map fields:

`{- $person.name }}`

#### Lists

List can contain any expression. Valid example:

`{{ let $arr=[1, 'foo', {'name' : 'John', 'age' : 21}, True, ['a', 'b', 'c']] }}`

Most useful thing about list is that it can be loop through.

### Functions

Function is treated as an expression and is evaluated to some expression. Functions are called in C-like syntax. Nonok supports functions 3-arity functions. They all run in in Render monad which is an IO monad on steriods (io, state, writer, except).

#### Examples:

* `{- lower('DAVID') }}`
* `{{ if equal($a, $b) }} ... {{ endif }}`
* `{- concat(lower($filename), ".html") }}`

#### Standard functions:

* equal - (==)
* gt - (>)
* gte - (>=)
* lt - (<)
* lte - (<=)
* lower - 1-ary, accepts string, returns lowercased
* upper - 1-ary, accepts string, returns uppercased
* strip - 1-ary, accepts string, removes spaces from start and end
* replace - 3-ary, accepts pattern, replacement, string
* concat - 2-ary, concats two strings

#### Adding custom functions

I'll try to complete this readme in future.

## Examples

Check examples folder in this repo.

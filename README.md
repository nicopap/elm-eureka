Elm eureka is a rust library for parsing elm projects and elm code.

## Goal

The aim is to facilitate the creation of efficient tooling for the [elm
programming language](http://elm-lang.org).


## Current state

I've just done the lexer. Even then, the lexer is not that accurate, it doesn't
really vaidate literals.


<!--
## Features

* A fast parser.

* An API to query the elm AST.

## Implementation

The architecture is based on the usual lexer+parser passes. Since the goal is
to have an efficient parser, I made it *lazy*. Lazy in the sense that it will
only attempt to parse expressions that are *needed* in order to get information
on the code.

The typical usecase for the parser is to get the list of exported symbols in a
file, this usually requires to read the `module` declaration and nothing else!

If you want to visualise the dependency graph of your application, it is also
only a matter of the first few lines of a source file!

Finally, if you want to retreive the type of the exported values, you need only
to look at a very limited subset of the file. In fact, you don't even need to
parse any expression.

-->

## Planned (maybe?)

* An efficient parser capable of building an elm AST.

* Proper line location tracking, so you can retreive the location of the
	definition of some functions.

* A clean API to just retreive interesting stuff from the code (types, doc
	strings, exported symbols etc)

* Other crates would implement the actual user-facing part of dev tools. One
	can imagine a completion manager with caching, or a dependency graph
	generator etc.

<!--
## Nonfeatures

This library is not built to "verify" code, but to analyse existing code so it
can help you. If you need a tool to compile or verify elm code, use elm-make :)

Pathological cases are:

* How character literals are handled (they are parsed like strings)

* How number literals are handled (anything starting by a number and containing
	0123456789abcdefx.- is accepted)


## Features provided by other crates

* Proper dependency anaylsis for namespace based completion suggestions.

* Caching so the whole project doesn't need to be parsed every time.
-->

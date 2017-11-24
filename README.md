Elm eureka is a rust library for parsing elm projects and elm code.

## Goal

The aim is to facilitate the creation of efficient tooling for the [elm
programming language](http://elm-lang.org).


## Current features

* A lexer capable of tokenizing all elm constructs from a character iterator.

* A parser capable of parsing:
	* Module declarations
	* Documentation comments
	* Import declarations
	* Type declarations


## Implementation

The architecture is based on the usual lexer+parser passes. Since the goal is
to have an efficient parser, I made it *lazy*. Lazy in the sense that it will
only attempt to parse expressions *needed* to get a specific information on
some code.

The typical usecase for the parser is to get the list of exported symbols in a
file, this usually requires to read the `module` declaration and nothing else!

If you want to visualise the dependency graph of your application, it is also
only a matter of the first few lines of a source file!

Finally, if you want to retreive the type of the exported values, you need only
to look at a very limited subset of the file. In fact, you don't even need to
parse any expression.


## Planned (maybe?)

* An efficient parser capable of building an elm AST lazily.

* Proper line location tracking, so you can retreive the location of the
	definition of some functions.

* A clean API to just retreive interesting stuff from the code (types, doc
	strings, exported symbols etc)

* Other crates would implement the actual user-facing part of dev tools. One
	can imagine a completion manager with caching, or a dependency graph
	generator etc.

## Limitations

* Aligned indentation after `let` keyword **do not work**. If you are thourugh
	about using elm-format, this shouldn't be an issue.
	\
	example:
	\
	```elm
	-- Do not work
	f x = let y = x * 3
	          z = x + 92
				in x + y + z
	-- Works, elm-format approved.
	f x =
	  let
		  y = x * 3
			z = x + 92
		in
		  x + y + z
	```

* Prefix `-`. The weird semantic behind that are beyond me:
	* If preceeding without whitespace a word or opening parenthesis, and there
		is no words preceeding the `-` without whitespace, it is a prefix `-`.
	* Otherwise it is the `-` operator.
	* You can't imagine how much of an hassle this is to parse.

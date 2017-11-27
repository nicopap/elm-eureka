Elm eureka is a rust library for parsing elm projects and elm code.

## Goal

The aim is to facilitate the creation of efficient tooling for the [elm
programming language](http://elm-lang.org).


## Current features

* A lexer capable of tokenizing all elm constructs from a character iterator.

* A parser capable of parsing the entirety of the elm grammar, at the exception
	of a couple edge cases evocated in the [limitations](#Limitations) section.


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

* An efficient parser capable of building an elm AST lazily. (Currently, it
	just parses everything upfront)

* Proper line location tracking, so you can retreive the location of the
	definition of some functions.

* A clean API to just retreive interesting stuff from the code (types, doc
	strings, exported symbols etc)

* Other crates would implement the actual user-facing part of dev tools. One
	can imagine a completion manager with caching, or a dependency graph
	generator etc.

* Proper error handling :)

* Proper handling of shader and multiline string literals.

* Reorganize the crate, separate parse-tree from ast etc.

## Limitations

* Aligned indentation after `let` keyword **do not work**. If you are thourugh
	about using elm-format, this shouldn't be an issue.
	\
	example:
	\
	```elm
	-- Does not work (should work)
	f x = let y = x * 3
	          z = x + 92
				in x + y + z
	-- Works (should)
	f x =
	  let
		  y = x * 3
			z = x + 92
		in
		  x + y + z
	-- Works (should)
	f x = let y = 10 in y * x
	```
	\
	This is also true for `case` pattern branches.

* Prefix `-`. The weird semantic makes it hard to integrate in a lexer:
	* If preceeding without whitespace a word or opening parenthesis, and there
		is no words preceeding the `-` without whitespace, it is a prefix `-`.
	* Otherwise it is the `-` operator.

* Shader literals and multiline string literals are not yet handled properly.

* I handle indentation in a different maner than the official elm-compiler
	parser: instead of checking alignement within the parser, I execute a
	"preparse" phase in which I insert closing delimiters **only when case
	branches can be ambiguous** (nested `case`).
	\
	I have concidered encoding into the type of the expression the level of
	indentation using Peano numbers, but I found this was a bit too *fancy*. If
	anyone has the guts to do that and show me code readability and performance
	improvements, I'm all hear.
	\
	The upside is that the semantics should wind up being the same as keeping
	track of the indentation level. However, we may have a slower parser as a
	result.


## License(s)

* The libraries are licensied under the LGPL3. Please see
  <https://www.gnu.org/licenses/lgpl-3.0.html> for more informations.

* I do not claim any copyrights over the files in "examples" directories. They
	are trivial generic implementations with no claims of fitness for any
	purpose.

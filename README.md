# ScanRat - PEG Parser Combinators for F# with support for Left Recursion and Memoization

[![Build status](https://ci.appveyor.com/api/projects/status/7pc2dgvwrmw3r5l7?svg=true)](https://ci.appveyor.com/project/pragmatrix/scanrat)

ScanRat is a mashup of the IronMeta PackRat parsing algorithm, and the concepts of the [FParsec](http://www.quanttec.com/fparsec/) and [Sprache](https://github.com/sprache/sprache) projects.

## Features

- Automatic memoization of the parsing results.
- Direct and mutual left recursion as specified in the paper [Left Recursion in Parsing Expression Grammars](http://arxiv.org/pdf/1207.0443v1.pdf).
- Computation Expressions to conventiently parse sequences (inspired by Sprache's LinQ SelectMany "hack").

and what's missing:

- RegExp support
- Source indices must be accessible inside the result generators (define an additional production operator?).

## Get Started

To use ScanRat in Visual Studio, install the [NuGet package](https://www.nuget.org/packages/ScanRat/) or clone this repository and add the `ScanRat/ScanRat.fsproj` as a reference to your project.

In your F# source file, add

	open ScanRat.ScanRat

and start writing grammars. A grammar is specified as a collection of production rules. The rules are build from a number of combinators.

## Generic Combinators

The generic combinators listed here can be applied to any parsing rule of any type.

- `+` is the sequence combinator

		let twoDigits = digit + digit

	is a production rule that parses two digits, not one, not zero not three.

	Sequence combinators are left associative, which means that they are combined from left to right. The parsing result type of the `+` sequence combinator is a tuple that contains the parsing result of the left rule and the parsing result of the right production rule.

	The `+.` and `.+` sequence combinators can be used to only process the result of the rule that is placed at the side of the dot.

- `|-` is the choice combinator

		let eitherLeftOrRight = left |- right

	Parses either left or right. If both rules match the input, the first rule is preferred. Both rules must be of the same result type.

	The choice combinator is also left associative, but has a lower priority than the sequence combinators, which means that you can put sequences inside the choice rules without using parenthesis:

		let driverDecision = accellerate + overtake |- driveSlowly

- `-->` is the production combinator

	`-->` is used to capture and convert the parsing result. It expects a function that takes the parsing result from the rule on the left side and converts its result.

	For example, when parsing a two digit number, and `digit` itself is a rule that returns an integer, the resulting number can be computed on the spot:

		let twoDigitNumber = digit + digit --> fun (digit1, digit2) -> digit1 * 10 + digit2

## String Specific Combinators

The string specific combinators are optimized to handle string based input.

- `~~` Parses a simple string. This is an unary combinator that is placed in front of a string to convert it to a parsing rule. The rule's return type is a string.

		let hello = ~~"Hello"

	defines a rule that parses the string "Hello".

- `oneOf` Parses one character of a string. The rule's return type is a character.

		let oneOrTwo = oneOf "12"

	is a shorthand and optimized form of

		let oneOrTwo = (~~"1" |- ~~"2") --> fun str -> str.[0]

	To conveniently parse a digit and return the integer value of it, `oneOf` can be used like:

		let digit = oneOf "0123456789" --> fun c -> int(c) - int('0')

## Parsing

Parsing is done by calling the `parse` function. Two arguments are required, the first one is the grammar and the second one is the input (a string for now):

	let digit = oneOf "0123456789" --> fun c -> int(c) - int('0')
	let r = parse digit "3"

The result of a parse is either `Success` or `Failure`:

	match r with
	| Success s -> s.Value
	| Failure f -> failwith "error"

## Recursive parsing grammars

Because a rule may need to be accessed before the point it has been defined, recursive rules are specified in a slightly different way:

	let digit = oneOf "0123456789" --> fun d -> int(d) - int('0')
  	let digits = production "digits"
  	digits.rule
		<- digits + digit --> fun (a, b) -> a*10+b
 		|- digit

Here the `production` function creates an initially named but empty production rule. After that, the rule's term can then be assigned to production's `rule` property. This makes it possible for the term to refer back to the production and - like in this example - specify a left recursive grammar that parses digits.

## Parsing Sequences

Combining more than three items with the `+` sequence combinator may get a bit annoying, because each new item generates a new tuple that contains the previous result type as its first argument.

For longer sequences, there is an alternative which makes use of [Computation Expressions](http://msdn.microsoft.com/en-us/library/dd233182.aspx):

The rule:

	let addressRule = nameGrammar + streetGrammar + cityGrammar + phoneGrammar --> fun (((name, street), city), phone) -> { Name = name; Street = street; City = city; Phone = phone }

may also be specified by a much more readable and extensible:

	parseq {
		let! name = nameGrammar
		let! street = streetGrammar
		let! city = cityGrammar
		let! phone = phoneGrammar
		return { Name = name; Street = street; City = city; Phone = phone }
	}

## Error handling

TBD

## Limitations

- If a direct left and right recursion is used in one rule, the algorithm incorrectly right-associates the parses [as noted by Laurence Tratt in his paper](http://tratt.net/laurie/research/pubs/papers/tratt__direct_left_recursive_parsing_expression_grammars.pdf).
- The parsers that are built inside a computation expression can not be memoized, but the parsers they refer to, can. Therefore it's more efficient to refer to parsers from inside computation expressions instead of putting them inline.

## Acknowledges

Many thanks go to 

- [Gordon Tishler](http://sourceforge.net/users/kulibali), the author of the [IronMeta project](http://ironmeta.sourceforge.net/), who implemented the core matching algorithm.

- [Nicolaus Blumhardt](http://nblumhardt.com/) for the inspiring [Sprache parser combinators](https://github.com/sprache/sprache) I use in a lot of C# projects.

## License

[MIT](LICENSE)

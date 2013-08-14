# ScanRat - PEG Parser Combinators for F# with support for Left Recursion

ScanRat is a mashup of the IronMeta PackRat parsing algorithm, that supports left recursion, and the ideas from the FParsec and Sprache projects.

## Features

- Automatic memoization of the parsing results.
- Direct and mutual left recursion as specified in 
- Some error handling (includes the parse stack)
- Computation Expressions to conventiently parse sequences (inspired by Sprache's LinQ SelectMany "hack").

## Limitations

- If a direct left and right recursion is used in one rule, the algorithm wrongly right-associates the parses as noted by Tratt in his paper.

## Soon

- RegExp support
- Source indices must be accessible when for the result generators.

To use ScanRat, check out and import the ScanRat project or install the (upcoming) NuGet package.

	open ScanRat

A grammar is specified as a collection of production rules. The rules are build from a number of combinators.

## Generic Combinators

The combinators listed here can be applied to any parsing rule of any type.

- `+` is the sequence combinator

		let twoDigits = digit + digit

	is a production rule that parses two digits, not one, not zero not three.

	Sequence combinators are left associative, which means that they are combined from left to right. The parsing result type of the `+` sequence combinator is a tuple that contains the parsing result of the left and the parsing right of the right production rule.

	To ignore intermediate parsing results, the `+.` and `.+` sequence combinators only use the result of the expression at the side of the dot.

- **|-** is the choice combinator

		let eitherLeftOrRight = left |- right

	The choice combinator is also left associative, but has a lower priority than the sequence combinators, which means that you can put sequences nicely inside the choice rules without using paranthesis:

		let driverDecision = accellerate + overtake |- driveSlowly

- **-->** is the production combinator

	`-->` is used to capture and convert the parsing result. It expects a function that takes the parsing result from the rule on the left side and converts its result.

	For example, when parsing a two digit number, and digits itself are two integers, a the resulting number can be computed on the spot:

		let twoDigitNumber = digit + digit --> fun (digit1, digit2) -> digit1 * 10 + digit2

## String Specific Combinators

The string specific combinators are optimized to handle string based input.

- `~~` Parses a simple string. This is an unary combinator that is placed in front of a string to make them a parsing rule. The rule's return type is a string.

		let hello = ~~"Hello"

	defines a rule that parses the string "Hello".

- `oneOf` Parses one character of a string. The rule's return type is char.

		let oneOrTwo = oneOf "12"

	is a shorthand and optimized form of

		let oneOrTwo = (~~"1" |- ~~"2") --> fun str -> str.[0]

	A parser for a digit that actually returns the integer value of a digit can
be conventiently defined with `oneOf`:

		let digit = oneOf "0123456789" --> fun c -> int(c) - int('0')

## Parsing

Parsing is done by calling the `parse` function. Two arguments are required, the first one is the grammar and the secone one is the input (a string for now).

	let digit = oneOf "0123456789" --> fun c -> int(c) - int('0')
	let r = parse digit "3"
		
The result of a parse is either a ParsingSuccess or a ParsingFailure

	match r with
	| Success s -> s.value
	| Failure f -> failwith "error"

## Recursive parsing grammars

Because a rule may need to be accessed before the point it has been defined, recursive rules are specified slightly different:

	let digit = oneOf "0123456789" --> fun d -> int(d) - int('0')
  	let digits = production "digits"
  	digits.rule
		<- digits + digit --> fun (a, b) -> a*10+b
 		|- digit

Here the `production` function creates an initially named, but empty production rule. After that, the rule body can then be assigned to its `rule` property. This makes it possible for the rule body to refer back to the production and - like in this example - specify a left recursive rule to parse digits.

## Complex Sequences

Tranforming more than three items with the `+` sequence combinators into an aggregate may get a bit annoying, because each new item generates a new tuple that contains the previous result type in its first type argument.

So for more complex sequences, there is an alternative which makes use of Computation Expressions (TBD link):

The rule:

	let addressRule = nameGrammar + streetGrammar + cityGrammar + phoneGrammar --> fun (((name, street), city), phone) -> { Name = name; Street = street; City = city; Phone = phone }

could also be defined by a much more readable

	parseq {
		let! name = nameGrammar
		let! street = streetGrammar
		let! city = cityGrammar
		let! phone = phoneGrammar
		return { Name = name; Street = street; City = city; Phone = phone }
	}

## Error handling

TBD







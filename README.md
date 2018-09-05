This library allows you to generate a parser and generate an ABNF
specification for that parser.

The motivation is to allow you to create a parser which can be
automatically tested against a published ABNF specification.

It is assumed that it is possible to test two ABNF specifications for
equality by rewriting them into some normal form and then using simple
structural equality to see if they are the same grammar.

The implementation is based around a `ClassyParser` which contains
functions like `pCharVal` and `pMany`. One instance of this class
generates a parser, another instance generates the ABNF specification.

It is up to the library developer to prove that the parser instance
and ABNF correctly match each other. However, this is a much easier
problem than showing a specific parser matches an entire grammar.

Instead of a type-classs, this library could be implemented as a free
monad or using the operational library. The class-based implementation
is straight forward and probably has the best performance. But,
perhaps there are advantages to an alternative implementation.

Over all, efficiency is not the main goal of this library. It is
intended for implementing a correct reference parser for a
grammar. This would allow a developer to create a highly optimized
parser by direct ByteString manipulation and use QuickCheck to prove
that their parser is correct.

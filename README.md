# What's in this project

`lib/` - parser combinator library

`bin/main.ml` - example language parser

`grammar.ebnf` - grammar of the example language

`fib.lang`, `arithmetic.lang` - examples for the example language parser

# Building

```
dune build
```

This builds the example language parser

# Usage

Running the parser on some examples:

```
dune exec parser_combinators fib.lang
dune exec parser_combinators arithmetic.lang
```

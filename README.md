This is a compiler for the [MinCaml language][1] implemented using Rust and
[cranelift][2], and nothing else.

## Why?

I wanted to experiment with compiler design in Rust. Originally I wanted to
implement register allocator too, but that turned out to be a challenge in Rust
(borrow checking issues everywhere), so I later decided to use cranelift for
code generation, which was also something I wanted to learn.

## MinCaml in a few words

- A subset of OCaml
- 64-bit integers, 64-bit floats, arrays and tuples
- No user defined types
- No polymorphism, all types inferred
- Tail-call elimination (currently unimplemented, as I couldn't figure out how
  to do indirect jump in cranelift)
- No garbage collection (not possible to implement with cranelift anyway, as
  object code backend currently doesn't support stack maps)

## Build

Simply `cargo build` should work.

## Run and test

Build step above generates two executables:

- mc: main compiler executable.
- test: test runner.

To compile a file simply run `mc` with the file path as argument. Example:

```
$ ./target/release/mc programs/fib.ml
```

`mc` should dump some intermediate code, some stats, and finally generate two
files: `fib.o` and `fib`. The latter is the executable for this program.

`mc` uses `gcc` for building the runtime system (just a few built-in function implemented in C)
and linking.

To run the tests simply run the `test` executable. Note that the test runner
uses `ocamlc` as the reference compiler so make sure it is installed.

Currently the test `programs/bench/harmonic.ml` fails with stack overflow as we
don't do tail-call elimination. (I couldn't figure out how to generate indirect
jumps in cranelift)

## Reading

The code does not follow the original [MinCaml compiler][1], so here is some
notes for readers:

- Hand-written lexer and parser are in `src/lexer.rs` and `src/parser.rs`.
  Parser implements recursive-descent with something like ["recedence
  climbing"][3] or [Pratt][4] (not sure which, or if it's exactly one of them,
  but the idea should be similar to one or both of them) for expression parsing.

- To avoid excessive heap allocation and clonning, we intern variables and
  types. Intern table is implemented in `src/interner.rs`. Variables, types and
  intern tables are maintained by `Ctx` (for "context"), which is implemented in
  `src/ctx.rs`.

- After parsing we type check (`src/type_check.rs`). Type checker does naive
  unification (no union-find) and does not implement type schemes or
  generalization (the language doesn't support polymorphism), so it's fairly
  simple.

  One interesting thing type checker does is it replaces uses of variables with
  their binders. So for example when we parse `let x = 1 in x` the parser
  generates fresh unique numbers for every variable and generates something like
  `let x{0} = 1 in x{1}`. Here two uses of `x` look the same but they're different
  to the compiler. Type checker, when seeing `x{1}`, looks at the scope for its
  binder, and replaces it with `x{0}`. So after type checking we get `let x{0} =
  1 in {0}`.

  This means we don't need a renaming step to eliminate shadowing and make it
  easy to find binders of variables. Shadowing may happen in user-written names
  of variables, but no actual shadowing happens after type checking.

- Next pass is `anormal` (`src/anormal.rs`), which implement A-normalization.
  Nothing interesting here.

- Next pass is closure conversion (`src/closure_convert.rs`). In addition to
  lowering closures to tuples this pass turns the program into a CFG with basic
  blocks, assignment statements, and branching.

- Final pass is code generator, which generates native code using cranelift.
  Only tested on x86\_64 Linux.

- After that we link the generated object code using `gcc`.

## Want more?

If you're looking for small compilers to study, you might be interested in
[another (and older) project of mine][5], which implements a compiler in Racket
for a subset of Racket. Some notable differences from this project: it doesn't
implement a parser (uses Racket's reader), but implements register allocator,
and has a garbage collector.

[1]: http://esumii.github.io/min-caml/index-e.html
[2]: https://github.com/bytecodealliance/wasmtime/tree/master/cranelift
[3]: https://osa1.net/posts/2015-01-29-top-down-expr-parsing-easy.html
[4]: https://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
[5]: https://github.com/osa1/racket.rkt

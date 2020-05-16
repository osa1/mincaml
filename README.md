This is a compiler for the [MinCaml language][1] implemented using Rust and
[cranelift][2], and nothing else.

## Why?

I wanted to experiment with compiler design in Rust. Originally I wanted to
implement register allocator too, but that turned out to be a challenge in Rust
(borrow checking issues everywhere), so later I decided to use cranelift for
code generation, which was also something I wanted to learn.

[1]: http://esumii.github.io/min-caml/index-e.html
[2]: https://github.com/bytecodealliance/wasmtime/tree/master/cranelift

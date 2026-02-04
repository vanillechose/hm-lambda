# hm-lambda

Toy interpreter for a call-by-value λ-calculus with a Hindley-Milner
type system, written in OCaml.

```
>>> (\f. f f) (\x. x) ;;
error: this expression has type '_0 but type '_0 -> '_1 was expected
   1 | (\f. f f) (\x. x) ;;
     |      ^
>>> let id = let f = \x. x in f f ;;
id : '2 -> '2 = <fun>
>>> let not = \x. match x with
    case #t -> #f
    case _ -> #t
    ;;
not : bool -> bool = <fun>
>>> not (id #t) = #f ;;
- : bool = #t
```

## Building & Running

The project can be built using `dune build`. Open the repl with `dune exec hm-lambda`.

## Features

The following expressions are supported:

- **Atomic expressions**: booleans (`#t` or `#f`), integers, unit value (`()`) , variables
- **Tuples**: `6, 7`
- **Lambda abstractions** of the form `\x. M`
- **Function application**: `M N`
- **Polymorphic equality test**: `M = N` or `M <> N`, `=` and `<>` are right-associative
and have the same precedence. Equality tests on closures are well-typed but will
always return false
- **Boolean expressions**: `M && N` or `M || N`, `&&` and `||` are right-associative
and have the same precedence
- **if ... then ... else ...** expressions.
- **pattern matching** on tuples, constant patterns and wildcards

```
>>> let f = \x. match x with
        case _, #f, #t -> 1
        case #f, #t, _ -> 2
        case _, _, #f -> 3
        case _, _, #t -> 4
    ;;
f : bool * bool * bool -> int = <fun>
>>> f (#f, #t, #t) ;;
- : int = 2
```

- **Let-in expressions**: `let x = M in N`
    - If `M` is well-typed, its type is generalized and `x` becomes a
    polymorphic object. Each occurrence of `x` in `N` will be typed with
    its own instance of `x`'s type scheme, allowing expressions like
    `let f = \x. x in f f` to be written

Additionally, **top-level bindings** of the form `let x = M` are supported. In this
case, `x` will remain in the scope for the rest of the program.

The interpreter is a REPL. On top of hm-lambda programs, it also recognizes
top-level directives of the form `#directive`:

- `#tree` : toggle tree, i.e. show interpreter tree code before each execution
- `#trace` : toggle interpreter trace, i.e. show what the tree-walk interpreter is doing
- `#env` : show the layout of the execution environment

## Bugs / Todo

- Type and evaluate `let rec` bindings

- Improve type inference. The type system, based on algorithm W as described in [1],
is easy to implement and doesn't technically require mutability, but it's slow
as it needs to apply substitutions to the entire environment. There are more efficient
ways to implement it ([2] seems to be what I'm looking for).

- Improve the interpreter or replace it with a more efficient garbage collected 
virtual machine. As of now the interpreter is probably leaking memory : In the
following code, the first f is still reachable in the execution environment (meaning
it will never be collected by OCaml's GC), but it is unreachable by subsequent
hm-lambda code.

```
>>> let f = \x. x ;;
f : '0 -> '0 = <fun>
>>> let f = () ;;
f : unit = ()
>>> #env
0 = ()
1 = <fun>
```

## References

[1] Luis Damas and Robin Milner. 1982. Principal type-schemes for functional programs. In Proceedings of the 9th ACM SIGPLAN-SIGACT symposium on Principles of programming languages (POPL '82). Association for Computing Machinery, New York, NY, USA, 207–212. https://doi.org/10.1145/582153.582176

[2] Didier Rémy. Extension of ML type system with a sorted equation theory on types. [Research Report] RR-1766, INRIA. 1992. inria-00077006

[3] Luc Maranget. 2008. Compiling pattern matching to good decision trees. In Proceedings of the 2008 ACM SIGPLAN workshop on ML (ML '08). Association for Computing Machinery, New York, NY, USA, 35–46. https://doi.org/10.1145/1411304.1411311

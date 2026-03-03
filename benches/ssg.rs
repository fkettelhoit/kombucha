use criterion::{Criterion, criterion_group, criterion_main};
use kombucha::v0::*;

const SCRIPT: &str = include_str!("../examples/ssg.kb");

const MARKDOWN: &str = "\
# Turning lambda calculus into a programming language

## Pure lambda calculus

Lambda calculus is one of those ideas that feels almost _too_ simple to be useful. The entire language has only three constructs: variables, abstraction, and application. A variable `x` is just a name. An abstraction `(x) { x }` creates an anonymous function. And an application `f(x)` calls a function with an argument. That is **all** there is to it.

And yet from these three building blocks you can construct _anything_ that a Turing machine can compute. Numbers, booleans, conditionals, recursion, data structures, all of it emerges from nothing but functions calling functions.

## A bit of syntax and sugar

The pure calculus is elegant but painful to write real programs in. Every practical language derived from it adds at least `let` bindings and some notion of named definitions. We also want pattern matching, which turns out to be the single most important feature for working with algebraic data types.

Consider a simple example: we want to define a `map` function that transforms every element in a list. In our language, this looks like:

```
map = (f, xs) {
  match(xs) [
    Nil -> { Nil }
    Cons(head, tail) -> { Cons(f(head), map(f, tail)) }
  ]
}
```

The pattern matching syntax is concise and readable. Each arm specifies a constructor pattern and a body. Variables in the pattern are bound in the body. This is the bread and butter of functional programming.

## Data structures

With tagged values and pattern matching, we can encode any algebraic data type. A binary tree, for instance, is either a `Leaf` or a `Node` with a value and two children:

- A `Leaf` carries no data and represents the empty tree.
- A `Node(val, left, right)` holds a value and two subtrees.
- We can write functions like `insert`, `lookup`, and `fold` using pattern matching.
- The tree is **persistent** by default since we never mutate, only construct new nodes.

This is a powerful model. Instead of thinking about pointers and mutation, we think about _shapes_ of data and how to transform them. The compiler can help us ensure we handle every case.

## Effects and the outside world

A pure language cannot do anything useful on its own. It needs some way to interact with the world: reading files, printing output, making network requests. The traditional approach is monads, but algebraic effects offer a more flexible alternative.

With algebraic effects, a function can _perform_ an effect (like `read!(\"config.txt\")`) and the caller decides how to handle it. The effect bubbles up until it finds a handler, which can inspect the effect, provide a value, and resume the computation. This is strictly more powerful than exceptions because the handler can **resume** the interrupted computation, not just abort it.

> The key insight is that effects separate the _description_ of what a program wants to do from the _implementation_ of how it gets done. This makes programs more modular and testable.

Here is an example of a simple effect handler:

```
try {
  content = read!(\"config.txt\")
  parse(content)
} catch: [
  read!: (resume, path) => {
    resume(\"default = true\")
  }
]
```

## Performance considerations

The elephant in the room with pure functional languages is performance. Linked lists are cache-unfriendly. Immutable data structures require allocation on every update. Pattern matching on deeply nested structures can be slow.

There are well-known mitigation strategies: **compile-time optimizations** like deforestation and strictness analysis, **runtime techniques** like pointer tagging and bump allocation, and **algorithmic approaches** like using balanced trees with `O(log n)` operations instead of lists.

But the honest answer is that a naive implementation will be _significantly_ slower than the equivalent imperative code. The question is whether the clarity and safety benefits outweigh the cost, and for many applications they do.

## Putting it all together

What we end up with is a small but expressive language. Functions are first-class values. Data is represented as tagged variants, deconstructed with pattern matching. Side effects are explicit, handled through an algebraic effect system that keeps pure code pure. The whole thing compiles to a simple bytecode VM that is easy to embed in a host application.

The language is not trying to be fast or feature-complete. It is trying to be _clear_: a minimal substrate for expressing computation, where every feature earns its place.
";

const DATE: &str = "2025/06/15";

fn run_ssg(program: &Program, md: &str, date: &str) -> String {
    let mut result = run_program(program).unwrap();
    loop {
        match result {
            VMResult::Done(val, strings) => {
                let mut output = String::new();
                flatten(&val, &strings, &mut output);
                return output;
            }
            VMResult::Effect { name, args: _, mut continuation } => {
                let resume_val = match name.as_str() {
                    "input" => {
                        let lines: Vec<Value> = md
                            .lines()
                            .map(|line| str_to_charlist(line, &mut continuation.strings))
                            .collect();
                        Value::list_from_vec(lines)
                    }
                    "date" => str_to_charlist(date, &mut continuation.strings),
                    "footer" => str_to_charlist("", &mut continuation.strings),
                    "check_link" => Value::list_from_vec(vec![]),
                    other => panic!("unhandled effect: {other}"),
                };
                result = continuation.resume(resume_val).unwrap();
            }
        }
    }
}

fn bench_ssg(c: &mut Criterion) {
    let program = compile(SCRIPT).unwrap();

    // Sanity check: make sure it produces output
    let html = run_ssg(&program, MARKDOWN, DATE);
    assert!(html.contains("<h1>"));
    assert!(html.contains("<h2>"));
    assert!(html.contains("<strong>"));
    assert!(html.contains("<em>"));
    assert!(html.contains("<code>"));
    assert!(html.contains("<pre><code>"));
    assert!(html.contains("<ul>"));
    assert!(html.contains("<blockquote>"));
    assert!(html.contains("<a href="));

    c.bench_function("ssg", |b| b.iter(|| run_ssg(&program, MARKDOWN, DATE)));
}

criterion_group!(benches, bench_ssg);
criterion_main!(benches);

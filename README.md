# Kombucha

A minimal and malleable programming language for symbiotic end-user programming.

Kombucha is designed to be the opposite of systems programming languages like Rust: it's intended for _programming in the small_, for _incremental development_ with a _rapid feedback cycle_. It doesn't compete with systems languages, but is meant to extend them _symbiotically_.

(Kombucha is under active development. One key feature that is mostly done is Kombucha's **macro system that preserves static reasoning**: You can define custom control structures and extend the language without sacrificing the ability to analyze and optimize code. For more details, see the [Kombucha macro paper](https://fkettelhoit.com/papers/reasonable-macros-through-explicit-bindings.pdf).)

Here's a taste of Kombucha:

```swift
::gen-section(:title, :section) = {
    match (section) [
        Note([["date", [:yy, :mm, :dd]]]) -> { [
            #"<p><a href="..#notes">Notes</a><br />"#, yy, "/", mm, "/", dd, "</p>"
            "<h1>", escape(title), "</h1>"
        ] }
        Section([["blocks", :blocks]]) -> { [
            blocks |> map(gen-block("<p>", "</p>"))
        ] }
        Section([["title", :title], ["blocks", :blocks]]) -> { [
            "<h2>", escape(title), "</h2>"
            blocks |> map(gen-block("<p>", "</p>"))
        ] }
    ]
}
```

Kombucha is...

### 🎯 **Minimal**

Kombucha provides a small, regular core that can be extended. There are no built-in keywords or control structures with special syntax. Even variable assignment is just a normal function. The language is similar to Lisps in this regard, but provides syntactic sugar that avoids parentheses-heavy syntax.

### 🔧 **Malleable**

Without privileged control structures or special keywords, Kombucha is easy to adapt to different problem domains. Even fundamental features like pattern matching are defined as functions and used like any other function.

### ⚡ **Effectful**

Any symbol ending with `!` is an _effect_—like a resumable exception. Effects stop execution and walk up the call stack until finding a handler that can either abort (like an exception) or resume execution with a result.

### 🤝 **Symbiotic**

Effects don't need to be handled inside Kombucha—they can bubble up to the host language (like Rust), keeping Kombucha small while easily hooking into existing ecosystems.

### 🧠 **Reasonable**

While dynamic and built for fast iteration, Kombucha aims to be easy to statically reason about. Runtime reflection is limited by design, and the language explicitly distinguishes variables being _used_ from variables being _bound_ (prefixed with `:`).

## Quick Examples

### Variables and Bindings

```swift
:x = Foo  // Assigns Foo to variable x
:y = x    // Assigns value of x to variable y
```

### Functions

```swift
// Single argument
:f = (:x => { f(x) })

// Multiple arguments
:g = ([:x, :y] => { f(x, y) })
```

### Multiple Function Call Syntaxes

```swift
// Prefix function call
f(x, y)

// Infix function call
x f y

// Keyword function call
if (x == y) then: {
    // ...
} else: {
    // ...
}
```

### Pattern Matching

```swift
match (x) [
    Foo(Bar, Baz) -> { "just Foo(Bar, Baz)" }
    Foo(:x, :x) -> { twice(x) }             // Bind and match same value
    Foo(:x, y) -> { "second element is y" } // Bind x, match against y's value
    :else -> { throw!("Expected a Foo(...)!") }
]
```

### Resumable Effects

```swift
// foo doesn't care how the read! effect is handled
::foo() = {
    :input = read!()
    match (input) [
        Ok(:contents) -> { contents }
        :else -> { throw!("Couldn't read any input!") }
    ]
}

// We can catch the effect and handle it
try {
    foo()
} catch: read! as: [:resume, :arg] => {
    resume("TestInput")
}
```

### Host Integration

```swift
// read! and print! handled by the host language
:file = read!("foo.txt")
match (file) [
    Ok(:contents) -> { print!(contents) }
    Err(:err) -> { print!("Could not read file.") }
]
```

## Real-World Example

Here's a simple HTML generator showing Kombucha in action:

```swift
::gen-inline(:inline) = {
    match(inline) [
        Em(:content) -> { ["<em>", escape(content), "</em>"] }
        Strong(:content) -> { ["<strong>", escape(content), "</strong>"] }
        Code(:content) -> { ["<code>", escape(content), "</code>"] }
        Link([["href", :href], ["text", :text]]) -> {
            [#"<a href=""#, href, #"">"#, escape(text), "</a>"]
        }
        :string -> { [escape(string)] }
    ]
}

::gen-html(:page) = {
    match (page) [
        Html([["title", :title], ["sections", :sections]]) -> {
            [
                "<!DOCTYPE html>"
                "<html>"
                "<head><title>", escape(title), "</title></head>"
                "<body>"
                sections |> map(gen-section(title))
                "</body>"
                "</html>"
            ]
        }
    ]
}

read!() |> gen-html |> deep-flatten
```

Kombucha's syntax also makes it useful for describing data:

```swift
Html lang: "en" stylesheet: "styles/style.css" title: "Page Title" sections: [
    Note date: ["2025", "06", "12"]
    Section title: "Section Title" blocks: [
        ["This is a ", Strong("paragraph"), " with ", Em("inline"), " ", Code("code"), "."]
        List [
            ["An item with a ", Link href: "http://www.example.com" text: "link", "."]
        ]
        Subsection title: "Subsection Title" blocks: [
            ["Another paragraph."]
            ["Another paragraph."]
        ]
        Code(#"["Foo", "Bar"]"#)
    ]
    Section title: "Section 2" id: "sec2" blocks: [
        ["Not much here."]
    ]
]
```

## Current Status

Kombucha is currently in active development. I'm building it as part of my batch at the [Recurse Center](https://www.recurse.com/).

### Goals

- ✅ Minimal, regular syntax
- ✅ Pattern matching as functions
- ✅ Effect system
- ✅ Host language integration
- ✅ Rust interop through serde
- 🚧 Bytecode VM with Perceus reference counting (in development)
- 🚧 Hot reloading (in development)
- 🚧 Partial evluation
- 🚧 Type system

### Philosophy

Kombucha embraces the idea that code should be _malleable_: easy to reshape and adapt to new requirements. By treating all language constructs as regular functions and providing a powerful effect system, Kombucha enables rapid prototyping and iterative development while maintaining the ability to reason about code statically.

## Contributing

Kombucha is an experimental language in early development. If you're interested in language design, functional programming, or effect systems, reach out! That said, Kombucha is very much a personal project and probably not useful for anyone else at this point.

---

_Want to become a better programmer? [Join the Recurse Center!](https://www.recurse.com/)_

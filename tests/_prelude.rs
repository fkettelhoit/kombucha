use kombucha::{compile::compile, run::State};

#[test]
fn prelude_arrow() -> Result<(), String> {
    let code = "
[
    (Bar -> { Foo })(Bar)
    (:x -> { x })(Foo)
    (:x -> { Foo })(Bar)
    (Pair(Foo, Bar) -> { Foo })(Pair(Foo, Bar))
    (Pair(:x, :y) -> { y })(Pair(Bar, Foo))
    (Pair(:x, Bar) -> { x })(Pair(Foo, Bar))
    (Pair(:x, :x) -> { x })(Pair(Foo, Foo))
    (:foo(:x, :y) -> { x })(Pair(Foo, Bar))
    ([:x, :y] -> { y })([Bar, Foo])
    (Foo(Bar(:x, :y)) -> { x })(Foo(Bar(Foo, Bar)))
    ([[:x, :y]] -> { y })([[Bar, Foo]])
]
";
    match compile(code)?.run().unwrap() {
        State::Done(v) => {
            assert_eq!(v.to_string(), format!("[{}]", ["Foo"; 11].join(", ")))
        }
        State::Resumable(vm) => panic!("{}!({})", vm.effect(), vm.arg.to_string()),
    }
    Ok(())
}

#[test]
fn prelude_match() -> Result<(), String> {
    let code = "
match (Pair first: Foo second: Bar) with: [
    Pair([[\"twice\", :x]]) -> { Twice(x) }
    Pair([[\"first\", :x], [\"second\", :x]]) -> { Twice(x) }
    Pair([[\"first\", :x], [\"second\", :y]]) -> { Different(x, y) }
    Pair([[\"first\", :x], [\"second\", :y]]) -> { invalid-pair!() }
]
";
    match compile(code)?.run().unwrap() {
        State::Done(v) => assert_eq!(v.to_string(), "Different(Foo, Bar)"),
        State::Resumable(vm) => panic!("{}!({})", vm.effect(), vm.arg.to_string()),
    }
    Ok(())
}

#[test]
fn prelude_deep_flatten() -> Result<(), String> {
    let code = "deep-flatten([[[A, B, C]], [D, E, [F, G]]])";
    match compile(code)?.run().unwrap() {
        State::Done(v) => assert_eq!(v.to_string(), "[A, B, C, D, E, F, G]"),
        State::Resumable(vm) => panic!("{}!({})", vm.effect(), vm.arg.to_string()),
    }
    Ok(())
}

#[test]
fn prelude_fn_def() -> Result<(), String> {
    let code = "::foo(:x, :y) = { Foo(x, y) }, :bar = Bar, foo(bar, Baz)";
    match compile(code)?.run().unwrap() {
        State::Done(v) => assert_eq!(v.to_string(), "Foo(Bar, Baz)"),
        State::Resumable(vm) => panic!("{}!({})", vm.effect(), vm.arg.to_string()),
    }
    Ok(())
}

#[test]
fn gen_html() -> Result<(), String> {
    let code = include_str!("../examples/generate_html/main.kb");
    let mut result = compile(code)?.run().unwrap();
    loop {
        match result {
            State::Done(v) => {
                let result: Vec<String> = v.deserialize().unwrap();
                assert_eq!(result.join(""), include_str!("../examples/generate_html/page.html"));
                return Ok(());
            }
            State::Resumable(mut vm) => match vm.effect() {
                "read" => {
                    let start =
                        vm.arg.bytecode.load(include_str!("../examples/generate_html/page.kb"))?;
                    result = vm.resume_at(start).unwrap();
                }
                "escape" => {
                    let str = vm.arg.deserialize::<String>().unwrap();
                    let escaped = str
                        .replace("&", "&amp;")
                        .replace("<", "&lt;")
                        .replace(">", "&gt;")
                        .replace("\"", "&quot;")
                        .replace("'", "&apos;");
                    let arg = vm.serialize(&escaped).unwrap();
                    result = vm.resume(arg).unwrap();
                }
                eff => panic!("{eff}!({})", vm.arg.to_string()),
            },
        }
    }
}

#[test]
fn md_to_html() -> Result<(), String> {
    let code = include_str!("../examples/md_to_html/main.kb");
    let mut result = compile(code)?.run().unwrap();
    loop {
        match result {
            State::Done(v) => {
                let result: Vec<String> = v.deserialize().unwrap();
                assert_eq!(result.join(""), include_str!("../examples/md_to_html/2025-06-12.html"));
                return Ok(());
            }
            State::Resumable(mut vm) => match vm.effect() {
                "print" => {
                    println!("{}", vm.arg.pretty());
                    let nil = vm.arg.bytecode.serialize(&()).unwrap();
                    result = vm.resume(nil).unwrap();
                }
                "date" => {
                    let date = vm.arg.bytecode.serialize(&"2025/06/12").unwrap();
                    result = vm.resume(date).unwrap();
                }
                "chars" => {
                    let chars: Vec<char> =
                        include_str!("../examples/md_to_html/2025-06-12.md").chars().collect();
                    let arg = vm.arg.bytecode.serialize(&chars).map_err(|e| e.to_string())?;
                    result = vm.resume(arg).unwrap()
                }
                "escape" => {
                    let strs = vm.arg.deserialize::<Vec<String>>().unwrap();
                    let escaped = strs
                        .into_iter()
                        .map(|s| {
                            s.replace("&", "&amp;")
                                .replace("<", "&lt;")
                                .replace(">", "&gt;")
                                .replace("\"", "&quot;")
                                .replace("'", "&apos;")
                        })
                        .collect::<Vec<String>>();
                    let arg = vm.serialize(&escaped).unwrap();
                    result = vm.resume(arg).unwrap();
                }
                eff => panic!("{eff}!({})", vm.arg.to_string()),
            },
        }
    }
}

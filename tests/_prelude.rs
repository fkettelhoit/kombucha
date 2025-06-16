use vorpal::{compile::compile, run::State};

#[test]
fn prelude_match_pair() -> Result<(), String> {
    let code = "
match (Pair first: Foo second: Foo) with: [
    Pair([[\"twice\", :x]]) -> { Twice(x) }
    Pair([[\"first\", :x], [\"second\", :x]]) -> { Twice(x) }
    Pair([[\"first\", :x], [\"second\", :y]]) -> { invalid-pair!() }
]
";
    let bytecode = compile(code)?;
    match bytecode.run().unwrap() {
        State::Done(v) => assert_eq!(v.to_string(), "Twice(Foo)"),
        State::Resumable(vm) => panic!("{}!({})", vm.effect(), vm.arg.to_string()),
    }
    Ok(())
}

#[test]
fn prelude_deep_flatten() -> Result<(), String> {
    let code = "deep-flatten([[[A, B, C]], [D, E, [F, G]]])";
    let bytecode = compile(code)?;
    match bytecode.run().unwrap() {
        State::Done(v) => assert_eq!(v.to_string(), "[A, B, C, D, E, F, G]"),
        State::Resumable(vm) => panic!("{}!({})", vm.effect(), vm.arg.to_string()),
    }
    Ok(())
}

#[test]
fn prelude_fn_def() -> Result<(), String> {
    let code = "::foo(:x, :y) = { Foo(x, y) }, :bar = Bar, foo(bar, Baz)";
    let bytecode = compile(code)?;
    match bytecode.run().unwrap() {
        State::Done(v) => assert_eq!(v.to_string(), "Foo(Bar, Baz)"),
        State::Resumable(vm) => panic!("{}!({})", vm.effect(), vm.arg.to_string()),
    }
    Ok(())
}

#[test]
fn prelude_generate_html2() -> Result<(), String> {
    let code = include_str!("../examples/gen_html.vo");
    let bytecode = compile(code)?;
    let mut result = bytecode.run().unwrap();
    loop {
        match result {
            State::Done(v) => {
                let result: Vec<String> = v.deserialize().unwrap();
                assert_eq!(result.join(""), include_str!("../examples/gen_html.page.html"));
                return Ok(());
            }
            State::Resumable(mut vm) => match vm.effect() {
                "read" => {
                    let start =
                        vm.arg.bytecode.load(include_str!("../examples/gen_html.page.vo"))?;
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

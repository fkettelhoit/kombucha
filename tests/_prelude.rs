use vorpal::{compile::compile, run::State};

#[test]
fn prelude_match_pair() -> Result<(), String> {
    let code = "
match (Pair(Foo, Foo)) with: [
    :_(:x, Bar) -> { Bar }
    Pair(:x, :x) -> { Twice(x) }
]
";
    let bytecode = compile(code)?;
    match bytecode.run().unwrap() {
        State::Done(v) => assert_eq!(v.pretty(), "Twice(Foo)"),
        State::Resumable(vm) => panic!("{}!({})", vm.effect(), vm.arg_pretty()),
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
                let result: Vec<Vec<String>> = v.deserialize().unwrap();
                assert_eq!(
                    result.iter().map(|l| l.join("")).collect::<Vec<_>>().join("\n"),
                    include_str!("../examples/gen_html.page.html")
                );
                return Ok(());
            }
            State::Resumable(mut vm) => match vm.effect() {
                "read" => {
                    let start = vm.bytecode.load(include_str!("../examples/gen_html.page.vo"))?;
                    result = vm.resume_at(start).unwrap();
                }
                eff => panic!("{eff}!({})", vm.arg_pretty()),
            },
        }
    }
}

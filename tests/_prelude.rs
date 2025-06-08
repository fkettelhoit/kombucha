use vorpal::{compile::compile, run::State};

#[test]
fn prelude_match_pair() -> Result<(), String> {
    let code = "
match: Pair(Foo, Foo) with: [
    '_('x, Bar) -> { Bar }
    Pair('x, 'x) -> { Twice(x) }
]
";
    let bytecode = compile(code).unwrap();
    match bytecode.run().unwrap() {
        State::Done(v) => assert_eq!(v.pretty(), "Twice(Foo)"),
        State::Resumable(_) => panic!("Found a resumable!"),
    }
    Ok(())
}

#[test]
fn prelude_generate_html() -> Result<(), String> {
    let code = include_str!("with_prelude_run_ssg.txt");
    let bytecode = compile(code).unwrap();
    match bytecode.run().unwrap() {
        State::Done(v) => {
            let result: Vec<Vec<String>> = v.deserialize().unwrap();
            assert_eq!(
                result.iter().map(|l| l.join("")).collect::<Vec<_>>().join("\n"),
                include_str!("with_prelude_run_ssg.out.txt")
            )
        }
        State::Resumable(_) => panic!("Found a resumable!"),
    }
    Ok(())
}

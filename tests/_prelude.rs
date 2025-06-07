use vorpal::{
    compile::compile,
    run::{VmState, pretty},
};

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
        VmState::Done(v, strings) => assert_eq!(pretty(&v, &strings), "Twice(Foo)"),
        VmState::Resumable(_, _) => panic!("Found a resumable!"),
    }
    Ok(())
}

#[test]
fn prelude_generate_html() -> Result<(), String> {
    let code = include_str!("with_prelude_run_ssg.txt");
    let bytecode = compile(code).unwrap();
    match bytecode.run().unwrap() {
        VmState::Done(v, strings) => {
            assert_eq!(pretty(&v, &strings), include_str!("with_prelude_run_ssg.out.txt"))
        }
        VmState::Resumable(_, _) => panic!("Found a resumable!"),
    }
    Ok(())
}

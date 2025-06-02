use vorpal::{
    compile::{Expr, abs, app, compile_prg},
    run::{VmState, pretty},
};

#[test]
fn eval_raw_app1() {
    // (\x.x) "Foo"
    let expr = Expr::App(
        Box::new(Expr::Abs(Box::new(Expr::Var(0)))),
        Box::new(Expr::String(0)),
    );
    let strings = vec!["Foo"];
    assert_eq!(eval_expr(expr, strings).unwrap(), "Foo");
}

#[test]
fn eval_raw_app2() {
    // ((\x.\y.y) "Foo") "Bar"
    let expr = Expr::App(
        Box::new(Expr::App(
            Box::new(Expr::Abs(Box::new(Expr::Abs(Box::new(Expr::Var(0)))))),
            Box::new(Expr::String(0)),
        )),
        Box::new(Expr::String(1)),
    );
    let strings = vec!["Foo", "Bar"];
    assert_eq!(eval_expr(expr, strings).unwrap(), "Bar");
}

#[test]
fn eval_raw_app3() {
    // (((\x.\y.\z.Vec(z, x, z)) "Foo") "Bar") "Baz"
    let expr = Expr::App(
        Box::new(Expr::App(
            Box::new(Expr::App(
                Box::new(Expr::Abs(Box::new(Expr::Abs(Box::new(Expr::Abs(
                    Box::new(Expr::App(
                        Box::new(Expr::App(
                            Box::new(Expr::App(Box::new(Expr::String(3)), Box::new(Expr::Var(0)))),
                            Box::new(Expr::Var(2)),
                        )),
                        Box::new(Expr::Var(0)),
                    )),
                )))))),
                Box::new(Expr::String(0)),
            )),
            Box::new(Expr::String(1)),
        )),
        Box::new(Expr::String(2)),
    );
    let strings = vec!["Foo", "Bar", "Baz", "Vec"];
    assert_eq!(eval_expr(expr, strings).unwrap(), "Vec(Baz, Foo, Baz)");
}

#[test]
fn eval_raw_app4() {
    // (\x.(x(\x."Foo") "Bar")) "Baz"
    let expr = Expr::App(
        Box::new(Expr::Abs(Box::new(Expr::App(
            Box::new(Expr::Var(0)),
            Box::new(Expr::App(
                Box::new(Expr::Abs(Box::new(Expr::String(0)))),
                Box::new(Expr::String(1)),
            )),
        )))),
        Box::new(Expr::String(2)),
    );
    let strings = vec!["Foo", "Bar", "Baz"];
    assert_eq!(eval_expr(expr, strings).unwrap(), "Baz(Foo)");
}

#[test]
fn eval_raw_if() {
    // if(Foo, Foo, { True }, { False })
    let t = abs(Expr::String(1));
    let f = abs(Expr::String(2));
    let if_fn = abs(abs(abs(abs(Expr::Cmp(
        Box::new(Expr::Var(3)),
        Box::new(Expr::Var(2)),
        Box::new(Expr::Var(1)),
        Box::new(Expr::Var(0)),
    )))));
    let expr = app(app(app(app(if_fn, Expr::String(0)), Expr::String(0)), t), f);
    let strings = vec!["Foo", "True", "False"];
    assert_eq!(eval_expr(expr, strings).unwrap(), "True");
}

#[test]
fn eval_raw_pop() {
    // pop(Foo(Bar), 'f => { 'x => { x } }, { Error })
    let foo_bar = app(Expr::String(0), Expr::String(1));
    let t = abs(abs(Expr::Var(0)));
    let f = abs(Expr::String(2));
    let pop_fn = abs(abs(abs(Expr::Unpack(
        Box::new(Expr::Var(2)),
        Box::new(Expr::Var(1)),
        Box::new(Expr::Var(0)),
    ))));
    let expr = app(app(app(pop_fn, foo_bar), t), f);
    let strings = vec!["Foo", "Bar", "Error"];
    assert_eq!(eval_expr(expr, strings).unwrap(), "Bar");
}

fn eval_expr(expr: Expr, strings: Vec<&str>) -> Result<String, String> {
    let bytecode = compile_prg(expr, strings.into_iter().map(|s| s.to_string()).collect());
    match bytecode.run() {
        Ok(VmState::Done(v, strs)) => Ok(pretty(&v, &strs)),
        Ok(VmState::Resumable(res, arg)) => Err(format!(
            "{}!({})",
            res.get_effect_name().unwrap(),
            pretty(&arg, res.strings())
        )),
        Err(e) => Err(format!("Error at op {e}")),
    }
}

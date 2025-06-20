use std::collections::HashMap;

use serde::Deserialize;
use vorpal::{
    compile::compile,
    run::{State, Value},
    serde::de::Error,
};

fn run(code: &str) -> Option<Value> {
    match compile(code).unwrap().run().unwrap() {
        State::Done(value) => Some(value),
        State::Resumable(_) => None,
    }
}

#[test]
fn deserialize_bool() -> Result<(), Error> {
    assert_eq!(run("True").unwrap().deserialize::<bool>()?, true);
    assert_eq!(run("False").unwrap().deserialize::<bool>()?, false);
    assert!(run("Foo").unwrap().deserialize::<bool>().is_err());
    Ok(())
}

#[test]
fn deserialize_char() -> Result<(), Error> {
    assert_eq!(run("\"c\"").unwrap().deserialize::<char>()?, 'c');
    assert!(run("Foo").unwrap().deserialize::<char>().is_err());
    Ok(())
}

#[test]
fn deserialize_string() -> Result<(), Error> {
    assert_eq!(run("\"Foo\"").unwrap().deserialize::<String>()?, "Foo");
    assert_eq!(run("Foo").unwrap().deserialize::<String>()?, "Foo");
    assert_eq!(run(":foo").unwrap().deserialize::<String>()?, "foo");
    assert_eq!(run("::foo").unwrap().deserialize::<String>()?, "foo");
    assert_eq!(run("foo!").unwrap().deserialize::<String>()?, "foo");
    Ok(())
}

#[test]
fn deserialize_option() -> Result<(), Error> {
    assert_eq!(run("None").unwrap().deserialize::<Option<bool>>()?, None);
    assert_eq!(run("Some(True)").unwrap().deserialize::<Option<bool>>()?, Some(true));
    assert_eq!(run("[]").unwrap().deserialize::<Option<bool>>()?, None);
    assert_eq!(run("True").unwrap().deserialize::<Option<bool>>()?, Some(true));
    Ok(())
}

#[test]
fn deserialize_unit() -> Result<(), Error> {
    assert_eq!(run("[]").unwrap().deserialize::<()>()?, ());
    Ok(())
}

#[derive(Debug, PartialEq, Eq, Deserialize)]
struct Foo;

#[test]
fn deserialize_unit_struct() -> Result<(), Error> {
    assert_eq!(run("Foo").unwrap().deserialize::<Foo>()?, Foo);
    assert!(run("\"Foo\"").unwrap().deserialize::<Foo>().is_err());
    Ok(())
}

#[derive(Debug, PartialEq, Eq, Deserialize)]
struct Bar(bool);

#[test]
fn deserialize_newtype_struct() -> Result<(), Error> {
    assert_eq!(run("Bar(True)").unwrap().deserialize::<Bar>()?, Bar(true));
    assert_eq!(run("True").unwrap().deserialize::<Bar>()?, Bar(true));
    Ok(())
}

#[test]
fn deserialize_seq() -> Result<(), Error> {
    assert_eq!(run("[Foo, Bar]").unwrap().deserialize::<Vec<String>>()?, vec!["Foo", "Bar"]);
    assert_eq!(run("[Foo, Foo]").unwrap().deserialize::<Vec<Foo>>()?, vec![Foo, Foo]);
    Ok(())
}

#[test]
fn deserialize_tuple() -> Result<(), Error> {
    assert_eq!(run("[True, Foo]").unwrap().deserialize::<(bool, Foo)>()?, (true, Foo));
    Ok(())
}

#[derive(Debug, PartialEq, Eq, Deserialize)]
struct Baz(bool, char);

#[test]
fn deserialize_tuple_struct() -> Result<(), Error> {
    assert_eq!(run("Baz(True, X)").unwrap().deserialize::<Baz>()?, Baz(true, 'X'));
    Ok(())
}

#[test]
fn deserialize_map() -> Result<(), Error> {
    assert_eq!(
        run("[[A, X], [B, Y]]").unwrap().deserialize::<HashMap<String, String>>()?,
        HashMap::from_iter(vec![("A".into(), "X".into()), ("B".into(), "Y".into())]),
    );
    Ok(())
}

#[derive(Debug, PartialEq, Eq, Deserialize)]
struct Qux {
    foo: bool,
    bar: char,
}

#[test]
fn deserialize_struct() -> Result<(), Error> {
    assert_eq!(
        run("Qux foo: True bar: X").unwrap().deserialize::<Qux>()?,
        Qux { foo: true, bar: 'X' }
    );
    assert!(run("Qux baz: True bar: X").unwrap().deserialize::<Qux>().is_err());
    assert_eq!(run("Qux(True, \"X\")").unwrap().deserialize::<Qux>()?, Qux { foo: true, bar: 'X' });
    Ok(())
}

#[derive(Debug, PartialEq, Eq, Deserialize)]
enum FooBarBaz {
    Foo(bool),
    Bar(bool, char),
    Baz { x: char, y: char },
}

#[test]
fn deserialize_enum() -> Result<(), Error> {
    assert_eq!(run("Foo(True)").unwrap().deserialize::<FooBarBaz>()?, FooBarBaz::Foo(true));
    assert_eq!(
        run("Bar(False, X)").unwrap().deserialize::<FooBarBaz>()?,
        FooBarBaz::Bar(false, 'X')
    );
    assert_eq!(
        run("Baz x: X y: Y").unwrap().deserialize::<FooBarBaz>()?,
        FooBarBaz::Baz { x: 'X', y: 'Y' }
    );
    assert!(run("Baz x: X z: Z").unwrap().deserialize::<FooBarBaz>().is_err());
    assert_eq!(
        run("Baz(X, Y)").unwrap().deserialize::<FooBarBaz>()?,
        FooBarBaz::Baz { x: 'X', y: 'Y' }
    );
    Ok(())
}

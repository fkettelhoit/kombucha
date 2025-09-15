use std::collections::BTreeMap;

use kombucha::bytecode::Bytecode;
use serde::Serialize;
use serde_json::Error;

fn serialize<T: Serialize>(v: &T) -> Result<String, Error> {
    let mut bytecode = Bytecode::default();
    let v = bytecode.serialize(&v)?;
    Ok(v.pretty(&bytecode.ctx.strs))
}

#[test]
fn serialize_bool() -> Result<(), Error> {
    assert_eq!(serialize(&true)?, "True");
    Ok(())
}

#[test]
fn serialize_char() -> Result<(), Error> {
    assert_eq!(serialize(&'x')?, "\"x\"");
    assert_eq!(serialize(&'X')?, "\"X\"");
    Ok(())
}

#[test]
fn serialize_str() -> Result<(), Error> {
    assert_eq!(serialize(&"foo bar")?, "\"foo bar\"");
    assert_eq!(serialize(&"foo")?, "\"foo\"");
    Ok(())
}

#[test]
fn serialize_none() -> Result<(), Error> {
    // assert_eq!(serialize::<Option<bool>>(&None)?, "None");
    // TODO:
    assert_eq!(serialize::<Option<bool>>(&None)?, "[]");
    Ok(())
}

#[test]
fn serialize_some() -> Result<(), Error> {
    // assert_eq!(serialize::<Option<bool>>(&Some(true))?, "Some(True)");
    assert_eq!(serialize::<Option<bool>>(&Some(true))?, "True");
    Ok(())
}

#[test]
fn serialize_unit() -> Result<(), Error> {
    assert_eq!(serialize(&())?, "[]");
    Ok(())
}

#[derive(Serialize)]
struct Foo;

#[test]
fn serialize_unit_struct() -> Result<(), Error> {
    // assert_eq!(serialize(&Foo)?, "Foo");
    assert_eq!(serialize(&Foo)?, "[]");
    Ok(())
}

#[derive(Serialize)]
struct Bar(bool);

#[test]
fn serialize_newtype_struct() -> Result<(), Error> {
    // assert_eq!(serialize(&Bar(true))?, "Bar(True)");
    assert_eq!(serialize(&Bar(true))?, "True");
    Ok(())
}

#[derive(Serialize)]
enum FooBarBaz {
    Foo(bool),
    Bar(bool, char),
    Baz { x: char, y: char },
}

#[test]
fn serialize_newtype_variant() -> Result<(), Error> {
    // assert_eq!(serialize(&FooBarBaz::Foo(true))?, "Foo(True)");
    assert_eq!(serialize(&FooBarBaz::Foo(true))?, "[[\"Foo\", True]]");
    Ok(())
}

#[test]
fn serialize_seq() -> Result<(), Error> {
    assert_eq!(serialize(&[true, false])?, "[True, False]");
    Ok(())
}

#[test]
fn serialize_tuple() -> Result<(), Error> {
    assert_eq!(serialize(&(true, "Foo"))?, "[True, \"Foo\"]");
    Ok(())
}

#[derive(Serialize)]
struct Baz(bool, char);

#[test]
fn serialize_tuple_struct() -> Result<(), Error> {
    // assert_eq!(serialize(&Baz(true, 'X'))?, "Baz(True, \"X\")");
    assert_eq!(serialize(&Baz(true, 'X'))?, "[True, \"X\"]");
    Ok(())
}

#[test]
fn serialize_tuple_variant() -> Result<(), Error> {
    // assert_eq!(serialize(&FooBarBaz::Bar(true, 'X'))?, "Bar(True, \"X\")");
    assert_eq!(serialize(&FooBarBaz::Bar(true, 'X'))?, "[[\"Bar\", [True, \"X\"]]]");
    Ok(())
}

#[test]
fn serialize_map() -> Result<(), Error> {
    let m: BTreeMap<&str, bool> = BTreeMap::from_iter(vec![("x", true), ("y", false)]);
    assert_eq!(serialize(&m)?, "[[\"x\", True], [\"y\", False]]");
    Ok(())
}

#[derive(Serialize)]
struct Qux {
    foo: bool,
    bar: char,
}

#[test]
fn serialize_struct() -> Result<(), Error> {
    // assert_eq!(
    //     serialize(&Qux { foo: true, bar: 'X' })?,
    //     "Qux([[\"foo\", True], [\"bar\", \"X\"]])"
    // );
    assert_eq!(serialize(&Qux { foo: true, bar: 'X' })?, "[[\"bar\", \"X\"], [\"foo\", True]]");
    Ok(())
}

#[test]
fn serialize_struct_variant() -> Result<(), Error> {
    // assert_eq!(
    //     serialize(&FooBarBaz::Baz { x: 'X', y: 'Y' })?,
    //     "Baz([[\"x\", \"X\"], [\"y\", \"Y\"]])"
    // );
    assert_eq!(
        serialize(&FooBarBaz::Baz { x: 'X', y: 'Y' })?,
        "[[\"Baz\", [[\"x\", \"X\"], [\"y\", \"Y\"]]]]"
    );
    Ok(())
}

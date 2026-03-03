use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use kombucha::v0::*;

// ---------------------------------------------------------------------------
// Program sources (same definitions as in the test suite)
// ---------------------------------------------------------------------------

const BUILTIN_FACTORIAL: &str = "
    factorial = (n) {
        if (n == 0) { 1 } else: { n * factorial(n - 1) }
    }";

const BUILTIN_FIB: &str = "
    fib = (n) {
        if (n < 2) { n } else: { fib(n - 1) + fib(n - 2) }
    }";

const HOST_FACTORIAL: &str = "
    factorial = (n) {
        if (eq!(n, 0)) { 1 } else: { mul!(n, factorial(sub!(n, 1))) }
    }";

const HOST_FIB: &str = "
    fib = (n) {
        if (lt!(n, 2)) { n } else: { add!(fib(sub!(n, 1)), fib(sub!(n, 2))) }
    }";

const PEANO_PRELUDE: &str = "
    to_peano = (n) {
        if (n == 0) { Z } else: { S(to_peano(n - 1)) }
    }
    from_peano = (p) {
        match (p) [
            Z -> { 0 }
            S(n) -> { 1 + from_peano(n) }
        ]
    }
    peano_add = (a, b) {
        match (a) [
            Z -> { b }
            S(n) -> { S(peano_add(n, b)) }
        ]
    }
    peano_sub = (a, b) {
        match (b) [
            Z -> { a }
            S(m) -> {
                match (a) [
                    Z -> { Z }
                    S(n) -> { peano_sub(n, m) }
                ]
            }
        ]
    }
    peano_mul = (a, b) {
        match (a) [
            Z -> { Z }
            S(n) -> { peano_add(b, peano_mul(n, b)) }
        ]
    }
    peano_eq = (a, b) {
        match (a) [
            Z -> { match (b) [ Z -> { True }, S(m) -> { False } ] }
            S(n) -> { match (b) [ Z -> { False }, S(m) -> { peano_eq(n, m) } ] }
        ]
    }
    peano_lt = (a, b) {
        match (b) [
            Z -> { False }
            S(m) -> { match (a) [ Z -> { True }, S(n) -> { peano_lt(n, m) } ] }
        ]
    }
";

// ---------------------------------------------------------------------------
// Pure Rust implementations (same algorithms as builtin, for comparison)
// ---------------------------------------------------------------------------

fn rust_factorial(n: i64) -> i64 {
    if n == 0 { 1 } else { n * rust_factorial(n - 1) }
}

fn rust_fib(n: i64) -> i64 {
    if n < 2 { n } else { rust_fib(n - 1) + rust_fib(n - 2) }
}

fn rust_sum_to(n: i64) -> i64 {
    if n == 0 { 0 } else { n + rust_sum_to(n - 1) }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn run_builtin(program: &Program) -> Value {
    match run_program(program).unwrap() {
        VMResult::Done(val, _) => val,
        VMResult::Effect { name, .. } => panic!("Unexpected effect: {name}!"),
    }
}

fn run_host(program: &Program) -> Value {
    run_with_arithmetic_host(program).unwrap()
}

// ---------------------------------------------------------------------------
// Benchmarks
// ---------------------------------------------------------------------------

fn bench_factorial(c: &mut Criterion) {
    let mut group = c.benchmark_group("factorial");

    // n=7 keeps Peano feasible (7! = 5040 unary nodes)
    let n = 7;

    let builtin = compile(&format!("{BUILTIN_FACTORIAL}\nfactorial({n})")).unwrap();
    let host = compile(&format!("{HOST_FACTORIAL}\nfactorial({n})")).unwrap();
    let peano = compile(&format!(
        "{PEANO_PRELUDE}
        factorial = (n) {{
            if (peano_eq(n, Z)) {{ S(Z) }} else: {{ peano_mul(n, factorial(peano_sub(n, S(Z)))) }}
        }}
        from_peano(factorial(to_peano({n})))"
    ))
    .unwrap();

    group.bench_with_input(BenchmarkId::new("rust", n), &n, |b, &n| b.iter(|| rust_factorial(n)));
    group.bench_with_input(BenchmarkId::new("builtin", n), &builtin, |b, prog| {
        b.iter(|| run_builtin(prog))
    });
    group.bench_with_input(BenchmarkId::new("host", n), &host, |b, prog| b.iter(|| run_host(prog)));
    group.bench_with_input(BenchmarkId::new("peano", n), &peano, |b, prog| {
        b.iter(|| run_builtin(prog))
    });

    group.finish();
}

fn bench_fib(c: &mut Criterion) {
    let mut group = c.benchmark_group("fib");

    // fib(10) = 55: max Peano number is 55 nodes, feasible
    let n = 10;

    let builtin = compile(&format!("{BUILTIN_FIB}\nfib({n})")).unwrap();
    let host = compile(&format!("{HOST_FIB}\nfib({n})")).unwrap();
    let peano = compile(&format!(
        "{PEANO_PRELUDE}
        fib = (n) {{
            if (peano_lt(n, S(S(Z)))) {{ n }} else: {{
                peano_add(fib(peano_sub(n, S(Z))), fib(peano_sub(n, S(S(Z)))))
            }}
        }}
        from_peano(fib(to_peano({n})))"
    ))
    .unwrap();

    group.bench_with_input(BenchmarkId::new("rust", n), &n, |b, &n| b.iter(|| rust_fib(n)));
    group.bench_with_input(BenchmarkId::new("builtin", n), &builtin, |b, prog| {
        b.iter(|| run_builtin(prog))
    });
    group.bench_with_input(BenchmarkId::new("host", n), &host, |b, prog| b.iter(|| run_host(prog)));
    group.bench_with_input(BenchmarkId::new("peano", n), &peano, |b, prog| {
        b.iter(|| run_builtin(prog))
    });

    group.finish();
}

fn bench_sum_to(c: &mut Criterion) {
    let mut group = c.benchmark_group("sum_to");

    // sum_to(10) = 55: max Peano number is 55 nodes, feasible
    let n = 10;

    let builtin = compile(&format!(
        "sum_to = (n) {{
            if (n == 0) {{ 0 }} else: {{ n + sum_to(n - 1) }}
        }}
        sum_to({n})"
    ))
    .unwrap();
    let host = compile(&format!(
        "sum_to = (n) {{
            if (eq!(n, 0)) {{ 0 }} else: {{ add!(n, sum_to(sub!(n, 1))) }}
        }}
        sum_to({n})"
    ))
    .unwrap();
    let peano = compile(&format!(
        "{PEANO_PRELUDE}
        sum_to = (n) {{
            if (peano_eq(n, Z)) {{ Z }} else: {{ peano_add(n, sum_to(peano_sub(n, S(Z)))) }}
        }}
        from_peano(sum_to(to_peano({n})))"
    ))
    .unwrap();

    group.bench_with_input(BenchmarkId::new("rust", n), &n, |b, &n| b.iter(|| rust_sum_to(n)));
    group.bench_with_input(BenchmarkId::new("builtin", n), &builtin, |b, prog| {
        b.iter(|| run_builtin(prog))
    });
    group.bench_with_input(BenchmarkId::new("host", n), &host, |b, prog| b.iter(|| run_host(prog)));
    group.bench_with_input(BenchmarkId::new("peano", n), &peano, |b, prog| {
        b.iter(|| run_builtin(prog))
    });

    group.finish();
}

criterion_group!(benches, bench_factorial, bench_fib, bench_sum_to);
criterion_main!(benches);

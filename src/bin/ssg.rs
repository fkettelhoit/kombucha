use kombucha::v0::{Program, VMResult, Value, compile, flatten, run_program, str_to_charlist};
use std::path::{Path, PathBuf};
use std::{env, fs};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: ssg <path> [--footer <file>] [--index <file>]");
        std::process::exit(1);
    }

    let input_path = &args[1];
    let footer = parse_flag(&args, "--footer")
        .map(|path| {
            fs::read_to_string(&path).unwrap_or_else(|e| {
                eprintln!("cannot read footer file {path}: {e}");
                std::process::exit(1);
            })
        })
        .unwrap_or_default();

    let script = include_str!("../../examples/ssg.kb");
    let program = compile(script).unwrap_or_else(|e| {
        eprintln!("compile error: {e}");
        std::process::exit(1);
    });

    let meta = fs::metadata(input_path).unwrap_or_else(|e| {
        eprintln!("cannot stat {input_path}: {e}");
        std::process::exit(1);
    });

    if meta.is_dir() {
        let mut md_files = Vec::new();
        find_md_files(Path::new(input_path), &mut md_files);
        md_files.sort();
        for path in &md_files {
            process_file(&program, path.to_str().unwrap(), &footer);
        }
        if let Some(index_path) = parse_flag(&args, "--index") {
            generate_index(input_path, &index_path);
        }
    } else {
        process_file(&program, input_path, &footer);
    }
}

fn parse_flag(args: &[String], flag: &str) -> Option<String> {
    args.iter().position(|a| a == flag).and_then(|i| args.get(i + 1).cloned())
}

fn process_file(program: &Program, md_path: &str, footer: &str) {
    let md_content = fs::read_to_string(md_path).unwrap_or_else(|e| {
        eprintln!("cannot read {md_path}: {e}");
        std::process::exit(1);
    });
    let date = extract_date(md_path);
    let out_dir = Path::new(md_path).parent().unwrap();
    let out_path = md_path.replace(".md", ".html");
    println!("{out_path}");

    let mut result = run_program(program).unwrap_or_else(|e| {
        eprintln!("runtime error: {e}");
        std::process::exit(1);
    });

    loop {
        match result {
            VMResult::Done(val, strings) => {
                let mut output = String::new();
                flatten(&val, &strings, &mut output);
                fs::write(&out_path, &output).expect("cannot write output");
                return;
            }
            VMResult::Effect { name, args, mut continuation } => {
                let resume_val = match name.as_str() {
                    "input" => {
                        let lines: Vec<Value> = md_content
                            .lines()
                            .map(|line| str_to_charlist(line, &mut continuation.strings))
                            .collect();
                        Value::list_from_vec(lines)
                    }
                    "date" => str_to_charlist(&date, &mut continuation.strings),
                    "footer" => str_to_charlist(footer, &mut continuation.strings),
                    "check_link" => {
                        if let Some(url_val) = args.first() {
                            let mut url = String::new();
                            flatten(url_val, &continuation.strings, &mut url);
                            check_link(&url, out_dir, md_path);
                        }
                        Value::list_from_vec(vec![])
                    }
                    other => {
                        eprintln!("unhandled effect: {other}");
                        std::process::exit(1);
                    }
                };
                result = continuation.resume(resume_val).unwrap_or_else(|e| {
                    eprintln!("runtime error: {e}");
                    std::process::exit(1);
                });
            }
        }
    }
}

fn check_link(url: &str, dir: &Path, source: &str) {
    if url.is_empty()
        || url.starts_with("http://")
        || url.starts_with("https://")
        || url.starts_with("mailto:")
        || url.starts_with("ftp://")
        || url.starts_with('#')
        || url.starts_with("//")
    {
        return;
    }
    let path = url.split('#').next().unwrap();
    if !dir.join(path).exists() {
        eprintln!("broken link: {url} (in {source})");
    }
}

fn generate_index(notes_dir: &str, index_path: &str) {
    let notes_abs = Path::new(notes_dir).canonicalize().unwrap();
    let index_abs = Path::new(index_path).canonicalize().unwrap();
    let base_dir = index_abs.parent().unwrap();

    let mut md_files = Vec::new();
    find_md_files(&notes_abs, &mut md_files);
    md_files.sort();
    md_files.reverse();

    let mut html = String::new();
    let mut current_ym = String::new();

    for path in &md_files {
        let content = fs::read_to_string(path).unwrap_or_else(|e| {
            eprintln!("cannot read {}: {e}", path.display());
            std::process::exit(1);
        });
        let title = content
            .lines()
            .find(|l| l.starts_with("# "))
            .map(|l| l[2..].trim())
            .unwrap_or("Untitled");

        let month = path.parent().unwrap().file_name().unwrap().to_str().unwrap();
        let year = path.parent().unwrap().parent().unwrap().file_name().unwrap().to_str().unwrap();

        let ym = format!("{year}/{month}");
        if ym != current_ym {
            if !current_ym.is_empty() {
                html.push_str("</ul>\n");
            }
            html.push_str(&format!("<h3>{ym}</h3>\n<ul>\n"));
            current_ym = ym;
        }

        let html_path = path.with_extension("html");
        let href = html_path.strip_prefix(base_dir).unwrap();
        html.push_str(&format!("<li><a href=\"{}\">{}</a></li>\n", href.display(), title));
    }
    if !current_ym.is_empty() {
        html.push_str("</ul>");
    }

    let template = fs::read_to_string(index_path).unwrap_or_else(|e| {
        eprintln!("cannot read {index_path}: {e}");
        std::process::exit(1);
    });
    let begin = "<!-- begin notes -->";
    let end = "<!-- end notes -->";
    let (Some(start_pos), Some(end_pos)) = (template.find(begin), template.find(end)) else {
        eprintln!("warning: {index_path} missing {begin} / {end} markers, skipping");
        return;
    };
    let replacement = format!("{begin}\n{html}\n{end}");
    let output =
        format!("{}{}{}", &template[..start_pos], replacement, &template[end_pos + end.len()..]);
    fs::write(index_path, output).unwrap_or_else(|e| {
        eprintln!("cannot write {index_path}: {e}");
        std::process::exit(1);
    });
    println!("{index_path}");
}

fn find_md_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let entries = fs::read_dir(dir).unwrap_or_else(|e| {
        eprintln!("cannot read directory {}: {e}", dir.display());
        std::process::exit(1);
    });
    for entry in entries.filter_map(|e| e.ok()) {
        let path = entry.path();
        if path.is_dir() {
            find_md_files(&path, out);
        } else if path.extension().is_some_and(|ext| ext == "md") {
            out.push(path);
        }
    }
}

/// Extract date from path: "notes/2025/06/12.md" -> "2025/06/12"
fn extract_date(path: &str) -> String {
    let p = Path::new(path);
    let day = p.file_stem().and_then(|s| s.to_str()).unwrap_or("");
    let month = p.parent().and_then(|p| p.file_name()).and_then(|s| s.to_str()).unwrap_or("");
    let year = p
        .parent()
        .and_then(|p| p.parent())
        .and_then(|p| p.file_name())
        .and_then(|s| s.to_str())
        .unwrap_or("");
    format!("{year}/{month}/{day}")
}

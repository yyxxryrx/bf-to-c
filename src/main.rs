mod ast;
mod bf_ir;
mod cli;
mod generate;
mod lexer;

use crate::ast::{optimal_ast, parse_ast};
use crate::bf_ir::{generate_ir, optimize_ir};
use crate::cli::{Cli, Language};
use crate::lexer::{Token, lexer_all};
use std::error::Error;
use std::fmt::Formatter;
use std::io::{Read, Write};

const FILE_HEAD: &'static str = "#include <stdio.h>\n#include <stdint.h>\n\nint cur = 15000;\nuint8_t buffer[30000];\n\nint main() {\n";

fn get(count: usize, sub: bool, name: Option<&str>) -> String {
    if count == 0 {
        name.map(|name| name.to_string()).unwrap_or_default()
    } else if count == 1 {
        if sub {
            name.map(|name| format!("--{name}"))
                .unwrap_or("--".to_string())
        } else {
            name.map(|name| format!("++{name}"))
                .unwrap_or("++".to_string())
        }
    } else {
        if sub {
            name.map(|name| format!("{name} -= {count}"))
                .unwrap_or(format!(" -= {count}"))
        } else {
            name.map(|name| format!("{name} += {count}"))
                .unwrap_or(format!(" += {count}"))
        }
    }
}

#[derive(Debug)]
struct NotSupportLanguage(String);

impl std::fmt::Display for NotSupportLanguage {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Not support language: {}", self.0)
    }
}

impl Error for NotSupportLanguage {
}

fn run<T>(
    mode: usize,
    fs: &mut T,
    tokens: Vec<(Token, usize)>,
    opt: usize,
    language: Language,
) -> Result<(), Box<dyn Error>>
where
    T: Write,
{
    match mode {
        0 => {
            if !matches!(language, Language::C) {
                return Err(Box::new(NotSupportLanguage("The compiler version is too low".to_string())))
            }
            fs.write_all(FILE_HEAD.as_bytes())?;
            let mut value_change = 0isize;
            let mut index_change = 0isize;
            let mut goto_index = 0usize;
            let mut gotos = vec![];
            for (token, count) in tokens {
                match token {
                    Token::Add => {
                        value_change += count as isize;
                        continue;
                    }
                    Token::Sub => {
                        value_change -= count as isize;
                        continue;
                    }
                    Token::Ignore => continue,
                    _ => {}
                }
                if value_change != 0 {
                    fs.write_all(
                        format!(
                            "    buffer[{}]{};\n",
                            get(index_change.abs() as usize, index_change < 0, Some("cur")),
                            get(value_change.abs() as usize, value_change < 0, None)
                        )
                        .as_bytes(),
                    )?;
                    index_change = 0;
                }
                value_change = 0;
                match token {
                    Token::MoveLeft => index_change -= count as isize,
                    Token::MoveRight => index_change += count as isize,
                    Token::GotoLeft => {
                        for _ in 0..count {
                            if index_change != 0 {
                                fs.write_all(
                                    format!(
                                        "    cur{};\n",
                                        get(index_change.abs() as usize, index_change < 0, None)
                                    )
                                    .as_bytes(),
                                )?;
                                index_change = 0;
                            }
                            gotos.push(goto_index);
                            fs.write_all(format!("label{goto_index}:\n").as_bytes())?;
                            goto_index += 1;
                        }
                    }
                    Token::GotoRight => {
                        for _ in 0..count {
                            let goto_index = gotos.pop().unwrap();
                            fs.write_all(
                                format!(
                                    "    if (buffer[{}] != 0) goto label{goto_index};\n",
                                    get(index_change.abs() as usize, index_change < 0, Some("cur"))
                                )
                                .as_bytes(),
                            )?;
                            index_change = 0;
                        }
                    }
                    Token::Input => {
                        for _ in 0..count {
                            fs.write_all(
                                format!(
                                    "    buffer[{}] = fgetc(stdin);\n",
                                    get(index_change.abs() as usize, index_change < 0, Some("cur"))
                                )
                                .as_bytes(),
                            )?;
                            index_change = 0;
                        }
                    }
                    Token::Output => {
                        for _ in 0..count {
                            fs.write_all(
                                format!(
                                    "    fputc(buffer[{}], stdout);\n",
                                    get(index_change.abs() as usize, index_change < 0, Some("cur"))
                                )
                                .as_bytes(),
                            )?;
                            index_change = 0;
                        }
                    }
                    _ => {}
                }
            }
            fs.write_all("    return 0;\n}\n".as_bytes())?;
        }
        1 => {
            let mut ast = parse_ast(&tokens)?;
            if opt > 0 {
                optimal_ast(&mut ast);
            }
            let mut ir = generate_ir(&ast);
            if opt > 1 {
                optimize_ir(&mut ir);
            }
            let code = language.generate_code(&ir);
            fs.write_all(code.as_bytes())?;
        }
        _ => {}
    }
    Ok(())
}

#[allow(dead_code)]
fn main() -> Result<(), Box<dyn Error>> {
    use clap::Parser;
    let cli = Cli::parse();
    let mut buffer = vec![];
    if cli.input.to_str().unwrap_or_default() == "-" {
        std::io::stdin().lock().read_to_end(&mut buffer)?;
    } else {
        let bf_file = std::fs::File::open(&cli.input)?;
        let mut bf_code = std::io::BufReader::new(bf_file);
        bf_code.read_to_end(&mut buffer)?;
    }
    let code = String::from_utf8(buffer)?;
    let tokens = lexer_all(code)?;
    if cli
        .output
        .as_ref()
        .and_then(|out| out.to_str())
        .unwrap_or_default()
        == "-"
    {
        let mut fs = std::io::stdout();
        run(cli.cv, &mut fs, tokens, cli.opt, cli.language)?;
    } else {
        let mut fs = std::fs::File::create(
            cli.output.unwrap_or(
                format!(
                    "{}.{}",
                    cli.input.file_stem().unwrap_or("out".as_ref()).display(),
                    cli.language.get_ext()
                )
                .into(),
            ),
        )?;
        run(cli.cv, &mut fs, tokens, cli.opt, cli.language)?;
    }
    Ok(())
}

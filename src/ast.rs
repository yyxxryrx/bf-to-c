use crate::ast::error::AstParseResult;
use crate::lexer::Token;

#[derive(Debug)]
pub enum BfAst {
    Add(isize),
    Move(isize),
    Loop(Vec<Self>),
    Input,
    Output,
}

pub fn parse_ast(tokens: &Vec<(Token, usize)>) -> AstParseResult<Vec<BfAst>> {
    let mut ast = vec![];
    let mut tokens = tokens.into_iter();
    while let Some((token, count)) = tokens.next() {
        match token {
            Token::Add => ast.push(BfAst::Add(*count as isize)),
            Token::Sub => ast.push(BfAst::Add(-(*count as isize))),
            Token::MoveRight => ast.push(BfAst::Move(*count as isize)),
            Token::MoveLeft => ast.push(BfAst::Move(-(*count as isize))),
            Token::GotoLeft => {
                let mut depth = 1;
                let mut loop_ast = vec![];
                while depth > 0 {
                    let current = tokens.next();
                    match current {
                        Some((Token::GotoLeft, _)) => {
                            depth += 1;
                            loop_ast.push(*current.unwrap());
                        }
                        Some((Token::GotoRight, _)) => {
                            depth -= 1;
                            if depth > 0 {
                                loop_ast.push(*current.unwrap());
                            }
                        }
                        Some(t) => loop_ast.push(*t),
                        None => return Err(error::AstParseError::ItIsNotClosed),
                    }
                }
                ast.push(BfAst::Loop(parse_ast(&loop_ast)?))
            }
            Token::GotoRight => return Err(error::AstParseError::ItIsNotClosed),
            Token::Output => ast.push(BfAst::Output),
            Token::Input => ast.push(BfAst::Input),
            Token::Ignore => {}
        }
    }
    Ok(ast)
}

pub fn optimal_ast(ast: &mut Vec<BfAst>) {
    let mut last_ast: Option<&BfAst> = None;
    let mut index = 0;
    while index < ast.len() {
        let current_ast = &ast[index];
        match current_ast {
            BfAst::Add(i) => {
                if let Some(BfAst::Add(j)) = last_ast {
                    ast[index] = BfAst::Add(*i + j);
                    ast.remove(index - 1);
                    last_ast = Some(&ast[index - 1]);
                    continue;
                }
            }
            BfAst::Move(i) => {
                if let Some(BfAst::Move(j)) = last_ast {
                    ast[index] = BfAst::Move(i + j);
                    ast.remove(index - 1);
                    last_ast = Some(&ast[index - 1]);
                    continue;
                }
            }
            _ => match &mut ast[index] {
                BfAst::Loop(sub) => {
                    optimal_ast(sub);
                }
                _ => {}
            },
        }
        last_ast = Some(&ast[index]);
        index += 1;
    }
}

pub fn unparse(ast: &Vec<BfAst>) -> String {
    let mut r = String::new();
    fn _up(ast: &Vec<BfAst>, result: &mut String) {
        for i in ast {
            match i {
                BfAst::Add(v) => {
                    if *v >= 0 {
                        *result += &"+".repeat(*v as usize)
                    } else {
                        *result += &"-".repeat((-*v) as usize)
                    }
                }
                BfAst::Move(v) => {
                    if *v >= 0 {
                        *result += &">".repeat(*v as usize)
                    } else {
                        *result += &"<".repeat((-*v) as usize)
                    }
                }
                BfAst::Loop(body) => {
                    *result += "[";
                    _up(body, result);
                    *result += "]";
                }
                BfAst::Input => *result += ",",
                BfAst::Output => *result += ".",
            }
        }
    }
    _up(ast, &mut r);
    r
}

#[macro_export]
macro_rules! bf_ast_print {
    ($tree:expr) => {{
        fn _p(tree: &Vec<$crate::ast::BfAst>, depth: usize) {
            const INDENT: &str = "  ";
            println!("[");
            for ast in tree {
                match ast {
                    $crate::ast::BfAst::Add(i) => println!(
                        "{}\x1b[93mAdd \x1b[94m{i}\x1b[0m,",
                        INDENT.repeat(depth + 1)
                    ),
                    $crate::ast::BfAst::Move(i) => println!(
                        "{}\x1b[93mMove \x1b[94m{i}\x1b[0m,",
                        INDENT.repeat(depth + 1)
                    ),
                    $crate::ast::BfAst::Loop(i) => {
                        print!("{}\x1b[93mLoop\x1b[0m(", INDENT.repeat(depth + 1));
                        _p(i, depth + 1);
                        println!("),");
                    }
                    $crate::ast::BfAst::Input => {
                        println!("{}\x1b[93m{:?}\x1b[0m,", INDENT.repeat(depth + 1), ast)
                    }
                    $crate::ast::BfAst::Output => {
                        println!("{}\x1b[93m{:?}\x1b[0m,", INDENT.repeat(depth + 1), ast)
                    }
                }
            }
            print!("{}]", INDENT.repeat(depth));
            if depth == 0 {
                println!();
            }
        }
        _p($tree, 0);
    }};
}

mod error {
    use std::fmt::{Display, Formatter};

    #[derive(Debug)]
    pub enum AstParseError {
        ItIsNotClosed,
    }

    impl Display for AstParseError {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::ItIsNotClosed => write!(f, "It is not closed"),
            }
        }
    }

    impl std::error::Error for AstParseError {}

    pub type AstParseResult<T> = Result<T, AstParseError>;
}

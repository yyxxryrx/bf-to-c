use nom::bytes::complete::take_while1;
use nom::error::Error;
use nom::multi::many0;
use nom::{IResult, Parser, branch::alt, bytes::complete::tag, multi::many1_count};

const TOKENS: &str = "+-<>[].,";

#[derive(Debug, Clone, Copy)]
pub enum Token {
    Add,
    Sub,
    GotoLeft,
    GotoRight,
    MoveLeft,
    MoveRight,
    Input,
    Output,
    Ignore,
}

impl Token {
    fn token(&self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::GotoLeft => "[",
            Self::GotoRight => "]",
            Self::MoveLeft => "<",
            Self::MoveRight => ">",
            Self::Input => ",",
            Self::Output => ".",
            Self::Ignore => "",
        }
    }

    fn lexer<'a>(&self, input: &'a str) -> IResult<&'a str, ()> {
        let (input, _) = match self {
            Self::Ignore => take_while1(|c| !TOKENS.contains(c))(input)?,
            _ => tag(self.token())(input)?,
        };
        Ok((input, ()))
    }
}

fn lexer_count(token: Token) -> Box<dyn Fn(&str) -> IResult<&str, (Token, usize)>> {
    Box::new(move |input: &str| {
        let (input, count) = many1_count(|i| token.lexer(i)).parse(input)?;
        Ok((input, (token, count)))
    })
}

fn lexer_(token: Token) -> Box<dyn Fn(&str) -> IResult<&str, (Token, usize)>> {
    Box::new(move |input: &str| {
        let (input, _) = token.lexer(input)?;
        Ok((input, (token, 1)))
    })
}

pub fn lexer(input: &str) -> IResult<&str, (Token, usize)> {
    alt((
        lexer_count(Token::Add),
        lexer_count(Token::Sub),
        lexer_(Token::GotoLeft),
        lexer_(Token::GotoRight),
        lexer_count(Token::MoveLeft),
        lexer_count(Token::MoveRight),
        lexer_(Token::Output),
        lexer_(Token::Input),
        lexer_count(Token::Ignore),
    ))
    .parse(input)
}

pub fn lexer_all(
    input: String,
) -> Result<Vec<(Token, usize)>, nom::Err<Error<String>, Error<String>>> {
    let (_, tokens) = many0(lexer).parse(&input).map_err(|e| {
        e.map(|e| Error {
            input: e.input.to_string(),
            code: e.code,
        })
    })?;
    Ok(tokens)
}

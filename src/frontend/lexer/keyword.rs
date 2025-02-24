//! Keyword lexer definitions.

use crate::frontend::lexer::{CharacterMapWindow, Error, ErrorKind, Token, TokenKind, Tokenize};

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Keyword {
    ForEach,
    Match,
    If,
    Then,
    Else,
    Underscore,
    LeftParen,
    RightParen,
    TwoDots,
    Comma,
}

impl Keyword {
    pub fn table() -> impl Iterator<Item = (&'static str, Self)> {
        [
            ("foreach", Keyword::ForEach),
            ("match", Keyword::Match),
            ("if", Keyword::If),
            ("then", Keyword::Then),
            ("else", Keyword::Else),
            ("_", Keyword::Underscore),
            ("(", Keyword::LeftParen),
            (")", Keyword::RightParen),
            ("..", Keyword::TwoDots),
            (",", Keyword::Comma),
        ]
        .into_iter()
    }
}

impl<'a> Tokenize<'a> for Keyword {
    fn tokenize(window: CharacterMapWindow<'a>) -> Result<Token<'a>, Error<'a>> {
        Self::table()
            .find_map(|(tag, t)| {
                window.recognize_tag(tag).map(|w| Token {
                    span: w,
                    kind: TokenKind::Keyword(t),
                })
            })
            .ok_or_else(|| Error::new(window, ErrorKind::NotFound, None))
    }
}

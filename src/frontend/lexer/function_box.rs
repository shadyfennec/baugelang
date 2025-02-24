//! Lexer structures and function to read function boxes.

use std::fmt;

use crate::frontend::{
    is_box_drawing_character,
    lexer::{
        coordinates::{Coords, ICoords, Rectangle},
        heavy, CharacterMapWindow, Error, ErrorKind, Token, TokenKind, Tokenize,
    },
};

/// A function box, with an optional comment:
///
/// ```text
/// ┏━ foo: Int -> String ━━━━━━━━━━━┓
/// ┃                                ┃
/// ┃  Function documentation text.  ┃
/// ┃                                ┃
/// ┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫
/// ┃  #(write your function here)#  ┃
/// ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
/// ```
pub struct FunctionBox<'a> {
    pub signature: CharacterMapWindow<'a>,
    pub definition: CharacterMapWindow<'a>,
    pub comment: Option<CharacterMapWindow<'a>>,
}

impl FunctionBox<'_> {
    /// Returns `true` if the token contains the given point. For a function box,
    /// this includes the box perimeter, including the signature, the
    /// documentation if present; it excludes the definition itself, to allow
    /// for the definition to be lexxed separately.
    pub fn contains_point<C>(&self, c: C, initial_span: Rectangle) -> bool
    where
        C: Into<Coords>,
    {
        let c = c.into();
        let [top, bottom, left, right] = initial_span.borders();

        assert!(top.contains_rectangle(self.signature.rectangle)); // Just to be sure.

        let result = top.contains_point(c)
            || bottom.contains_point(c)
            || left.contains_point(c)
            || right.contains_point(c)
            || self
                .comment
                .as_ref()
                .map(|comment| {
                    comment.rectangle.contains_point(c)
                        || Rectangle::try_from((
                            (left.top_left.x, comment.rectangle.bottom_right.y + 1),
                            (right.top_left.x, comment.rectangle.bottom_right.y + 1),
                        ))
                        .unwrap()
                        .contains_point(c)
                })
                .unwrap_or(false);

        if !result {
            // If the point is not in the border or in the doc comments, then
            // it must be in the definition.
            assert!(
                self.definition.rectangle.contains_point(c),
                "{c} isn't in function definition {} despite not being anywhere else",
                self.definition.rectangle,
            );
        }

        result
    }
}

impl<'a> Tokenize<'a> for FunctionBox<'a> {
    fn tokenize(window: CharacterMapWindow<'a>) -> Result<Token<'a>, Error<'a>> {
        enum State {
            Top,
            Signature {
                start: Coords,
                non_space_encountered: bool,
                last_space: bool,
            },
            TopSignature {
                signature: Rectangle,
            },
            Right {
                signature: Rectangle,
                comment_y: Option<usize>,
            },
            Bottom {
                signature: Rectangle,
                comment_y: Option<usize>,
                bottom_right: Coords,
            },
            Left {
                signature: Rectangle,
                comment_y: Option<usize>,
                bottom_right: Coords,
                comment_seen: bool,
            },
            Midline {
                signature: Rectangle,
                bottom_right: Coords,
                comment_y: usize,
            },
        }

        let mut state = State::Top;
        let mut cursor: Coords = (0, 0).into();

        assert_eq!(window[cursor], heavy::SE);

        loop {
            match &mut state {
                State::Top => {
                    cursor = cursor + ICoords::RIGHT;

                    if !window.contains_point(cursor) {
                        return Err(Error::new(
                            window.crop(
                                (cursor + ICoords::LEFT, cursor + ICoords::LEFT)
                                    .try_into()
                                    .unwrap(),
                            ),
                            ErrorKind::UnexpectedEof,
                            Some("while reading along the top of the function box".to_string()),
                        ));
                    }

                    match window[cursor] {
                        ' ' => {
                            state = State::Signature { start: cursor, non_space_encountered: false, last_space: true };
                        }
                        c if c == heavy::EW => {
                            continue;
                        }
                        c if c == heavy::SW => {
                            return Err(Error::new(
                                window.crop((cursor, cursor).try_into().unwrap()),
                                ErrorKind::UnexpectedChar {
                                    expected: "[' ']".to_string(),
                                },
                                Some("missing function signature".to_string())
                            ))
                        }
                        _ => {
                            return Err(Error::new(
                                window.crop((cursor, cursor).try_into().unwrap()),
                                ErrorKind::UnexpectedChar {
                                    expected: format!("['{}', ' ']", heavy::EW),
                                },
                                Some("expected either function signature or continuation of function box".to_string())
                            ))
                        }
                    }
                }
                State::Signature {
                    start,
                    non_space_encountered,
                    last_space,
                } => {
                    cursor = cursor + ICoords::RIGHT;

                    if !window.contains_point(cursor) {
                        return Err(Error::new(
                            window.crop(
                                (cursor + ICoords::LEFT, cursor + ICoords::LEFT)
                                    .try_into()
                                    .unwrap(),
                            ),
                            ErrorKind::UnexpectedEof,
                            Some(
                                "while reading along the signature of the function box".to_string(),
                            ),
                        ));
                    }

                    match window[cursor] {
                        ' ' => {
                            *last_space = true;
                        }
                        c if c == heavy::SW || c == heavy::EW => {
                            if *non_space_encountered {
                                if *last_space {
                                    if c == heavy::SW {
                                        state = State::Right {
                                            signature: (*start, cursor + ICoords::LEFT)
                                                .try_into()
                                                .unwrap(),
                                            comment_y: None,
                                        }
                                    } else {
                                        state = State::TopSignature {
                                            signature: (*start, cursor + ICoords::LEFT)
                                                .try_into()
                                                .unwrap(),
                                        }
                                    }
                                } else {
                                    return Err(Error::new(
                                        window.crop((cursor, cursor).try_into().unwrap()),
                                        ErrorKind::UnexpectedChar { expected: "[' ']".to_string() },
                                        Some("missing space separating signature and function box perimeter".to_string()),
                                    ));
                                }
                            } else {
                                return Err(Error::new(
                                    window.crop(((0, 0), cursor).try_into().unwrap()),
                                    ErrorKind::UnexpectedChar {
                                        expected: "[(any non box-drawing character)]".to_string(),
                                    },
                                    Some("function signature found empty".to_string()),
                                ));
                            }
                        }
                        c if is_box_drawing_character(c) => {
                            return Err(Error::new(
                                window.crop(((0, 0), cursor).try_into().unwrap()),
                                ErrorKind::UnexpectedChar {
                                    expected: "[(any non box-drawing character)]".to_string(),
                                },
                                Some(
                                    "box-drawing characters cannot appear in function signature"
                                        .to_string(),
                                ),
                            ));
                        }
                        _ => {
                            *non_space_encountered = true;
                            *last_space = false;
                        }
                    }
                }
                State::TopSignature { signature } => {
                    cursor = cursor + ICoords::RIGHT;

                    if !window.contains_point(cursor) {
                        return Err(Error::new(
                            window.crop(
                                (cursor + ICoords::LEFT, cursor + ICoords::LEFT)
                                    .try_into()
                                    .unwrap(),
                            ),
                            ErrorKind::UnexpectedEof,
                            Some("while reading along the top of the function box".to_string()),
                        ));
                    }

                    match window[cursor] {
                        c if c == heavy::EW => continue,
                        c if c == heavy::SW => {
                            state = State::Right {
                                signature: *signature,
                                comment_y: None,
                            }
                        }
                        _ => {
                            return Err(Error::new(
                                window.crop((cursor, cursor).try_into().unwrap()),
                                ErrorKind::UnexpectedChar {
                                    expected: format!("['{}', '{}']", heavy::EW, heavy::SW),
                                },
                                None,
                            ))
                        }
                    }
                }
                State::Right {
                    signature,
                    comment_y,
                } => {
                    cursor = cursor + ICoords::DOWN;

                    if !window.contains_point(cursor) {
                        return Err(Error::new(
                            window.crop(
                                (cursor + ICoords::UP, cursor + ICoords::UP)
                                    .try_into()
                                    .unwrap(),
                            ),
                            ErrorKind::UnexpectedEof,
                            Some("while reading along the right of the function box".to_string()),
                        ));
                    }

                    match window[cursor] {
                        c if c == heavy::NS => continue,
                        c if c == heavy::NW => {
                            state = State::Bottom {
                                signature: *signature,
                                comment_y: *comment_y,
                                bottom_right: cursor,
                            }
                        }
                        c if c == heavy::NSW => {
                            *comment_y = Some(cursor.y);
                        }
                        _ => {
                            return Err(Error::new(
                                window.crop((cursor, cursor).try_into().unwrap()),
                                ErrorKind::UnexpectedChar {
                                    expected: format!(
                                        "['{}', '{}', '{}']",
                                        heavy::NS,
                                        heavy::NW,
                                        heavy::NSW
                                    ),
                                },
                                None,
                            ))
                        }
                    }
                }
                State::Bottom {
                    signature,
                    comment_y,
                    bottom_right,
                } => {
                    if cursor.x == 0 {
                        return Err(Error::new(
                            window.crop((cursor, cursor).try_into().unwrap()),
                            ErrorKind::UnexpectedEof,
                            Some("while reading along the bottom of the function box".to_string()),
                        ));
                    }

                    cursor = cursor + ICoords::LEFT;

                    match window[cursor] {
                        c if c == heavy::EW => continue,
                        c if c == heavy::NE => {
                            state = State::Left {
                                signature: *signature,
                                comment_y: *comment_y,
                                bottom_right: *bottom_right,
                                comment_seen: false,
                            }
                        }
                        _ => {
                            return Err(Error::new(
                                window.crop((cursor, cursor).try_into().unwrap()),
                                ErrorKind::UnexpectedChar {
                                    expected: format!("['{}', '{}']", heavy::EW, heavy::NE,),
                                },
                                None,
                            ))
                        }
                    }
                }
                State::Left {
                    signature,
                    comment_y,
                    bottom_right,
                    comment_seen,
                } => {
                    if cursor.y == 0 {
                        return Err(Error::new(
                            window.crop((cursor, cursor).try_into().unwrap()),
                            ErrorKind::UnexpectedEof,
                            Some("while reading along the left of the function box".to_string()),
                        ));
                    }

                    cursor = cursor + ICoords::UP;

                    match window[cursor] {
                        c if c == heavy::NS => {
                            if let Some(y) = comment_y.as_ref() {
                                if *y == cursor.y {
                                    return Err(Error::new(
                                        window.crop((cursor, cursor).try_into().unwrap()),
                                        ErrorKind::UnexpectedChar {
                                            expected: format!(
                                                "['{}']",
                                                heavy::NSE,
                                            ),
                                        },
                                        Some("function documentation separator line misaligned with right side".to_string()),
                                    ));
                                }
                            }
                        }
                        c if c == heavy::SE => {
                            if comment_y.is_some() && !*comment_seen {
                                return Err(Error::new(
                                    window.crop((cursor, cursor).try_into().unwrap()),
                                    ErrorKind::UnexpectedChar {
                                        expected: format!(
                                            "['{}']",
                                            heavy::NSE,
                                        ),
                                    },
                                    Some(format!("missing function documentation separator line connector ('{}') on the left side", heavy::NSE)),
                                ));
                            } else if let Some(comment_y) = comment_y.as_ref().copied() {
                                cursor.y = comment_y;
                                cursor.x = 0;
                                state = State::Midline {
                                    signature: *signature,
                                    bottom_right: *bottom_right,
                                    comment_y,
                                }
                            } else {
                                break Ok(Token {
                                    span: window.crop(((0, 0), *bottom_right).try_into().unwrap()),
                                    kind: TokenKind::FunctionBox(Self {
                                        signature: window.crop(*signature),
                                        definition: window.crop(
                                            ((1, 1), *bottom_right + ICoords::LEFT + ICoords::UP)
                                                .try_into()
                                                .unwrap(),
                                        ),
                                        comment: None,
                                    }),
                                });
                            }
                        }
                        c if c == heavy::NSE => {
                            if let Some(comment_y) = comment_y.as_ref().copied() {
                                if cursor.y == comment_y {
                                    *comment_seen = true;
                                } else {
                                    return Err(Error::new(
                                        window.crop((cursor, cursor).try_into().unwrap()),
                                        ErrorKind::UnexpectedChar {
                                            expected: format!(
                                                "['{}', '{}']",
                                                heavy::NS, heavy::SE
                                            )
                                        },
                                        Some("function documentation separator line misaligned with right side".to_string()),
                                    ));
                                }
                            } else {
                                return Err(Error::new(
                                    window.crop((cursor, cursor).try_into().unwrap()),
                                    ErrorKind::UnexpectedChar {
                                        expected: format!(
                                            "['{}', '{}']",
                                            heavy::NS, heavy::SE
                                        ),
                                    },
                                    Some(format!("missing function documentation separator line connector ('{}') on the right side", heavy::NSW)),
                                ));
                            }
                        }
                        _ => {
                            return Err(Error::new(
                                window.crop((cursor, cursor).try_into().unwrap()),
                                ErrorKind::UnexpectedChar {
                                    expected: format!(
                                        "['{}', '{}', '{}']",
                                        heavy::NS,
                                        heavy::NSE,
                                        heavy::SE
                                    ),
                                },
                                None,
                            ))
                        }
                    }
                }
                State::Midline {
                    signature,
                    bottom_right,
                    comment_y,
                } => {
                    cursor = cursor + ICoords::RIGHT;

                    if !window.contains_point(cursor) {
                        return Err(Error::new(
                            window.crop(
                                (cursor + ICoords::LEFT, cursor + ICoords::LEFT)
                                    .try_into()
                                    .unwrap(),
                            ),
                            ErrorKind::UnexpectedEof,
                            Some("while reading along the midline of the function box".to_string()),
                        ));
                    }

                    match window[cursor] {
                        c if c == heavy::EW => continue,
                        c if c == heavy::NSW => {
                            break Ok(Token {
                                span: window.crop(((0, 0), *bottom_right).try_into().unwrap()),
                                kind: TokenKind::FunctionBox(FunctionBox {
                                    signature: window.crop(*signature),
                                    definition: window.crop(
                                        (
                                            (1, *comment_y + 1),
                                            *bottom_right + ICoords::LEFT + ICoords::UP,
                                        )
                                            .try_into()
                                            .unwrap(),
                                    ),
                                    comment: Some(
                                        window.crop(
                                            ((1, 1), (bottom_right.x - 1, *comment_y - 1))
                                                .try_into()
                                                .unwrap(),
                                        ),
                                    ),
                                }),
                            });
                        }
                        _ => {
                            return Err(Error::new(
                                window.crop((cursor, cursor).try_into().unwrap()),
                                ErrorKind::UnexpectedChar {
                                    expected: format!("['{}', '{}']", heavy::EW, heavy::NSW,),
                                },
                                None,
                            ))
                        }
                    }
                }
            }
        }
    }
}

impl fmt::Display for FunctionBox<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "#( [DEBUG] Function box: signature @ {} )#",
            self.signature.rectangle
        )?;
        writeln!(f, "{}", self.signature)?;
        writeln!(
            f,
            "#( [DEBUG] Function box: definition @ {} )#",
            self.definition.rectangle
        )?;
        writeln!(f, "{}", self.definition)?;

        if let Some(comment) = self.comment.as_ref() {
            writeln!(
                f,
                "#( [DEBUG] Function box: doc comment @ {} )#",
                comment.rectangle
            )?;
            writeln!(f, "{}", comment)?;
        }

        Ok(())
    }
}

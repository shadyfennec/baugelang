//! Path lexer definitions.

use std::fmt;

use crate::frontend::{
    coordinates::{Coords, ICoords},
    lexer::{CharacterMapWindow, Error, ErrorKind, Token, TokenKind, Tokenize, CONNECTORS},
};

/// The kind of path of a [`PathFragment`].
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum PathKind {
    /// A single path: `│`, `─`, `┌`, ...
    Single,
    /// A double path: `║`, `═`, `╔`, ...
    Double,
}

impl PathKind {
    /// Returns the type of path in the four [`Directions`](Direction) of a connector.
    pub fn from_connector(c: char) -> [Option<Self>; 4] {
        let north = match c {
            '└' | '┘' | '┼' | '├' | '┤' | '┴' | '╘' | '╛' | '╞' | '╡' | '╧' | '╪' => {
                Some(PathKind::Single)
            }
            '╚' | '╝' | '╬' | '╠' | '╣' | '╩' | '╙' | '╜' | '╟' | '╢' | '╨' | '╫' => {
                Some(PathKind::Double)
            }
            _ => None,
        };

        let east = match c {
            '┌' | '└' | '┼' | '├' | '┬' | '┴' | '╓' | '╙' | '╟' | '╥' | '╨' | '╫' => {
                Some(PathKind::Single)
            }
            '╔' | '╚' | '╬' | '╠' | '╦' | '╩' | '╒' | '╘' | '╞' | '╤' | '╧' | '╪' => {
                Some(PathKind::Double)
            }
            _ => None,
        };

        let south = match c {
            '┌' | '┐' | '┼' | '├' | '┤' | '┬' | '╒' | '╕' | '╞' | '╡' | '╤' | '╪' => {
                Some(PathKind::Single)
            }
            '╔' | '╗' | '╬' | '╠' | '╣' | '╦' | '╓' | '╖' | '╟' | '╢' | '╥' | '╫' => {
                Some(PathKind::Double)
            }
            _ => None,
        };

        let west = match c {
            '┐' | '┘' | '┼' | '┤' | '┬' | '┴' | '╖' | '╜' | '╢' | '╥' | '╨' | '╫' => {
                Some(PathKind::Single)
            }
            '╗' | '╝' | '╬' | '╣' | '╦' | '╩' | '╕' | '╛' | '╡' | '╤' | '╧' | '╪' => {
                Some(PathKind::Double)
            }
            _ => None,
        };

        [north, east, south, west]
    }
}

/// The kind of path fragment (or component).
pub enum PathFragment {
    /// A straight line, either horizontal or vertical:
    /// ```text
    /// ──────────────
    /// ```
    Line(PathKind),
    /// A connector between different [`PathKind`], or directions: `├`, `╞`, ...
    Connector {
        top: Option<PathKind>,
        right: Option<PathKind>,
        bottom: Option<PathKind>,
        left: Option<PathKind>,
    },
    /// An up arrow character: `↑`.
    ArrowUp,
    /// A right arrow character: `→`.
    ArrowRight,
    /// A down arrow character: `↓`.
    ArrowDown,
    /// A left arrow character: `←`.
    ArrowLeft,
    /// An up path start character: `╿`.
    StartUp,
    /// A right path start character: `╼`.
    StartRight,
    /// A down path start character: `╽`.
    StartDown,
    /// A left path start character: `╾`.
    StartLeft,
    /// A path error character: `╳`.
    Error,
}

impl<'a> Tokenize<'a> for PathFragment {
    fn tokenize(window: CharacterMapWindow<'a>) -> Result<Token<'a>, Error<'a>> {
        let fragment = match window[(0, 0)] {
            '↑' => PathFragment::ArrowUp,
            '→' => PathFragment::ArrowRight,
            '↓' => PathFragment::ArrowDown,
            '←' => PathFragment::ArrowLeft,
            '╿' => PathFragment::StartUp,
            '╼' => PathFragment::StartRight,
            '╽' => PathFragment::StartDown,
            '╾' => PathFragment::StartLeft,
            '╳' => PathFragment::Error,
            c @ ('│' | '─' | '║' | '═') => {
                let global_window = window.reset();
                let start = global_window.translate((0, 0), &window);

                let directions = match c {
                    '│' | '║' => [ICoords::UP, ICoords::DOWN],
                    '─' | '═' => [ICoords::LEFT, ICoords::RIGHT],
                    _ => unreachable!(),
                };

                let kind = match c {
                    '│' | '─' => PathKind::Single,
                    '║' | '═' => PathKind::Double,
                    _ => unreachable!(),
                };

                let (mut a, mut b) = (Ok::<Coords, Coords>(start), Ok::<Coords, Coords>(start));

                loop {
                    if let (Err(a), Err(b)) = (a, b) {
                        return Ok(Token {
                            span: global_window.crop((a, b).try_into().unwrap()),
                            kind: TokenKind::PathFragment(PathFragment::Line(kind)),
                        });
                    }

                    if let Ok(a_coords) = a.as_mut() {
                        let new_a_coords = *a_coords + directions[0];
                        if !global_window.contains_point(new_a_coords)
                            || global_window[new_a_coords] != c
                        {
                            a = Err(*a_coords);
                        } else {
                            *a_coords = new_a_coords
                        }
                    }

                    if let Ok(b_coords) = b.as_mut() {
                        let new_b_coords = *b_coords + directions[1];
                        if !global_window.contains_point(new_b_coords)
                            || global_window[new_b_coords] != c
                        {
                            b = Err(*b_coords);
                        } else {
                            *b_coords = new_b_coords
                        }
                    }
                }
            }
            c if CONNECTORS.contains(&c) => {
                let [top, right, bottom, left] = PathKind::from_connector(c);
                PathFragment::Connector {
                    top,
                    right,
                    bottom,
                    left,
                }
            }
            _ => {
                return Err(Error::new(
                    window.crop(((0, 0), (0, 0)).try_into().unwrap()),
                    ErrorKind::NotFound,
                    None,
                ))
            }
        };

        Ok(Token {
            span: window.crop(((0, 0), (0, 0)).try_into().unwrap()),
            kind: TokenKind::PathFragment(fragment),
        })
    }
}

impl fmt::Display for PathFragment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PathFragment::Line(path_kind) => {
                write!(f, "#( [DEBUG] Path line (kind = {path_kind:?}) )#")
            }
            PathFragment::Connector {
                top,
                right,
                bottom,
                left,
            } => {
                let top = top
                    .map(|k| format!("{k:?}"))
                    .unwrap_or_else(|| "_".to_string());
                let right = right
                    .map(|k| format!("{k:?}"))
                    .unwrap_or_else(|| "_".to_string());
                let bottom = bottom
                    .map(|k| format!("{k:?}"))
                    .unwrap_or_else(|| "_".to_string());
                let left = left
                    .map(|k| format!("{k:?}"))
                    .unwrap_or_else(|| "_".to_string());

                write!(
                    f,
                    "#( [DEBUG] Path connector (N: {top} E: {right} S: {bottom} W: {left}) )#"
                )
            }
            PathFragment::ArrowUp => write!(f, "#( [DEBUG] Path arrow up )#"),
            PathFragment::ArrowRight => write!(f, "#( [DEBUG] Path arrow right )#"),
            PathFragment::ArrowDown => write!(f, "#( [DEBUG] Path arrow down )#"),
            PathFragment::ArrowLeft => write!(f, "#( [DEBUG] Path arrow left )#"),
            PathFragment::StartUp => write!(f, "#( [DEBUG] Path start up )#"),
            PathFragment::StartRight => write!(f, "#( [DEBUG] Path start right )#"),
            PathFragment::StartDown => write!(f, "#( [DEBUG] Path start down )#"),
            PathFragment::StartLeft => write!(f, "#( [DEBUG] Path start left )#"),
            PathFragment::Error => write!(f, "#( [DEBUG] Path error end )#"),
        }
    }
}

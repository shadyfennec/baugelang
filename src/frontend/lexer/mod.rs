//! Lexer for Bauge.

use std::{backtrace::Backtrace, error, fmt, ops::Index};

use crate::frontend::coordinates::{self, Coords, ICoords, Rectangle};
use crate::log;
use crate::logging;

pub mod function_box;
use function_box::FunctionBox;

pub mod keyword;
use keyword::Keyword;

pub mod path;
use path::PathFragment;

use super::is_box_drawing_character;

macro_rules! path_mods {
    ($name:ident, [$ns:literal, $ew:literal, $se:literal, $sw:literal, $ne:literal, $nw:literal, $nsw:literal, $nse:literal, $ewn:literal, $ews:literal, $nsew:literal]) => {
        #[allow(dead_code)]
        mod $name {
            pub const NS: char = $ns;
            pub const EW: char = $ew;
            pub const SE: char = $se;
            pub const SW: char = $sw;
            pub const NE: char = $ne;
            pub const NW: char = $nw;
            pub const NSW: char = $nsw;
            pub const NSE: char = $nse;
            pub const EWN: char = $ewn;
            pub const EWS: char = $ews;
            pub const NSEW: char = $nsew;

            pub const fn all() -> [char; 11] {
                [$ns, $ew, $se, $sw, $ne, $nw, $nsw, $nse, $ewn, $ews, $nsew]
            }
        }
    };
}

path_mods!(
    single,
    ['│', '─', '┌', '┐', '└', '┘', '┤', '├', '┴', '┬', '┼']
);
path_mods!(
    double,
    ['║', '═', '╔', '╗', '╚', '╝', '╣', '╠', '╩', '╦', '╬']
);
path_mods!(
    heavy,
    ['┃', '━', '┏', '┓', '┗', '┛', '┫', '┣', '┻', '┳', '╋']
);

macro_rules! tokenize_log {
    ($t:literal, $c:ident, $v:ident, $e:expr_2021) => {
        log!("lexer", h3, "{} @ {}", $t, $c);

        let (maybe_token, t) = logging::time(|| $e);

        let token = match maybe_token {
            Ok(token) => token,
            Err(e) => {
                log!("lexer", p, "Failed after {}", logging::format_time(&t));

                log!("lexer", err, "{e}");

                log!("lexer", p, "Occured here:");
                log!("lexer", code, "{}", e.span.to_string());

                logging::finish("lexer").unwrap();
                return Err(e);
            }
        };

        log!("lexer", p, "Finished after {}:", logging::format_time(&t));
        log!("lexer", code, "{}", token.to_string());

        $v.push(token);
    };
}

#[rustfmt::skip]
const CONNECTORS: [char; 36] = [
    // single
    single::SE, single::SW, single::NE, single::NW, single::NSW, single::NSE, single::EWN, single::EWS, single::NSEW,
    // double
    double::SE, double::SW, double::NE, double::NW, double::NSW, double::NSE, double::EWN, double::EWS, double::NSEW,
    // mixed
    '╒', '╓', '╕', '╖', '╘', '╙', '╛', '╜', '╞', '╟', '╡', '╢', '╤', '╥', '╧', '╨', '╪', '╫',
];

/// An error that can arise during lexing.
#[derive(Debug)]
pub struct Error<'a> {
    pub span: CharacterMapWindow<'a>,
    pub kind: Box<ErrorKind<'a>>,
    pub note: Option<String>,
    pub backtrace: Backtrace,
}

impl<'a> Error<'a> {
    pub fn new(span: CharacterMapWindow<'a>, kind: ErrorKind<'a>, note: Option<String>) -> Self {
        Self {
            span,
            kind: Box::new(kind),
            note,
            backtrace: Backtrace::capture(),
        }
    }
}

impl fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Lexing error: in this area ({}): ", self.span.rectangle)?;
        write!(f, "{}", self.kind)?;
        if let Some(s) = self.note.as_deref() {
            write!(f, "\nNote: {s}")?;
        }

        Ok(())
    }
}

impl error::Error for Error<'_> {}

/// The kind of lexing [`Error`].
#[derive(Debug)]
pub enum ErrorKind<'a> {
    /// An unexpected character was encountered during lexing.
    UnexpectedChar {
        expected: String,
    },
    /// An error during coordinates handling occured
    CoordError(coordinates::Error),
    /// The edge of the map was encountered while expecting more characters.
    UnexpectedEof,
    /// The lexer could not find the corresponding pattern
    NotFound,
    _Dummy(&'a ()),
}

impl fmt::Display for ErrorKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorKind::UnexpectedChar { expected } => {
                writeln!(f, "Unexpected character.")?;
                write!(f, "Note: expected any of: {expected}")?;
            }
            ErrorKind::CoordError(error) => {
                writeln!(f, "Coordinates error: {error}")?;
                write!(f, "Warning: this is a bug in the compiler.")?;
            }
            ErrorKind::UnexpectedEof => {
                writeln!(f, "Unexpectedly ran into the edge of the program.")?;
            }
            ErrorKind::NotFound => {
                writeln!(f, "Couldn't find the token while reading")?;
            }
            ErrorKind::_Dummy(_) => todo!(),
        }
        Ok(())
    }
}

/// Trait indicating that a structure can be potentially created from a map window.
trait Tokenize<'a> {
    /// Tokenize starting at the origin of the passed [`CharacterMapWindow`].
    fn tokenize(window: CharacterMapWindow<'a>) -> Result<Token<'a>, Error<'a>>;
}

/// A lexer "token", unparsed but structured.
///
/// It contains a span, as well as a specialized [`TokenKind`] to refine its
/// definition.
pub struct Token<'a> {
    pub span: CharacterMapWindow<'a>,
    pub kind: TokenKind<'a>,
}

impl Token<'_> {
    /// Returns `true` if the token contains the given point. Used to determine,
    /// for example, if a circular token with a hole in its middle needs to
    /// have its middle lexxed to.
    pub fn contains_point<C>(&self, c: C) -> bool
    where
        C: Into<Coords>,
    {
        let c = c.into();

        // No point (hehe) in checking the finer details if not inside the
        // global area already
        self.span.rectangle.contains_point(c) && self.kind.contains_point(c, self.span.rectangle)
    }
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "#( [DEBUG] Token start, span @ {} )#",
            self.span.rectangle
        )?;
        writeln!(f, "{}", self.span)?;
        writeln!(f, "{}", self.kind)?;
        writeln!(f, "#( [DEBUG] Token end )#")?;

        Ok(())
    }
}

/// The different kinds of [`Token`].
pub enum TokenKind<'a> {
    /// A [`FunctionBox`].
    FunctionBox(FunctionBox<'a>),
    /// An integer literal, such as `123`, `0xABCD`, or `0b011001`.
    IntegerLiteral,
    /// A string literal, such as `"Hello, world!"`.
    StringLiteral,
    /// A keyword of the Bauge language, such as `if`, `_` or `foreach`.
    Keyword(Keyword),
    /// A fragment of a Bauge path.
    PathFragment(PathFragment),
    /// An identifier for a function.
    Identifier,
}

impl TokenKind<'_> {
    /// Returns `true` if the token contains the given point. Used to determine,
    /// for example, if a circular token with a hole in its middle needs to
    /// have its middle lexxed to.
    pub fn contains_point<C>(&self, c: C, initial_span: Rectangle) -> bool
    where
        C: Into<Coords>,
    {
        match self {
            TokenKind::FunctionBox(function_box) => function_box.contains_point(c, initial_span),
            TokenKind::IntegerLiteral
            | TokenKind::StringLiteral
            | TokenKind::Keyword(_)
            | TokenKind::PathFragment(_)
            | TokenKind::Identifier => initial_span.contains_point(c),
        }
    }
}

impl fmt::Display for TokenKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::FunctionBox(function_box) => write!(f, "{function_box}"),
            TokenKind::IntegerLiteral => {
                write!(f, "#( [DEBUG] Integer literal )#")
            }
            TokenKind::StringLiteral => {
                write!(f, "#( [DEBUG] String literal )#")
            }
            TokenKind::Keyword(k) => {
                write!(f, "#( [DEBUG] Keyword {k:?} )#")
            }
            TokenKind::PathFragment(path_fragment) => {
                write!(f, "{path_fragment}")
            }
            TokenKind::Identifier => {
                write!(f, "#( [DEBUG] Identifier )#")
            }
        }
    }
}

/// Attempts to recognize a string literal at the top-left of the passed window.
fn tokenize_string_literal(window: CharacterMapWindow<'_>) -> Result<Token<'_>, Error> {
    let mut cursor = Coords::from((1, 0));
    loop {
        if !window.contains_point(cursor) {
            return Err(Error::new(
                window.crop(((0, 0), cursor + ICoords::LEFT).try_into().unwrap()),
                ErrorKind::UnexpectedEof,
                Some("while parsing string literal".to_string()),
            ));
        } else if window[cursor] == '"' {
            return Ok(Token {
                span: window.crop(((0, 0), cursor).try_into().unwrap()),
                kind: TokenKind::StringLiteral,
            });
        }
        cursor = cursor + ICoords::RIGHT;
    }
}

/// Attempts to recognize an integer literal at the top-left of the passed window.
fn tokenize_integer_literal(window: CharacterMapWindow<'_>) -> Result<Token<'_>, Error> {
    fn is_integer_literal_char(c: char) -> bool {
        matches!(c, '0'..='9' | 'x' | 'X' | 'b' | 'B' | 'o' | 'O' | '_')
    }

    let mut cursor = Coords::from((1, 0));
    loop {
        if !window.contains_point(cursor) {
            return Err(Error::new(
                window.crop(((0, 0), cursor + ICoords::LEFT).try_into().unwrap()),
                ErrorKind::UnexpectedEof,
                Some("while parsing integer literal".to_string()),
            ));
        } else if !is_integer_literal_char(window[cursor]) {
            return Ok(Token {
                span: window.crop(((0, 0), cursor + ICoords::LEFT).try_into().unwrap()),
                kind: TokenKind::IntegerLiteral,
            });
        }
        cursor = cursor + ICoords::RIGHT;
    }
}

/// Attempts to recognize an identifier at the top-left of the passed window.
fn tokenize_ident(window: CharacterMapWindow<'_>) -> Result<Token<'_>, Error> {
    let mut cursor = Coords::from((0, 0));
    while window[cursor] != ' ' {
        cursor = cursor + ICoords::RIGHT;
        if !window.contains_point(cursor) {
            return Err(Error::new(
                window.crop(((0, 0), cursor + ICoords::LEFT).try_into().unwrap()),
                ErrorKind::UnexpectedEof,
                Some("while parsing identifier".to_string()),
            ));
        }

        if is_box_drawing_character(window[cursor]) {
            return Err(Error::new(
                window.crop(((0, 0), cursor).try_into().unwrap()),
                ErrorKind::UnexpectedChar {
                    expected: "any valid identifier character".to_string(),
                },
                Some("Block-drawing characters are not allowed in identifiers".to_string()),
            ));
        }
    }

    Ok(Token {
        span: window.crop(((0, 0), cursor + ICoords::LEFT).try_into().unwrap()),
        kind: TokenKind::Identifier,
    })
}

/// A two-dimensional collection of characters, used to better reason with
/// two-dimensional structures.
///
/// As programs can have unequal line lengths, lines are padded with spaces to
/// the length of the longest lines. Lengths (such as width and height) are
/// measured in distinct characters.
#[derive(Debug)]
pub struct CharacterMap {
    lines: Vec<Vec<char>>,
}

impl CharacterMap {
    /// Returns the length of the map, in characters. It corresponds to the
    /// longest line of the original program.
    pub fn width(&self) -> usize {
        self.lines.first().map(|l| l.len()).unwrap_or_default()
    }

    /// Returns the height of the map, in characters. It corresponds to the
    /// number of lines in the original program.
    pub fn height(&self) -> usize {
        self.lines.len()
    }

    /// Creates a [window](CharacterMapWindow) of the [`CharacterMap`], restricting the view to a
    /// rectangle.
    pub fn window(&self, rectangle: Rectangle) -> CharacterMapWindow {
        CharacterMapWindow {
            original: self,
            rectangle,
        }
    }

    /// Creates a [window](CharacterMapWindow) of the [`CharacterMap`], restricting the view to a
    /// rectangle whose top-left corner is the point passed as a parameter, and
    /// bottom-right is the bottom-right of the map.
    pub fn window_from_point<C>(&self, c: C) -> CharacterMapWindow
    where
        C: Into<Coords>,
    {
        let c = c.into();
        self.window(
            (
                c,
                (
                    self.width().saturating_sub(1),
                    self.height().saturating_sub(1),
                ),
            )
                .try_into()
                .unwrap(),
        )
    }

    /// Returns a [`Rectangle`] encompassing the entirety of the map.
    pub fn rectangle(&self) -> Rectangle {
        (
            (0, 0),
            (
                self.width().saturating_sub(1),
                self.height().saturating_sub(1),
            ),
        )
            .try_into()
            .unwrap()
    }

    /// Reads and tokenizes the map into a list of [`Tokens`](Token).
    pub fn tokenize(&self) -> Result<Vec<Token>, Error> {
        let mut tokens: Vec<Token> = Vec::new();

        log!("lexer", h2, "Tokenization");

        let (_, timing) = logging::time(|| {
            for line in self.rectangle().iter() {
                for coord in line {
                    if tokens.iter().any(|t| t.contains_point(coord)) {
                        continue;
                    }

                    match self[coord] {
                        ' ' => continue,
                        '"' => {
                            tokenize_log!(
                                "String literal",
                                coord,
                                tokens,
                                tokenize_string_literal(self.window_from_point(coord))
                            );
                        }
                        c if c == heavy::SE => {
                            tokenize_log!(
                                "Function box",
                                coord,
                                tokens,
                                FunctionBox::tokenize(self.window_from_point(coord))
                            );
                        }
                        c if c.is_ascii_digit() => {
                            tokenize_log!(
                                "Integer literal",
                                coord,
                                tokens,
                                tokenize_integer_literal(self.window_from_point(coord))
                            );
                        }
                        c => {
                            let (maybe_keyword, t) =
                                logging::time(|| Keyword::tokenize(self.window_from_point(coord)));
                            match maybe_keyword {
                                Ok(token) => {
                                    log!("lexer", h3, "Keyword @ {coord}");
                                    log!("lexer", p, "Finished in {}:", logging::format_time(&t));
                                    log!("lexer", code, "{}", token.to_string());

                                    tokens.push(token);
                                }
                                _ => {
                                    if is_box_drawing_character(c) {
                                        tokenize_log!(
                                            "Path fragment",
                                            coord,
                                            tokens,
                                            PathFragment::tokenize(self.window_from_point(coord))
                                        );
                                    } else {
                                        tokenize_log!(
                                            "Identifier",
                                            coord,
                                            tokens,
                                            tokenize_ident(self.window_from_point(coord))
                                        );
                                    }
                                }
                            }
                        }
                    }
                }
            }

            Ok(())
        });

        log!("lexer", h2, "Summary");
        log!(
            "lexer",
            p,
            "Lexer recognized {} tokens in {}.",
            tokens.len(),
            logging::format_time(&timing)
        );

        log!("lexer", finish);

        Ok(tokens)
    }
}

impl fmt::Display for CharacterMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, l) in self.lines.iter().enumerate() {
            for c in l {
                write!(f, "{c}")?;
            }
            if i != self.height() - 1 {
                writeln!(f)?;
            }
        }

        Ok(())
    }
}

impl<S> FromIterator<S> for CharacterMap
where
    S: AsRef<str>,
{
    fn from_iter<T: IntoIterator<Item = S>>(iter: T) -> Self {
        let (lines, t) = logging::time(|| {
            let mut max_len = 0;

            let mut lines = iter
                .into_iter()
                .map(|s| {
                    let s = s.as_ref();
                    let v = s.chars().collect::<Vec<_>>();
                    max_len = max_len.max(v.len());
                    v
                })
                .collect::<Vec<_>>();

            for line in lines.iter_mut() {
                if line.len() < max_len {
                    line.extend(std::iter::repeat(' ').take(max_len - line.len()));
                }
            }

            lines
        });

        let map = Self { lines };

        log!("lexer", h1, "Lexer");
        log!("lexer", h2, "`CharacterMap` creation");
        log!(
            "lexer",
            p,
            "`CharacterMap` of dimension {}x{} created in {}:",
            map.width(),
            map.height(),
            logging::format_time(&t)
        );
        log!("lexer", code, "{}", map.to_string());

        map
    }
}

impl<C> Index<C> for CharacterMap
where
    C: Into<Coords>,
{
    type Output = char;

    fn index(&self, index: C) -> &Self::Output {
        let Coords { x, y } = index.into();

        &self.lines[y][x]
    }
}

/// A slice, or view, into a [`CharacterMap`].
///
/// Indexing into that slice uses *local* coordinates (that is, `(0, 0)` is the
/// top-left corner of the slice, and not the original map).
#[derive(Debug)]
pub struct CharacterMapWindow<'a> {
    original: &'a CharacterMap,
    rectangle: Rectangle, // Relative to the original
}

impl CharacterMapWindow<'_> {
    /// Returns the width of the window, in characters.
    pub fn width(&self) -> usize {
        self.rectangle.width()
    }

    /// Returns the height of the window, in characters.
    pub fn height(&self) -> usize {
        self.rectangle.height()
    }

    /// Crops the window and gives a smaller one. The given rectangle is local
    /// to the window's rectangle (and not to the original).
    pub fn crop(&self, r: Rectangle) -> Self {
        assert!(
            self.rectangle.normalize().contains_rectangle(r),
            "{} is not contained within {} (normalized from {})",
            r,
            self.rectangle.normalize(),
            self.rectangle
        );

        Self {
            original: self.original,
            rectangle: r + self.rectangle.top_left,
        }
    }

    /// Returns true if the passed window-local [`Coords`] is contained within
    /// this window.
    pub fn contains_point<C>(&self, c: C) -> bool
    where
        C: Into<Coords>,
    {
        self.rectangle.normalize().contains_point(c)
    }

    /// Attempts to recognize a string passed as a parameter by reading from
    /// local (0, 0) to the left. If it's found, then a new window encompassing
    /// the tag is returned.
    pub fn recognize_tag(&self, tag: &str) -> Option<Self> {
        tag.chars()
            .try_fold(Coords::from((0, 0)), |cursor, c| {
                if self.contains_point(cursor) {
                    if self[cursor] == c {
                        Some(cursor + ICoords::RIGHT)
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .map(|end| self.crop(((0, 0), end + ICoords::LEFT).try_into().unwrap()))
    }

    /// Creates a window on the entirety of the original map, effectively "resetting" it.
    pub fn reset(&self) -> Self {
        self.original.window(self.original.rectangle())
    }

    /// Translates coordinates local to `from` into coordinates local to this window.
    pub fn translate<C>(&self, c: C, from: &Self) -> Coords
    where
        C: Into<Coords>,
    {
        let c = c.into();
        c + from.rectangle.top_left - self.rectangle.top_left
    }
}

impl<C> Index<C> for CharacterMapWindow<'_>
where
    C: Into<Coords>,
{
    type Output = char;

    fn index(&self, index: C) -> &Self::Output {
        let index = index.into();
        assert!(
            self.rectangle.normalize().contains_point(index),
            "Indexing window {} out of bounds at {}",
            self.rectangle,
            index
        );
        &self.original[index + self.rectangle.top_left]
    }
}

impl fmt::Display for CharacterMapWindow<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, l) in self.rectangle.normalize().iter().enumerate() {
            for c in l {
                write!(f, "{}", self[c])?;
            }
            if i != self.height() - 1 {
                writeln!(f)?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_window() {
        let c: CharacterMap = "abcd\nefgh\nijkl\nnmop".lines().collect();

        let w = c.window(((1, 1), (2, 2)).try_into().unwrap());

        assert_eq!(w[(0, 0)], 'f');
    }
}

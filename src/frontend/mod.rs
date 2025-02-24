//! Frontend modules for Bauge.

pub mod coordinates;
pub mod lexer;

/// Returns `true` if the passed character is a box-drawing character, including the 4 directional arrows.
pub fn is_box_drawing_character(c: char) -> bool {
    ('\u{2500}'..='\u{257F}').contains(&c) || ('\u{2190}'..='\u{2193}').contains(&c)
}

//! Coordinates structures and helper functions.
//!
//! The main structure, [`Coords`], is used to index in two-dimensional
//! structures, the same manner as [`isize`] in one-dimensional structures.

use std::{error, fmt, ops};

/// Errors possible when using [`Coords`] and [`ICoords`].
#[derive(Debug)]
pub enum Error {
    /// The coordinate used as an index in a two-dimensional structure was out
    /// of bounds.
    OutOfBounds {
        coords: Coords,
        rectangle: Rectangle,
    },
    /// Attempted to create a rectangle with an invalid shape, inverted corners,
    /// or otherwise impossible geometry.
    InvalidRectangle {
        top_left: Coords,
        bottom_right: Coords,
    },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::OutOfBounds { coords, rectangle } => {
                write!(f, "Index {coords} out of bounds for rectangle {rectangle}")
            }
            Error::InvalidRectangle {
                top_left,
                bottom_right,
            } => {
                write!(f, "Cannot construct a rectangle with a top-left of {top_left} and a bottom-right of {bottom_right}")
            }
        }
    }
}

impl error::Error for Error {}

/// A two-dimensional coordinate used to index into two-dimensional structures.
/// By convention, coordinates `(0, 0)` correspond to the top-left corner.
///
/// # Examples
/// You can create [`Coords`] from their [`From`] implementations:
///
/// ```rust
/// # use bauge::frontend::coordinates::Coords;
///
/// let c1: Coords = (123, 456).into();
/// let c2: Coords = [123, 456].into();
/// ```
///
/// Basic operations are supported (with signed operations using [`ICoords`]):
/// ```rust
/// # use bauge::frontend::coordinates::Coords;
///
/// let c1: Coords = (123, 456).into();
/// let c2: Coords = (111, 111).into();
///
/// assert_eq!(c1 + c2, (234, 567).into());
/// ```
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Coords {
    pub x: usize,
    pub y: usize,
}

impl fmt::Display for Coords {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

impl ops::Add for Coords {
    type Output = Self;

    fn add(self, Coords { x: x2, y: y2 }: Self) -> Self::Output {
        let Coords { x: x1, y: y1 } = self;

        Self {
            x: x1 + x2,
            y: y1 + y2,
        }
    }
}

impl ops::Add<ICoords> for Coords {
    type Output = Coords;

    fn add(self, ICoords { x: dx, y: dy }: ICoords) -> Self::Output {
        let Coords { x, y } = self;

        Self {
            x: x.checked_add_signed(dx).unwrap(),
            y: y.checked_add_signed(dy).unwrap(),
        }
    }
}

impl ops::AddAssign for Coords {
    fn add_assign(&mut self, Coords { x, y }: Self) {
        self.x += x;
        self.y += y;
    }
}

impl ops::Mul for Coords {
    type Output = Self;

    fn mul(self, Coords { x: x2, y: y2 }: Self) -> Self::Output {
        let Coords { x: x1, y: y1 } = self;

        Self {
            x: x1 * x2,
            y: y1 * y2,
        }
    }
}

impl ops::MulAssign for Coords {
    fn mul_assign(&mut self, Coords { x, y }: Self) {
        self.x *= x;
        self.y *= y;
    }
}

impl ops::Sub for Coords {
    type Output = Self;

    fn sub(self, Coords { x: x2, y: y2 }: Self) -> Self::Output {
        let Coords { x: x1, y: y1 } = self;

        Self {
            x: x1 - x2,
            y: y1 - y2,
        }
    }
}

impl ops::SubAssign for Coords {
    fn sub_assign(&mut self, Coords { x, y }: Self) {
        self.x -= x;
        self.y -= y;
    }
}

impl ops::Div for Coords {
    type Output = Self;

    fn div(self, Coords { x: x2, y: y2 }: Self) -> Self::Output {
        let Coords { x: x1, y: y1 } = self;

        Self {
            x: x1 / x2,
            y: y1 / y2,
        }
    }
}

impl ops::DivAssign for Coords {
    fn div_assign(&mut self, Coords { x, y }: Self) {
        self.x /= x;
        self.y /= y;
    }
}

impl From<(usize, usize)> for Coords {
    fn from((x, y): (usize, usize)) -> Self {
        Self { x, y }
    }
}

impl From<Coords> for (usize, usize) {
    fn from(Coords { x, y }: Coords) -> Self {
        (x, y)
    }
}

impl From<[usize; 2]> for Coords {
    fn from([x, y]: [usize; 2]) -> Self {
        Self { x, y }
    }
}

impl From<Coords> for [usize; 2] {
    fn from(Coords { x, y }: Coords) -> Self {
        [x, y]
    }
}

/// A signed two-dimensional coordinate, used to perform arithmetic with
/// [`Coords`].
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct ICoords {
    pub x: isize,
    pub y: isize,
}

impl fmt::Display for ICoords {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({:+}, {:+})", self.x, self.y)
    }
}

impl ICoords {
    /// The "up" direction vector: `(0, -1)`.
    pub const UP: Self = Self { x: 0, y: -1 };
    /// The "down" direction vector: `(0, 1)`.
    pub const DOWN: Self = Self { x: 0, y: 1 };
    /// The "left" direction vector: `(-1, 0)`.
    pub const LEFT: Self = Self { x: -1, y: 0 };
    /// The "right" direction vector: `(1, 0)`.
    pub const RIGHT: Self = Self { x: 1, y: 0 };
}

impl ops::Add for ICoords {
    type Output = Self;

    fn add(self, ICoords { x: x2, y: y2 }: Self) -> Self::Output {
        let ICoords { x: x1, y: y1 } = self;

        Self {
            x: x1 + x2,
            y: y1 + y2,
        }
    }
}

impl ops::AddAssign for ICoords {
    fn add_assign(&mut self, ICoords { x, y }: Self) {
        self.x += x;
        self.y += y;
    }
}

impl ops::Mul for ICoords {
    type Output = Self;

    fn mul(self, ICoords { x: x2, y: y2 }: Self) -> Self::Output {
        let ICoords { x: x1, y: y1 } = self;

        Self {
            x: x1 * x2,
            y: y1 * y2,
        }
    }
}

impl ops::MulAssign for ICoords {
    fn mul_assign(&mut self, ICoords { x, y }: Self) {
        self.x *= x;
        self.y *= y;
    }
}

impl ops::Sub for ICoords {
    type Output = Self;

    fn sub(self, ICoords { x: x2, y: y2 }: Self) -> Self::Output {
        let ICoords { x: x1, y: y1 } = self;

        Self {
            x: x1 - x2,
            y: y1 - y2,
        }
    }
}

impl ops::SubAssign for ICoords {
    fn sub_assign(&mut self, ICoords { x, y }: Self) {
        self.x -= x;
        self.y -= y;
    }
}

impl ops::Div for ICoords {
    type Output = Self;

    fn div(self, ICoords { x: x2, y: y2 }: Self) -> Self::Output {
        let ICoords { x: x1, y: y1 } = self;

        Self {
            x: x1 / x2,
            y: y1 / y2,
        }
    }
}

impl ops::DivAssign for ICoords {
    fn div_assign(&mut self, ICoords { x, y }: Self) {
        self.x /= x;
        self.y /= y;
    }
}

impl From<(isize, isize)> for ICoords {
    fn from((x, y): (isize, isize)) -> Self {
        Self { x, y }
    }
}

impl From<ICoords> for (isize, isize) {
    fn from(ICoords { x, y }: ICoords) -> Self {
        (x, y)
    }
}

impl From<[isize; 2]> for ICoords {
    fn from([x, y]: [isize; 2]) -> Self {
        Self { x, y }
    }
}

impl From<ICoords> for [isize; 2] {
    fn from(ICoords { x, y }: ICoords) -> Self {
        [x, y]
    }
}

/// A two-dimensional rectangle, defined by a top-left and a bottom-right
/// [`Coords`].
///
/// The coordinates of the rectangle are **inclusive**: the coordinates equal to
/// its bottom-right are included in the rectangle.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Rectangle {
    pub top_left: Coords,
    pub bottom_right: Coords,
}

impl Rectangle {
    /// Constructs a rectangle from any corners, re-defining a top-left and a
    /// bottom-right corner if the two points passed are top-right and bottom-left.
    pub fn from_any_corners<C1, C2>(c1: C1, c2: C2) -> Self
    where
        C1: Into<Coords>,
        C2: Into<Coords>,
    {
        let c1 = c1.into();
        let c2 = c2.into();

        let top_left = (c1.x.min(c2.x), c1.y.min(c2.y)).into();
        let bottom_right = (c1.x.max(c2.x), c1.y.max(c2.y)).into();

        Self {
            top_left,
            bottom_right,
        }
    }

    /// Returns `true` if the rectangle contains the passed [`Coords`].
    pub fn contains_point<C>(&self, c: C) -> bool
    where
        C: Into<Coords>,
    {
        let c = c.into();
        (self.top_left.x..=self.bottom_right.x).contains(&c.x)
            && (self.top_left.y..=self.bottom_right.y).contains(&c.y)
    }

    /// Returns `true` if the rectangle contains the passed rectangle.
    pub fn contains_rectangle<R>(&self, other: R) -> bool
    where
        R: Into<Rectangle>,
    {
        let other: Rectangle = other.into();

        self.contains_point(other.top_left) && self.contains_point(other.bottom_right)
    }

    /// Returns the width of the rectangle.
    pub fn width(&self) -> usize {
        self.bottom_right.x - self.top_left.x + 1
    }

    /// Returns the height of the rectangle.
    pub fn height(&self) -> usize {
        self.bottom_right.y - self.top_left.y + 1
    }

    /// Returns a normalized rectangle, where the top-left corner is shifted to
    /// (0, 0).
    pub fn normalize(&self) -> Self {
        Self {
            top_left: (0, 0).into(),
            bottom_right: self.bottom_right - self.top_left,
        }
    }

    /// Returns an iterator over the [`Coords`] contained in this rectangle,
    /// going from top to bottom, left to right.
    pub fn iter(&self) -> impl Iterator<Item = impl Iterator<Item = Coords> + '_> + '_ {
        (self.top_left.y..=self.bottom_right.y)
            .map(|y| (self.top_left.x..=self.bottom_right.x).map(move |x| (x, y).into()))
    }

    /// Returns rectangles with width of 1 for the left and right side of the
    /// rectangle, and with height of 1 for the top and bottom sides of the
    /// rectangle.
    ///
    /// The borders are returned in this order: top, bottom, left, right.
    pub fn borders(&self) -> [Rectangle; 4] {
        [
            (self.top_left, (self.bottom_right.x, self.top_left.y))
                .try_into()
                .unwrap(),
            ((self.top_left.x, self.bottom_right.y), self.bottom_right)
                .try_into()
                .unwrap(),
            (self.top_left, (self.top_left.x, self.bottom_right.y))
                .try_into()
                .unwrap(),
            ((self.bottom_right.x, self.top_left.y), self.bottom_right)
                .try_into()
                .unwrap(),
        ]
    }
}

impl ops::Add<Coords> for Rectangle {
    type Output = Rectangle;

    fn add(self, rhs: Coords) -> Self::Output {
        Rectangle {
            top_left: self.top_left + rhs,
            bottom_right: self.bottom_right + rhs,
        }
    }
}

impl ops::Add<ICoords> for Rectangle {
    type Output = Rectangle;

    fn add(self, rhs: ICoords) -> Self::Output {
        Rectangle {
            top_left: self.top_left + rhs,
            bottom_right: self.bottom_right + rhs,
        }
    }
}

impl<TL, BR> TryFrom<(TL, BR)> for Rectangle
where
    TL: Into<Coords>,
    BR: Into<Coords>,
{
    type Error = Error;

    // Checks that the two passed coords are indeed top-left and bottom-right
    fn try_from((top_left, bottom_right): (TL, BR)) -> Result<Self, Self::Error> {
        let top_left = top_left.into();
        let bottom_right = bottom_right.into();

        if top_left.x <= bottom_right.x && top_left.y <= bottom_right.y {
            Ok(Self {
                top_left,
                bottom_right,
            })
        } else {
            Err(Error::InvalidRectangle {
                top_left,
                bottom_right,
            })
        }
    }
}

impl fmt::Display for Rectangle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<{}, {}>", self.top_left, self.bottom_right)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn coords_from_tuple() {
        assert_eq!(Coords { x: 123, y: 456 }, (123, 456).into())
    }

    #[test]
    fn coords_from_array() {
        assert_eq!(Coords { x: 123, y: 456 }, [123, 456].into())
    }

    #[test]
    fn icoords_from_tuple() {
        assert_eq!(ICoords { x: -123, y: -456 }, (-123, -456).into())
    }

    #[test]
    fn icoords_from_array() {
        assert_eq!(ICoords { x: -123, y: -456 }, [-123, -456].into())
    }

    #[test]
    fn coords_add() {
        assert_eq!(
            Coords::from((1, 2)) + Coords::from((3, 4)),
            Coords::from((4, 6))
        )
    }

    #[test]
    fn coords_icoords_add() {
        assert_eq!(
            Coords::from((4, 4)) + ICoords::from((-1, 1)),
            Coords::from((3, 5))
        )
    }

    #[test]
    fn rectangle_contains_coords() {
        let r: Rectangle = ((0, 0), (10, 10)).try_into().unwrap();

        assert!(r.contains_point((0, 0)));
        assert!(r.contains_point((0, 1)));
        assert!(r.contains_point((10, 1)));
        assert!(r.contains_point((10, 10)));
        assert!(!r.contains_point((11, 10)));
    }

    #[test]
    fn rectangle_contains_other() {
        let r: Rectangle = ((5, 5), (10, 10)).try_into().unwrap();

        assert!(r.contains_rectangle(r));

        let r1: Rectangle = ((0, 0), (5, 5)).try_into().unwrap();

        assert!(!r.contains_rectangle(r1));

        let r2: Rectangle = ((5, 5), (6, 6)).try_into().unwrap();

        assert!(r.contains_rectangle(r2));
    }
}

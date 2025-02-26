#[derive(Clone, Copy, Default, PartialEq, Eq)]
pub struct StyledStr<'a>(Style, &'a str);
impl std::fmt::Display for StyledStr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}{}", self.0, self.1, Style::default())
    }
}
impl Style {
    pub fn apply(self, t: &str) -> StyledStr<'_> {
        StyledStr(self, t)
    }
}
impl<'a> From<&'a str> for StyledStr<'a> {
    fn from(value: &'a str) -> Self {
        Self(Style::default(), value)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Color {
    Red,
    Yellow,
    Cyan,
}
impl std::fmt::Display for Color {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Color::Red => write!(f, "\x1b[31m"),
            Color::Yellow => write!(f, "\x1b[33m"),
            Color::Cyan => write!(f, "\x1b[36m"),
        }
    }
}

#[derive(Clone, Copy, Default, PartialEq, Eq)]
pub struct Style {
    color: Option<Color>,
    underline: bool,
}
impl std::ops::BitOr for Style {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self {
            color: self.color.or(rhs.color),
            underline: self.underline | rhs.underline,
        }
    }
}
impl From<Color> for Style {
    fn from(value: Color) -> Self {
        Self {
            color: Some(value),
            ..Default::default()
        }
    }
}
impl Style {
    pub fn red() -> Style {
        Color::Red.into()
    }
    pub fn yellow() -> Style {
        Color::Yellow.into()
    }
    pub fn cyan() -> Style {
        Color::Cyan.into()
    }
    pub fn underline() -> Style {
        Self {
            underline: true,
            ..Default::default()
        }
    }
}
impl std::fmt::Display for Style {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self == &Self::default() {
            return write!(f, "\x1b[m");
        }
        if let Some(c) = self.color {
            write!(f, "{}", c)?;
        }
        if self.underline {
            write!(f, "\x1b[4m")?;
        }
        Ok(())
    }
}

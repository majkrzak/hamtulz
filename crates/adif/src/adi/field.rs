use std::fmt;

use nom::IResult;
use nom::Parser;
use nom::bytes::complete::{is_not, tag, take};
use nom::character::complete::usize;
use nom::combinator::opt;
use nom::sequence::delimited;
use nom::sequence::preceded;

/// A single ADI data-specifier.
///
/// Represents one `<NAME:LENGTH[:TYPE]>VALUE` element in an ADI file.
/// At this level the field is completely opaque — no validation is performed
/// on the name, type indicator, or data contents. This is a pure wire-format
/// representation per [ADIF 3.1.7 §IV.A.1](https://www.adif.org/317/ADIF_317.htm#ADI_Data_Specifiers).
#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: String,
    pub type_indicator: Option<String>,
    pub value: String,
}

impl Field {
    pub fn parse(input: &str) -> IResult<&str, Self> {
        let (input, (name, length, type_indicator)) = delimited(
            tag("<"),
            (
                is_not(":>"),
                preceded(tag(":"), usize),
                opt(preceded(tag(":"), is_not(">"))),
            ),
            tag(">"),
        )
        .parse(input)?;
        let (input, value) = take(length).parse(input)?;

        Ok((
            input,
            Field {
                name: name.into(),
                type_indicator: type_indicator.map(String::from),
                value: value.into(),
            },
        ))
    }
}

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.type_indicator {
            Some(ti) => write!(
                f,
                "<{}:{}:{}>{}",
                self.name,
                self.value.len(),
                ti,
                self.value
            ),
            None => write!(f, "<{}:{}>{}", self.name, self.value.len(), self.value),
        }
    }
}

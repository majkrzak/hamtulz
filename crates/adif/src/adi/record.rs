use std::fmt;

use nom::IResult;
use nom::Parser;
use nom::branch::alt;
use nom::bytes::complete::tag_no_case;
use nom::character::complete::anychar;
use nom::combinator::{peek, value};
use nom::multi::many_till;
use nom::sequence::preceded;

use super::Field;

/// A single ADI record.
///
/// Represents an `<EOR>`-terminated sequence of data-specifiers.
/// At this level the record is completely opaque — no validation is
/// performed on the fields or their ordering. This is a pure wire-format
/// representation per [ADIF 3.1.7 §IV.A.6](https://www.adif.org/317/ADIF_317.htm#ADI_Records).
#[derive(Debug, Clone, PartialEq)]
pub struct Record {
    pub fields: Vec<Field>,
}

impl Record {
    pub fn parse(input: &str) -> IResult<&str, Self> {
        fn garbage(input: &str) -> IResult<&str, (Vec<char>, ())> {
            many_till(
                anychar,
                peek(alt((
                    value((), Field::parse),
                    value((), tag_no_case("<EOR>")),
                ))),
            )
            .parse(input)
        }

        let (input, (fields, _)) = many_till(
            preceded(garbage, Field::parse),
            preceded(garbage, tag_no_case("<EOR>")),
        )
        .parse(input)?;

        Ok((input, Record { fields }))
    }
}

impl fmt::Display for Record {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for field in &self.fields {
            write!(f, "{}", field)?;
        }
        write!(f, "<EOR>")
    }
}

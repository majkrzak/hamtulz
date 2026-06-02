use std::fmt;

use nom::IResult;
use nom::Parser;
use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case};
use nom::character::complete::anychar;
use nom::combinator::not;
use nom::combinator::{peek, recognize, value};
use nom::multi::many_till;
use nom::sequence::preceded;

use super::Field;

/// An ADI header.
///
/// Represents free-text preamble followed by an `<EOH>`-terminated sequence
/// of data-specifiers.
/// At this level the header is completely opaque — no validation is
/// performed on the fields, their ordering, or the preamble contents.
/// This is a pure wire-format representation per
/// [ADIF 3.1.7 §IV.A.3](https://www.adif.org/317/ADIF_317.htm#ADI_Header).
#[derive(Debug, Clone, PartialEq)]
pub struct Header {
    pub preamble: String,
    pub fields: Vec<Field>,
}

impl Header {
    pub fn parse(input: &str) -> IResult<&str, Self> {
        fn garbage(input: &str) -> IResult<&str, (Vec<char>, ())> {
            many_till(
                anychar,
                peek(alt((
                    value((), Field::parse),
                    value((), tag_no_case("<EOH>")),
                ))),
            )
            .parse(input)
        }

        peek(not(tag("<"))).parse(input)?;

        let (input, preamble) = recognize(garbage).parse(input)?;

        let (input, (fields, _)) = many_till(
            preceded(garbage, Field::parse),
            preceded(garbage, tag_no_case("<EOH>")),
        )
        .parse(input)?;

        Ok((
            input,
            Header {
                preamble: preamble.into(),
                fields,
            },
        ))
    }
}

impl fmt::Display for Header {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for field in &self.fields {
            write!(f, "{}", field)?;
        }
        write!(f, "<EOH>")
    }
}

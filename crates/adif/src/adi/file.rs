use std::fmt;

use nom::IResult;
use nom::Parser;
use nom::combinator::opt;
use nom::multi::many0;

use super::{Header, Record};

/// An ADI file.
///
/// Represents an optional [`Header`] followed by zero or more [`Record`]s.
/// At this level the file is completely opaque — no validation is performed
/// on the header, records, or their ordering. This is a pure wire-format
/// representation per
/// [ADIF 3.1.7 §IV.A.2](https://www.adif.org/317/ADIF_317.htm#ADI_File_Structure).
#[derive(Debug, Clone, PartialEq)]
pub struct File {
    pub header: Option<Header>,
    pub records: Vec<Record>,
}

impl File {
    pub fn parse(input: &str) -> IResult<&str, Self> {
        let (input, header) = opt(Header::parse).parse(input)?;
        let (input, records) = many0(Record::parse).parse(input)?;

        Ok((input, File { header, records }))
    }
}

impl fmt::Display for File {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(header) = &self.header {
            write!(f, "{}", header)?;
        }
        for record in &self.records {
            write!(f, "{}", record)?;
        }
        Ok(())
    }
}

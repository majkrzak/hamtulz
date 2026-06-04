use xmltree::{Element, XMLNode};

use super::{Error, Header, Record, Result};

/// An ADX file.
///
/// Represents an `<ADX>` element with an optional [`Header`] followed by
/// a `<RECORDS>` container holding zero or more [`Record`]s.
/// At this level the file is completely opaque — no validation is performed
/// on the header, records, or their ordering. This is a pure wire-format
/// representation per [ADIF 3.1.7 §IV.B](https://www.adif.org/317/ADIF_317.htm#ADX_File_Format).
#[derive(Debug, Clone, PartialEq)]
pub struct File {
    pub header: Option<Header>,
    pub records: Vec<Record>,
}

impl TryFrom<&Element> for File {
    type Error = Error;

    fn try_from(elem: &Element) -> Result<Self> {
        let mut header = None;
        let mut records = Vec::new();

        for child in &elem.children {
            match child {
                XMLNode::Element(e) => {
                    if e.name == "HEADER" {
                        header = Some(Header::try_from(e)?);
                    } else if e.name == "RECORDS" {
                        for record_child in &e.children {
                            if let XMLNode::Element(re) = record_child {
                                if re.name == "RECORD" {
                                    records.push(Record::try_from(re)?);
                                }
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        Ok(File { header, records })
    }
}

impl From<&File> for Element {
    fn from(file: &File) -> Self {
        let mut elem = Element::new("ADX");
        if let Some(ref header) = file.header {
            elem.children.push(XMLNode::Element(Element::from(header)));
        }
        let mut records = Element::new("RECORDS");
        for record in &file.records {
            records
                .children
                .push(XMLNode::Element(Element::from(record)));
        }
        elem.children.push(XMLNode::Element(records));
        elem
    }
}

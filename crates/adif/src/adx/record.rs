use xmltree::{Element, XMLNode};

use super::{Error, Field, Result};

/// A single ADX record.
///
/// Represents a `<RECORD>` element containing a sequence of field elements.
/// At this level the record is completely opaque — no validation is
/// performed on the fields or their ordering. This is a pure wire-format
/// representation per [ADIF 3.1.7 §IV.B](https://www.adif.org/317/ADIF_317.htm#ADX_File_Format).
#[derive(Debug, Clone, PartialEq)]
pub struct Record {
    pub fields: Vec<Field>,
}

impl TryFrom<&Element> for Record {
    type Error = Error;

    fn try_from(elem: &Element) -> Result<Self> {
        let mut fields = Vec::new();

        for child in &elem.children {
            match child {
                XMLNode::Element(e) => {
                    if ["USERDEF", "APP"].contains(&&*e.name.to_uppercase()) {
                        continue;
                    }
                    fields.push(Field::try_from(e)?);
                }
                _ => {}
            }
        }

        Ok(Record { fields })
    }
}

impl From<&Record> for Element {
    fn from(record: &Record) -> Self {
        let mut elem = Element::new("RECORD");
        for field in &record.fields {
            elem.children.push(XMLNode::Element(Element::from(field)));
        }
        elem
    }
}

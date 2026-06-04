use xmltree::{Element, XMLNode};

use super::{Error, Field, Result};

/// An ADX header.
///
/// Represents a `<HEADER>` element with an optional preamble comment followed
/// by a sequence of field elements.
/// At this level the header is completely opaque — no validation is
/// performed on the fields or their ordering. This is a pure wire-format
/// representation per [ADIF 3.1.7 §IV.B](https://www.adif.org/317/ADIF_317.htm#ADX_File_Format).
#[derive(Debug, Clone, PartialEq)]
pub struct Header {
    pub preamble: Option<String>,
    pub fields: Vec<Field>,
}

impl TryFrom<&Element> for Header {
    type Error = Error;

    fn try_from(elem: &Element) -> Result<Self> {
        let mut preamble = None;
        let mut fields = Vec::new();

        for child in &elem.children {
            match child {
                XMLNode::Element(e) => {
                    if ["USERDEF"].contains(&&*e.name.to_uppercase()) {
                        continue;
                    }
                    fields.push(Field::try_from(e)?);
                }
                XMLNode::Comment(s) => {
                    if preamble.is_none() {
                        preamble = Some(s.clone());
                    }
                }
                _ => {}
            }
        }

        Ok(Header { preamble, fields })
    }
}

impl From<&Header> for Element {
    fn from(header: &Header) -> Self {
        let mut elem = Element::new("HEADER");
        if let Some(ref p) = header.preamble {
            elem.children.push(XMLNode::Comment(p.clone()));
        }
        for field in &header.fields {
            elem.children.push(XMLNode::Element(Element::from(field)));
        }
        elem
    }
}

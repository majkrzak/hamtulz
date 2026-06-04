use xmltree::{Element, XMLNode};

use super::{Error, Result};

/// A single ADX field element.
///
/// Represents one `<NAME>VALUE</NAME>` element in an ADX file.
/// At this level the field is completely opaque — no validation is performed
/// on the name or data contents. This is a pure wire-format representation
/// per [ADIF 3.1.7 §IV.B](https://www.adif.org/317/ADIF_317.htm#ADX_File_Format).
#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: String,
    pub value: String,
}

impl TryFrom<&Element> for Field {
    type Error = Error;

    fn try_from(elem: &Element) -> Result<Self> {
        match elem.children.as_slice() {
            [] => Ok(Field {
                name: elem.name.clone(),
                value: String::new(),
            }),
            [XMLNode::Text(s)] => Ok(Field {
                name: elem.name.clone(),
                value: s.clone(),
            }),
            _ => Err(Error::Unknown),
        }
    }
}

impl From<&Field> for Element {
    fn from(field: &Field) -> Self {
        let mut elem = Element::new(&field.name);
        if !field.value.is_empty() {
            elem.children.push(XMLNode::Text(field.value.clone()));
        }
        elem
    }
}

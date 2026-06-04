use std::io;

use serde::ser;

use crate::error::Result;

pub struct Serializer<W: io::Write> {
    pub writer: W,
}

impl<W: io::Write> Serializer<W> {
    pub fn new(writer: W) -> Self {
        Serializer { writer }
    }
}

impl<'a, W: io::Write> ser::Serializer for &'a mut Serializer<W> {
    type Ok = ();
    type Error = crate::Error;

    type SerializeSeq = Self;
    type SerializeTuple = Self;
    type SerializeTupleStruct = Self;
    type SerializeTupleVariant = Self;
    type SerializeMap = Self;
    type SerializeStruct = Self;
    type SerializeStructVariant = Self;

    fn serialize_bool(self, _v: bool) -> Result<Self::Ok> {
        unimplemented!()
    }
    fn serialize_i8(self, _v: i8) -> Result<Self::Ok> {
        unimplemented!()
    }
    fn serialize_i16(self, _v: i16) -> Result<Self::Ok> {
        unimplemented!()
    }
    fn serialize_i32(self, _v: i32) -> Result<Self::Ok> {
        unimplemented!()
    }
    fn serialize_i64(self, _v: i64) -> Result<Self::Ok> {
        unimplemented!()
    }
    fn serialize_u8(self, _v: u8) -> Result<Self::Ok> {
        unimplemented!()
    }
    fn serialize_u16(self, _v: u16) -> Result<Self::Ok> {
        unimplemented!()
    }
    fn serialize_u32(self, _v: u32) -> Result<Self::Ok> {
        unimplemented!()
    }
    fn serialize_u64(self, _v: u64) -> Result<Self::Ok> {
        unimplemented!()
    }
    fn serialize_f32(self, _v: f32) -> Result<Self::Ok> {
        unimplemented!()
    }
    fn serialize_f64(self, _v: f64) -> Result<Self::Ok> {
        unimplemented!()
    }
    fn serialize_char(self, _v: char) -> Result<Self::Ok> {
        unimplemented!()
    }
    fn serialize_str(self, _v: &str) -> Result<Self::Ok> {
        unimplemented!()
    }
    fn serialize_bytes(self, _v: &[u8]) -> Result<Self::Ok> {
        unimplemented!()
    }
    fn serialize_none(self) -> Result<Self::Ok> {
        unimplemented!()
    }
    fn serialize_some<T: ?Sized>(self, _value: &T) -> Result<Self::Ok>
    where
        T: ser::Serialize,
    {
        unimplemented!()
    }
    fn serialize_unit(self) -> Result<Self::Ok> {
        unimplemented!()
    }
    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok> {
        unimplemented!()
    }
    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _idx: u32,
        _variant: &'static str,
    ) -> Result<Self::Ok> {
        unimplemented!()
    }
    fn serialize_newtype_struct<T: ?Sized>(
        self,
        _name: &'static str,
        _value: &T,
    ) -> Result<Self::Ok>
    where
        T: ser::Serialize,
    {
        unimplemented!()
    }
    fn serialize_newtype_variant<T: ?Sized>(
        self,
        _name: &'static str,
        _idx: u32,
        _variant: &'static str,
        _value: &T,
    ) -> Result<Self::Ok>
    where
        T: ser::Serialize,
    {
        unimplemented!()
    }
    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
        unimplemented!()
    }
    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple> {
        unimplemented!()
    }
    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        unimplemented!()
    }
    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _idx: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        unimplemented!()
    }
    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
        unimplemented!()
    }
    fn serialize_struct(self, _name: &'static str, _len: usize) -> Result<Self::SerializeStruct> {
        unimplemented!()
    }
    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _idx: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        unimplemented!()
    }
}

impl<'a, W: io::Write> ser::SerializeSeq for &'a mut Serializer<W> {
    type Ok = ();
    type Error = crate::Error;
    fn serialize_element<T: ?Sized>(&mut self, _value: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        unimplemented!()
    }
    fn end(self) -> Result<Self::Ok> {
        unimplemented!()
    }
}

impl<'a, W: io::Write> ser::SerializeTuple for &'a mut Serializer<W> {
    type Ok = ();
    type Error = crate::Error;
    fn serialize_element<T: ?Sized>(&mut self, _value: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        unimplemented!()
    }
    fn end(self) -> Result<Self::Ok> {
        unimplemented!()
    }
}

impl<'a, W: io::Write> ser::SerializeTupleStruct for &'a mut Serializer<W> {
    type Ok = ();
    type Error = crate::Error;
    fn serialize_field<T: ?Sized>(&mut self, _value: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        unimplemented!()
    }
    fn end(self) -> Result<Self::Ok> {
        unimplemented!()
    }
}

impl<'a, W: io::Write> ser::SerializeTupleVariant for &'a mut Serializer<W> {
    type Ok = ();
    type Error = crate::Error;
    fn serialize_field<T: ?Sized>(&mut self, _value: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        unimplemented!()
    }
    fn end(self) -> Result<Self::Ok> {
        unimplemented!()
    }
}

impl<'a, W: io::Write> ser::SerializeMap for &'a mut Serializer<W> {
    type Ok = ();
    type Error = crate::Error;
    fn serialize_key<T: ?Sized>(&mut self, _key: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        unimplemented!()
    }
    fn serialize_value<T: ?Sized>(&mut self, _value: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        unimplemented!()
    }
    fn end(self) -> Result<Self::Ok> {
        unimplemented!()
    }
}

impl<'a, W: io::Write> ser::SerializeStruct for &'a mut Serializer<W> {
    type Ok = ();
    type Error = crate::Error;
    fn serialize_field<T: ?Sized>(&mut self, _key: &'static str, _value: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        unimplemented!()
    }
    fn end(self) -> Result<Self::Ok> {
        unimplemented!()
    }
}

impl<'a, W: io::Write> ser::SerializeStructVariant for &'a mut Serializer<W> {
    type Ok = ();
    type Error = crate::Error;
    fn serialize_field<T: ?Sized>(&mut self, _key: &'static str, _value: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        unimplemented!()
    }
    fn end(self) -> Result<Self::Ok> {
        unimplemented!()
    }
}

pub fn to_string<T: ser::Serialize>(value: &T) -> Result<String> {
    let mut buf = Vec::new();
    {
        let mut ser = Serializer::new(&mut buf);
        value.serialize(&mut ser)?;
    }
    let s = String::from_utf8(buf).map_err(|e| crate::Error::Custom(e.to_string()))?;
    Ok(s)
}

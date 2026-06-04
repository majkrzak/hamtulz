use crate::{Qso, adi, adx};

macro_rules! define_log {
    ($($(#[$attr:meta])* $field:ident : $ty:ty),* $(,)?) => {
        /// An ADIF log per [ADIF 3.1.7 §III.C.1.a](https://www.adif.org/317/ADIF_317.htm).
        #[derive(Debug, Clone, PartialEq)]
        pub struct Log {
            /// Optional preamble text preceding the data specifiers in the header.
            pub preamble: Option<String>,
            $($(#[$attr])* pub $field: Option<$ty>),*,
            /// QSO records contained in the log.
            pub qsos: Vec<Qso>,
        }

        impl TryFrom<&adi::File> for Log {
            type Error = crate::Error;

            fn try_from(file: &adi::File) -> Result<Self, Self::Error> {
                let preamble = file.header.as_ref().map(|h| h.preamble.clone());
                let mut log = Log {
                    preamble,
                    $($field: None),*,
                    qsos: Vec::new(),
                };

                if let Some(header) = &file.header {
                    for f in &header.fields {
                        $(
                            if f.name.eq_ignore_ascii_case(stringify!($field)) {
                                log.$field = Some(
                                    f.value.parse().map_err(|_| {
                                        crate::Error::InvalidFieldValue {
                                            field: stringify!($field).to_string(),
                                            value: f.value.clone(),
                                        }
                                    })?,
                                );
                            }
                        )*
                    }
                }

                for record in &file.records {
                    log.qsos.push(Qso::try_from(record)?);
                }

                Ok(log)
            }
        }

        impl From<&Log> for adi::File {
            fn from(log: &Log) -> Self {
                let mut fields = Vec::new();
                let mut has_header_fields = false;
                $(
                    if let Some(val) = &log.$field {
                        has_header_fields = true;
                        fields.push(adi::Field {
                            name: stringify!($field).to_uppercase(),
                            type_indicator: None,
                            value: val.to_string(),
                        });
                    }
                )*

                let header = if log.preamble.is_some() || has_header_fields {
                    Some(adi::Header {
                        preamble: log.preamble.clone().unwrap_or_default(),
                        fields,
                    })
                } else {
                    None
                };

                let records = log.qsos.iter().map(adi::Record::from).collect();
                adi::File { header, records }
            }
        }

        impl TryFrom<&adx::File> for Log {
            type Error = crate::Error;

            fn try_from(file: &adx::File) -> Result<Self, Self::Error> {
                let preamble = file.header.as_ref().and_then(|h| h.preamble.clone());
                let mut log = Log {
                    preamble,
                    $($field: None),*,
                    qsos: Vec::new(),
                };

                if let Some(header) = &file.header {
                    for f in &header.fields {
                        $(
                            if f.name.eq_ignore_ascii_case(stringify!($field)) {
                                log.$field = Some(
                                    f.value.parse().map_err(|_| {
                                        crate::Error::InvalidFieldValue {
                                            field: stringify!($field).to_string(),
                                            value: f.value.clone(),
                                        }
                                    })?,
                                );
                            }
                        )*
                    }
                }

                for record in &file.records {
                    log.qsos.push(Qso::try_from(record)?);
                }

                Ok(log)
            }
        }

        impl From<&Log> for adx::File {
            fn from(log: &Log) -> Self {
                let mut fields = Vec::new();
                let mut has_header_fields = false;
                $(
                    if let Some(val) = &log.$field {
                        has_header_fields = true;
                        fields.push(adx::Field {
                            name: stringify!($field).to_uppercase(),
                            value: val.to_string(),
                        });
                    }
                )*

                let header = if log.preamble.is_some() || has_header_fields {
                    Some(adx::Header {
                        preamble: Some(log.preamble.clone().unwrap_or_default()),
                        fields,
                    })
                } else {
                    None
                };

                let records = log.qsos.iter().map(adx::Record::from).collect();
                adx::File { header, records }
            }
        }
    };
}

define_log! {
    /// Identifies the version of ADIF used in this file in the format X.Y.Z
    adif_ver: String,
    /// Identifies the UTC date and time that the file was created (YYYYMMDD HHMMSS)
    created_timestamp: String,
    /// Identifies the name of the logger, converter, or utility that created or processed this ADIF content
    programid: String,
    /// Identifies the version of the logger, converter, or utility that created or processed this ADIF file
    programversion: String,
}

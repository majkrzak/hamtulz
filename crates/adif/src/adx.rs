use thiserror::Error;

pub mod field;
pub mod file;
pub mod header;
pub mod record;

pub use field::Field;
pub use file::File;
pub use header::Header;
pub use record::Record;

#[derive(Debug, Error)]
pub enum Error {
    #[error("unknown error")]
    Unknown,
}

pub type Result<T> = std::result::Result<T, Error>;

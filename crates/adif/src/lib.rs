use thiserror::Error;

pub mod adi;
pub mod adx;
pub mod log;
pub mod qso;

pub use log::Log;
pub use qso::Qso;

#[derive(Debug, Error)]
pub enum Error {
    #[error("invalid value '{value}' for field '{field}'")]
    InvalidFieldValue { field: String, value: String },
}

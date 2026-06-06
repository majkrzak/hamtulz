use serde_with::{DeserializeFromStr, SerializeDisplay};
use std::fmt;
use std::num::ParseFloatError;
use std::str::FromStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq, SerializeDisplay, DeserializeFromStr)]
pub struct Frequency(u64);

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ParseFrequencyError {
    #[error("invalid frequency value, {0}")]
    ParseFloatError(#[from] ParseFloatError),
}

impl Frequency {
    pub fn from_mhz(mhz: f64) -> Self {
        Frequency((mhz * 1_000_000.0).round() as u64)
    }

    pub fn as_mhz(self) -> f64 {
        self.0 as f64 / 1_000_000.0
    }
}

impl fmt::Display for Frequency {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mhz = self.as_mhz();
        write!(f, "{mhz}")
    }
}

impl FromStr for Frequency {
    type Err = ParseFrequencyError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mhz: f64 = s.parse()?;
        Ok(Frequency::from_mhz(mhz))
    }
}

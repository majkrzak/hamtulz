use serde::{Deserialize, Serialize};
use std::fmt;
use std::str::FromStr;

/// Frequency in MHz.
///
/// Can be parsed from a decimal MHz string or a floating-point value,
/// and serializes to and from a decimal string via serde.
///
/// # Examples
///
/// ```
/// # use hamtulz_radio::Frequency;
/// let f: Frequency = "14.200".parse().unwrap();
/// assert_eq!(f.as_mhz(), 14.2);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(into = "String", try_from = "&str")]
pub struct Frequency(u64);

impl Frequency {
    /// Create a `Frequency` from a floating-point MHz value.
    pub fn from_mhz(mhz: f64) -> Self {
        Frequency((mhz * 1_000_000.0).round() as u64)
    }

    /// Return the frequency as an `f64` MHz value.
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
    type Err = FrequencyParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mhz: f64 = s.parse().map_err(|_| FrequencyParseError)?;
        Ok(Frequency::from_mhz(mhz))
    }
}

impl From<f64> for Frequency {
    fn from(mhz: f64) -> Self {
        Frequency::from_mhz(mhz)
    }
}

impl From<Frequency> for f64 {
    fn from(f: Frequency) -> Self {
        f.as_mhz()
    }
}

impl TryFrom<&str> for Frequency {
    type Error = FrequencyParseError;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        s.parse()
    }
}

impl From<Frequency> for String {
    fn from(f: Frequency) -> Self {
        f.to_string()
    }
}

/// Error returned when parsing an invalid frequency string.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
#[error("invalid frequency")]
pub struct FrequencyParseError;

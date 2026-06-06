use serde::{Deserialize, Serialize};
use std::fmt;
use std::str::FromStr;

macro_rules! bands {
    ($([$name:ident, $label:literal $(, ($lo:expr, $hi:expr))?]),+ $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
        #[serde(into = "String", try_from = "&str")]
        pub enum Band { $( $name, )+ }

        impl fmt::Display for Band {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $( Band::$name => write!(f, $label), )+
                }
            }
        }

        impl FromStr for Band {
            type Err = ParseBandError;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    $( $label => Ok(Band::$name), )+
                    _ => Err(ParseBandError::InvalidValue),
                }
            }
        }

        impl TryFrom<&str> for Band {
            type Error = ParseBandError;

            fn try_from(s: &str) -> Result<Self, Self::Error> {
                s.parse()
            }
        }

        impl From<Band> for String {
            fn from(b: Band) -> Self {
                b.to_string()
            }
        }

        impl TryFrom<crate::Frequency> for Band {
            type Error = OutOfBandFrequency;

            fn try_from(f: crate::Frequency) -> Result<Self, Self::Error> {
                let mhz = f.as_mhz();
                match () {
                    $( $( () if ($lo..=$hi).contains(&mhz) => Ok(Band::$name), )? )+
                    _ => Err(OutOfBandFrequency(f)),
                }
            }
        }
    };
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ParseBandError {
    #[error("invalid band value")]
    InvalidValue,
}

#[derive(Debug, Clone, PartialEq, thiserror::Error)]
#[error("frequency {0} MHz out of defined bands")]
pub struct OutOfBandFrequency(pub crate::Frequency);

bands! {
    [TwoThousandOneHundredNinetyMeters, "2190m", (0.1357, 0.1378)],
    [SixHundredThirtyMeters, "630m", (0.472, 0.479)],
    [FiveHundredSixtyMeters, "560m", (0.501, 0.504)],
    [OneHundredSixtyMeters, "160m", (1.8, 2.0)],
    [EightyMeters, "80m", (3.5, 4.0)],
    [SixtyMeters, "60m", (5.06, 5.45)],
    [FortyMeters, "40m", (7.0, 7.3)],
    [ThirtyMeters, "30m", (10.10, 10.15)],
    [TwentyMeters, "20m", (14.00, 14.35)],
    [SeventeenMeters, "17m", (18.068, 18.168)],
    [FifteenMeters, "15m", (21.00, 21.45)],
    [TwelveMeters, "12m", (24.890, 24.990)],
    [TenMeters, "10m", (28.0, 29.7)],
    [EightMeters, "8m", (40.0, 45.0)],
    [SixMeters, "6m", (50.0, 54.0)],
    [FiveMeters, "5m", (54.000001, 69.9)],
    [FourMeters, "4m", (70.0, 71.0)],
    [TwoMeters, "2m", (144.0, 148.0)],
    [OneAndTwentyFiveHundredthsMeters, "1.25m", (222.0, 225.0)],
    [SeventyCentimetres, "70cm", (420.0, 450.0)],
    [ThirtyThreeCentimetres, "33cm", (902.0, 928.0)],
    [TwentyThreeCentimetres, "23cm", (1240.0, 1300.0)],
    [ThirteenCentimetres, "13cm", (2300.0, 2450.0)],
    [NineCentimetres, "9cm", (3300.0, 3500.0)],
    [SixCentimetres, "6cm", (5650.0, 5925.0)],
    [ThreeCentimetres, "3cm", (10000.0, 10500.0)],
    [OneAndTwentyFiveHundredthsCentimetres, "1.25cm", (24000.0, 24250.0)],
    [SixMilimetres, "6mm", (47000.0, 47200.0)],
    [FourMilimetres, "4mm", (75500.0, 81000.0)],
    [TwoAndFiveTenthsMilimeters, "2.5mm", (119980.0, 123000.0)],
    [TwoMilimeters, "2mm", (134000.0, 149000.0)],
    [OneMilimeter, "1mm", (241000.0, 250000.0)],
    [SubMilimieter, "submm", (300000.0, 7500000.0)],
}

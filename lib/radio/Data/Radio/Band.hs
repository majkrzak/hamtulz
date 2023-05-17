module Data.Radio.Band
  ( Band (..),
    fromFrequency,
  )
where

import Data.Empty (Empty)
import Data.Text (pack, unpack)
import Data.Yaml.Builder (ToYaml, toYaml)
import Data.Yaml.Helper (showFromYaml, showToYaml)
import Data.Yaml.Parser (FromYaml, fromYaml)
import GHC.Generics (Generic)
import Text.ParserCombinators.ReadP (choice, string)
import Text.Read (lift, readEither, readPrec)

-- | Radio Bands enumeration
-- Based on and compliant with https://adif.org/314/ADIF_314.htm#Band_Enumeration
data Band
  = TwoThousandOneHundredNinetyMeters
  | SixHundredThirtyMeters
  | FiveHundredSixtyMeters
  | OneHundredSixtyMeters
  | EightyMeters
  | SixtyMeters
  | FortyMeters
  | ThirtyMeters
  | TwentyMeters
  | SeventeenMeters
  | FifteenMeters
  | TwelveMeters
  | TenMeters
  | EightMeters
  | SixMeters
  | FiveMeters
  | FourMeters
  | TwoMeters
  | OneAndTwentyFiveHundredthsMeters
  | SeventyCentimetres
  | ThirtyThreeCentimetres
  | TwentyThreeCentimetres
  | ThirteenCentimetres
  | NineCentimetres
  | SixCentimetres
  | ThreeCentimetres
  | OneAndTwentyFiveHundredthsCentimetres
  | SixMilimetres
  | FourMilimetres
  | TwoAndFiveTenthsMilimeters
  | TwoMilimeters
  | OneMilimeter
  | SubMilimieter
  deriving (Eq, Generic, Empty)

instance Show Band where
  show TwoThousandOneHundredNinetyMeters = "2190m"
  show SixHundredThirtyMeters = "630m"
  show FiveHundredSixtyMeters = "560m"
  show OneHundredSixtyMeters = "160m"
  show EightyMeters = "80m"
  show SixtyMeters = "60m"
  show FortyMeters = "40m"
  show ThirtyMeters = "30m"
  show TwentyMeters = "20m"
  show SeventeenMeters = "17m"
  show FifteenMeters = "15m"
  show TwelveMeters = "12m"
  show TenMeters = "10m"
  show EightMeters = "8m"
  show SixMeters = "6m"
  show FiveMeters = "5m"
  show FourMeters = "4m"
  show TwoMeters = "2m"
  show OneAndTwentyFiveHundredthsMeters = "1.25m"
  show SeventyCentimetres = "70cm"
  show ThirtyThreeCentimetres = "33cm"
  show TwentyThreeCentimetres = "23cm"
  show ThirteenCentimetres = "13cm"
  show NineCentimetres = "9cm"
  show SixCentimetres = "6cm"
  show ThreeCentimetres = "3cm"
  show OneAndTwentyFiveHundredthsCentimetres = "1.25cm"
  show SixMilimetres = "6mm"
  show FourMilimetres = "4mm"
  show TwoAndFiveTenthsMilimeters = "2.5mm"
  show TwoMilimeters = "2mm"
  show OneMilimeter = "1mm"
  show SubMilimieter = "submm"

instance Read Band where
  readPrec =
    lift $
      choice
        [ string "2190m" >> return TwoThousandOneHundredNinetyMeters,
          string "630m" >> return SixHundredThirtyMeters,
          string "560m" >> return FiveHundredSixtyMeters,
          string "160m" >> return OneHundredSixtyMeters,
          string "80m" >> return EightyMeters,
          string "60m" >> return SixtyMeters,
          string "40m" >> return FortyMeters,
          string "30m" >> return ThirtyMeters,
          string "20m" >> return TwentyMeters,
          string "17m" >> return SeventeenMeters,
          string "15m" >> return FifteenMeters,
          string "12m" >> return TwelveMeters,
          string "10m" >> return TenMeters,
          string "8m" >> return EightMeters,
          string "6m" >> return SixMeters,
          string "5m" >> return FiveMeters,
          string "4m" >> return FourMeters,
          string "2m" >> return TwoMeters,
          string "1.25m" >> return OneAndTwentyFiveHundredthsMeters,
          string "70cm" >> return SeventyCentimetres,
          string "33cm" >> return ThirtyThreeCentimetres,
          string "23cm" >> return TwentyThreeCentimetres,
          string "13cm" >> return ThirteenCentimetres,
          string "9cm" >> return NineCentimetres,
          string "6cm" >> return SixCentimetres,
          string "3cm" >> return ThreeCentimetres,
          string "1.25cm" >> return OneAndTwentyFiveHundredthsCentimetres,
          string "6mm" >> return SixMilimetres,
          string "4mm" >> return FourMilimetres,
          string "2.5mm" >> return TwoAndFiveTenthsMilimeters,
          string "2mm" >> return TwoMilimeters,
          string "1mm" >> return OneMilimeter,
          string "submm" >> return SubMilimieter
        ]

instance FromYaml Band where
  fromYaml = showFromYaml

instance ToYaml Band where
  toYaml = showToYaml

-- | Convert frequency in MHz to Band, if possible
fromFrequency :: Double -> Maybe Band
fromFrequency f
  | 0.1357 <= f && f <= 0.1378 = Just TwoThousandOneHundredNinetyMeters
  | 0.472 <= f && f <= 0.479 = Just SixHundredThirtyMeters
  | 0.501 <= f && f <= 0.504 = Just FiveHundredSixtyMeters
  | 1.8 <= f && f <= 2.0 = Just OneHundredSixtyMeters
  | 3.5 <= f && f <= 4.0 = Just EightyMeters
  | 5.06 <= f && f <= 5.45 = Just SixtyMeters
  | 7.0 <= f && f <= 7.3 = Just FortyMeters
  | 10.10 <= f && f <= 10.15 = Just ThirtyMeters
  | 14.00 <= f && f <= 14.35 = Just TwentyMeters
  | 18.068 <= f && f <= 18.168 = Just SeventeenMeters
  | 21.00 <= f && f <= 21.45 = Just FifteenMeters
  | 24.890 <= f && f <= 24.990 = Just TwelveMeters
  | 28.0 <= f && f <= 29.7 = Just TenMeters
  | 40 <= f && f <= 45 = Just EightMeters
  | 50 <= f && f <= 54 = Just SixMeters
  | 54 < f && f < 70 = Just FiveMeters
  | 70 <= f && f <= 71 = Just FourMeters
  | 144 <= f && f <= 148 = Just TwoMeters
  | 222 <= f && f <= 225 = Just OneAndTwentyFiveHundredthsMeters
  | 420 <= f && f <= 450 = Just SeventyCentimetres
  | 902 <= f && f <= 928 = Just ThirtyThreeCentimetres
  | 1240 <= f && f <= 1300 = Just TwentyThreeCentimetres
  | 2300 <= f && f <= 2450 = Just ThirteenCentimetres
  | 3300 <= f && f <= 3500 = Just NineCentimetres
  | 5650 <= f && f <= 5925 = Just SixCentimetres
  | 10000 <= f && f <= 10500 = Just ThreeCentimetres
  | 24000 <= f && f <= 24250 = Just OneAndTwentyFiveHundredthsCentimetres
  | 47000 <= f && f <= 47200 = Just SixMilimetres
  | 75500 <= f && f <= 81000 = Just FourMilimetres
  | 119980 <= f && f <= 123000 = Just TwoAndFiveTenthsMilimeters
  | 134000 <= f && f <= 149000 = Just TwoMilimeters
  | 241000 <= f && f <= 250000 = Just OneMilimeter
  | 300000 <= f && f <= 7500000 = Just SubMilimieter
  | otherwise = Nothing

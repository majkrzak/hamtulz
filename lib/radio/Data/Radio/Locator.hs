module Data.Radio.Locator (Locator) where

import Data.Empty (Empty)
import Data.Yaml.Builder (ToYaml, toYaml)
import Data.Yaml.Helper (showFromYaml, showToYaml)
import Data.Yaml.Parser (FromYaml, fromYaml)
import GHC.Generics (Generic)
import Text.ParserCombinators.ReadP (choice, string)
import Text.Read (lift, pfail, readEither, readPrec, (<++))

data Locator
  = Locator1 Field
  | Locator2 Field Square
  | Locator3 Field Square Subsquare
  | Locator4 Field Square Subsquare Extsquare
  deriving (Eq, Generic, Empty)

data Field = Field Field_ Field_ deriving (Eq, Generic, Empty)

data Square = Square Square_ Square_ deriving (Eq, Generic, Empty)

data Subsquare = Subsquare Subsquare_ Subsquare_ deriving (Eq, Generic, Empty)

data Extsquare = Extsquare Extsquare_ Extsquare_ deriving (Eq, Generic, Empty)

data Field_
  = Field_A
  | Field_B
  | Field_C
  | Field_D
  | Field_E
  | Field_F
  | Field_G
  | Field_H
  | Field_I
  | Field_J
  | Field_K
  | Field_L
  | Field_M
  | Field_N
  | Field_O
  | Field_P
  | Field_Q
  | Field_R
  deriving (Eq, Generic, Empty)

data Square_
  = Square_0
  | Square_1
  | Square_2
  | Square_3
  | Square_4
  | Square_5
  | Square_6
  | Square_7
  | Square_8
  | Square_9
  deriving (Eq, Generic, Empty)

data Subsquare_
  = Subsquare_a
  | Subsquare_b
  | Subsquare_c
  | Subsquare_d
  | Subsquare_e
  | Subsquare_f
  | Subsquare_g
  | Subsquare_h
  | Subsquare_i
  | Subsquare_j
  | Subsquare_k
  | Subsquare_l
  | Subsquare_m
  | Subsquare_n
  | Subsquare_o
  | Subsquare_p
  | Subsquare_q
  | Subsquare_r
  | Subsquare_s
  | Subsquare_t
  | Subsquare_u
  | Subsquare_v
  | Subsquare_w
  | Subsquare_x
  deriving (Eq, Generic, Empty)

data Extsquare_
  = Extsquare_0
  | Extsquare_1
  | Extsquare_2
  | Extsquare_3
  | Extsquare_4
  | Extsquare_5
  | Extsquare_6
  | Extsquare_7
  | Extsquare_8
  | Extsquare_9
  deriving (Eq, Generic, Empty)

instance Show Locator where
  show (Locator1 field) = show field
  show (Locator2 field square) = show field <> show square
  show (Locator3 field square subsquare) = show field <> show square <> show subsquare
  show (Locator4 field square subsquare extsquare) = show field <> show square <> show subsquare <> show extsquare

instance Show Field where
  show (Field lon lat) = show lon <> show lat

instance Show Square where
  show (Square lon lat) = show lon <> show lat

instance Show Subsquare where
  show (Subsquare lon lat) = show lon <> show lat

instance Show Extsquare where
  show (Extsquare lon lat) = show lon <> show lat

instance Show Field_ where
  show Field_A = "A"
  show Field_B = "B"
  show Field_C = "C"
  show Field_D = "D"
  show Field_E = "E"
  show Field_F = "F"
  show Field_G = "G"
  show Field_H = "H"
  show Field_I = "I"
  show Field_J = "J"
  show Field_K = "K"
  show Field_L = "L"
  show Field_M = "M"
  show Field_N = "N"
  show Field_O = "O"
  show Field_P = "P"
  show Field_Q = "Q"
  show Field_R = "R"

instance Show Square_ where
  show Square_0 = "0"
  show Square_1 = "1"
  show Square_2 = "2"
  show Square_3 = "3"
  show Square_4 = "4"
  show Square_5 = "5"
  show Square_6 = "6"
  show Square_7 = "7"
  show Square_8 = "8"
  show Square_9 = "9"

instance Show Subsquare_ where
  show Subsquare_a = "a"
  show Subsquare_b = "b"
  show Subsquare_c = "c"
  show Subsquare_d = "d"
  show Subsquare_e = "e"
  show Subsquare_f = "f"
  show Subsquare_g = "g"
  show Subsquare_h = "h"
  show Subsquare_i = "i"
  show Subsquare_j = "j"
  show Subsquare_k = "k"
  show Subsquare_l = "l"
  show Subsquare_m = "m"
  show Subsquare_n = "n"
  show Subsquare_o = "o"
  show Subsquare_p = "p"
  show Subsquare_q = "q"
  show Subsquare_r = "r"
  show Subsquare_s = "s"
  show Subsquare_t = "t"
  show Subsquare_u = "u"
  show Subsquare_v = "v"
  show Subsquare_w = "w"
  show Subsquare_x = "x"

instance Show Extsquare_ where
  show Extsquare_0 = "0"
  show Extsquare_1 = "1"
  show Extsquare_2 = "2"
  show Extsquare_3 = "3"
  show Extsquare_4 = "4"
  show Extsquare_5 = "5"
  show Extsquare_6 = "6"
  show Extsquare_7 = "7"
  show Extsquare_8 = "8"
  show Extsquare_9 = "9"

instance Read Locator where
  readPrec =
    foldr
      (<++)
      pfail
      [ Locator4 <$> readPrec <*> readPrec <*> readPrec <*> readPrec,
        Locator3 <$> readPrec <*> readPrec <*> readPrec,
        Locator2 <$> readPrec <*> readPrec,
        Locator1 <$> readPrec
      ]

instance Read Field where
  readPrec = Field <$> readPrec <*> readPrec

instance Read Square where
  readPrec = Square <$> readPrec <*> readPrec

instance Read Subsquare where
  readPrec = Subsquare <$> readPrec <*> readPrec

instance Read Extsquare where
  readPrec = Extsquare <$> readPrec <*> readPrec

instance Read Field_ where
  readPrec =
    lift $
      choice
        [ string "A" >> return Field_A,
          string "B" >> return Field_B,
          string "C" >> return Field_C,
          string "D" >> return Field_D,
          string "E" >> return Field_E,
          string "F" >> return Field_F,
          string "G" >> return Field_G,
          string "H" >> return Field_H,
          string "I" >> return Field_I,
          string "J" >> return Field_J,
          string "K" >> return Field_K,
          string "L" >> return Field_L,
          string "M" >> return Field_M,
          string "N" >> return Field_N,
          string "O" >> return Field_O,
          string "P" >> return Field_P,
          string "Q" >> return Field_Q,
          string "R" >> return Field_R
        ]

instance Read Square_ where
  readPrec =
    lift $
      choice
        [ string "0" >> return Square_0,
          string "1" >> return Square_1,
          string "2" >> return Square_2,
          string "3" >> return Square_3,
          string "4" >> return Square_4,
          string "5" >> return Square_5,
          string "6" >> return Square_6,
          string "7" >> return Square_7,
          string "8" >> return Square_8,
          string "9" >> return Square_9
        ]

instance Read Subsquare_ where
  readPrec =
    lift $
      choice
        [ string "a" >> return Subsquare_a,
          string "b" >> return Subsquare_b,
          string "c" >> return Subsquare_c,
          string "d" >> return Subsquare_d,
          string "e" >> return Subsquare_e,
          string "f" >> return Subsquare_f,
          string "g" >> return Subsquare_g,
          string "h" >> return Subsquare_h,
          string "i" >> return Subsquare_i,
          string "j" >> return Subsquare_j,
          string "k" >> return Subsquare_k,
          string "l" >> return Subsquare_l,
          string "m" >> return Subsquare_m,
          string "n" >> return Subsquare_n,
          string "o" >> return Subsquare_o,
          string "p" >> return Subsquare_p,
          string "q" >> return Subsquare_q,
          string "r" >> return Subsquare_r,
          string "s" >> return Subsquare_s,
          string "t" >> return Subsquare_t,
          string "u" >> return Subsquare_u,
          string "v" >> return Subsquare_v,
          string "w" >> return Subsquare_w,
          string "x" >> return Subsquare_x
        ]

instance Read Extsquare_ where
  readPrec =
    lift $
      choice
        [ string "0" >> return Extsquare_0,
          string "1" >> return Extsquare_1,
          string "2" >> return Extsquare_2,
          string "3" >> return Extsquare_3,
          string "4" >> return Extsquare_4,
          string "5" >> return Extsquare_5,
          string "6" >> return Extsquare_6,
          string "7" >> return Extsquare_7,
          string "8" >> return Extsquare_8,
          string "9" >> return Extsquare_9
        ]

instance FromYaml Locator where
  fromYaml = showFromYaml

instance ToYaml Locator where
  toYaml = showToYaml

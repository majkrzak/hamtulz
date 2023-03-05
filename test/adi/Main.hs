module Main where

import Data.Adi
import Data.Valid
import Test.QuickCheck (Arbitrary, arbitrary)
import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.QuickCheck as QC

instance Arbitrary Field where
  arbitrary = suchThat (Field <$> arbitrary) valid

instance Arbitrary Record where
  arbitrary = suchThat (Record <$> arbitrary) valid

instance Arbitrary Header where
  arbitrary = suchThat (Header <$> arbitrary) valid

instance Arbitrary Document where
  arbitrary = suchThat (Document <$> arbitrary) valid

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testGroup
        "Read Write"
        [ QC.testProperty "Field" $
            \(field :: Field) -> read (show field) == field,
          QC.testProperty "[Field]" $
            \(fields :: [Field]) -> read (show fields) == fields,
          QC.testProperty "Record" $
            \(record :: Record) -> read (show record) == record,
          QC.testProperty "[Record]" $
            \(records :: [Record]) -> read (show records) == records,
          QC.testProperty "Header" $
            \(header :: Header) -> read (show header) == header,
          QC.testProperty "Document" $
            \(document :: Document) -> read (show document) == document
        ],
      testGroup
        "reads"
        [ HU.testCase "Field + <EOR>" $ reads "<A:1>A<EOR>" @?= [(Field ("A", "A"), "<EOR>")],
          HU.testCase "[Field] + <EOR>" $ reads "<A:1>A<EOR>" @?= [([], "<A:1>A<EOR>"), ([Field ("A", "A")], "<EOR>")],
          HU.testCase "Record" $ reads "<A:1>A<EOR>" @?= [(Record [Field ("A", "A")], "")],
          HU.testCase "Header" $ reads "TEST\n<A:1>A\n<EOH>" @?= [(Header ("TEST", [Field ("A", "A")]), "")],
          HU.testCase "Header without fields" $ reads "TEST\n<EOH>" @?= [(Header ("TEST", []), "")],
          HU.testCase "Header multiple fields" $ reads "TEST\n<A:1>A <B:1>B\n<EOH>" @?= [(Header ("TEST", [Field ("A", "A"), Field ("B", "B")]), "")]
        ],
      testGroup
        "show"
        [ HU.testCase "Field" $ show (Field ("A", "A")) @?= "<A:1>A",
          HU.testCase "[Field]" $ show [Field ("A", "A"), Field ("B", "B")] @?= "<A:1>A <B:1>B",
          HU.testCase "Record" $ show (Record [Field ("A", "A"), Field ("B", "B")]) @?= "<A:1>A <B:1>B <EOR>",
          HU.testCase "Header" $ show (Header ("TEST", [Field ("A", "A")])) @?= "TEST\n<A:1>A\n<EOH>",
          HU.testCase "Document" $ show (Document (Just (Header ("TEST", [Field ("A", "A")])), [Record [Field ("B", "B")]])) @?= "TEST\n<A:1>A\n<EOH>\n<B:1>B <EOR>\n"
        ],
      testGroup
        "!valid"
        [ HU.testCase "empty Header" $ valid (Header ("", [])) @?= False,
          HU.testCase "space Header" $ valid (Header (" ", [])) @?= False
        ]
    ]

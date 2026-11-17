module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tendril qualified
import Test.Tendril.Edit.Command qualified
import Test.Tendril.Edit.KillRing qualified
import Test.Tendril.Edit.Mode qualified
import Test.Tendril.Text.Buffer qualified
import Test.Tendril.Text.Grapheme qualified
import Test.Tendril.Text.Oracle qualified
import Test.Tendril.Text.Unicode qualified
import Test.Tendril.Text.Units qualified
import Test.Tendril.Text.Width qualified

main :: IO ()
main =
  defaultMain $
    testGroup
      "tendril-test"
      [ Test.Tendril.tests,
        Test.Tendril.Text.Unicode.tests,
        Test.Tendril.Text.Units.tests,
        Test.Tendril.Text.Grapheme.tests,
        Test.Tendril.Text.Width.tests,
        Test.Tendril.Text.Oracle.tests,
        Test.Tendril.Text.Buffer.tests,
        Test.Tendril.Edit.KillRing.tests,
        Test.Tendril.Edit.Command.tests,
        Test.Tendril.Edit.Mode.tests
      ]

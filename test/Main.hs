module Main (main) where

-- import Test.QuickCheck
-- import Test.QuickCheck.Gen (oneof)

import qualified EditorSpec
import qualified RefEditorSpec

main :: IO ()
main = do
  EditorSpec.runTests
  RefEditorSpec.runTests

  

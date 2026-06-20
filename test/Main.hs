module Main (main) where

-- import Test.QuickCheck
-- import Test.QuickCheck.Gen (oneof)

import qualified EditorSpec
import qualified RefEditorSpec
import qualified GapEditorSpec
import qualified MutRefEditorSpec


main :: IO ()
main = do
  -- EditorSpec.runTests
  -- RefEditorSpec.runTests
  -- GapEditorSpec.runTests
  MutRefEditorSpec.runTests
  

  

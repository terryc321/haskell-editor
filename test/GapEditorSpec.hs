module GapEditorSpec (runTests) where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

import GapEditor 

runTests :: IO ()
runTests = do
  putStrLn "starting GapEditorSpec tests"
  quickCheck prop_run
  quickCheck prop_insert_increases_length
  quickCheck prop_run_consistency
  quickCheck prop_insert_length
  quickCheck prop_delete_length
  quickCheck prop_moveLeft_preserves_contents
  quickCheck prop_moveRight_preserves_contents
  quickCheck prop_move_left_right_preserves_contents
  quickCheck prop_insert_then_delete_length
  quickCheck prop_insert_then_delete
  quickCheck prop_run_composition
  putStrLn "completed GapEditorSpec tests"


-- prop_editor_matches_reference :: [Command] -> Editor -> Bool
-- prop_editor_matches_reference cmds e =
--   contents (run cmds e)  == refRun cmds (contents e)

prop_run_composition :: [Command] -> [Command] -> Editor -> Bool
prop_run_composition xs ys e =
  run (xs ++ ys) e
  == run ys (run xs e)

prop_insert_then_delete :: Char -> Editor -> Bool
prop_insert_then_delete c e =
  let e1 = run [Insert c, Delete] e
  in e1 == e 


prop_insert_then_delete_length :: Char -> Editor -> Bool
prop_insert_then_delete_length c e =
  let e1 = run [Insert c, Delete] e
  in length (contents e1) <= length (contents e) + 1  

-- cursor move left then move right should not change the document
prop_move_left_right_preserves_contents :: Editor -> Bool
prop_move_left_right_preserves_contents e =
  contents (run [MoveRight, MoveLeft] e)
  == contents e
  
-- move right does not change the document
prop_moveRight_preserves_contents :: Editor -> Bool
prop_moveRight_preserves_contents e =
  contents (run [MoveRight] e) == contents e

-- move left does not change the document
prop_moveLeft_preserves_contents :: Editor -> Bool
prop_moveLeft_preserves_contents e =
  contents (run [MoveLeft] e) == contents e
  
-- delete command results in an equal or smaller length contents
-- buffer may be empty or cursor point is already far left as possible
prop_delete_length :: Editor -> Bool
prop_delete_length e =
  length (contents (run [Delete] e))
  <= length (contents e)

-- insert one character increases length of contents by one 
prop_insert_length :: Char -> Editor -> Bool
prop_insert_length c e =
  length (contents (run [Insert c] e))
  == length (contents e) + 1

instance Arbitrary Command where
  arbitrary =
    oneof
      [ Insert <$> arbitrary
      , pure Delete
      , pure MoveLeft
      , pure MoveRight
      ]

instance Arbitrary Editor where
  arbitrary = do
    xs <- arbitrary
    ys <- arbitrary
    return (Editor { left = xs, right = ys })

prop_run_consistency :: [Command] -> Editor -> Bool
prop_run_consistency cmds e =
  run cmds e == run cmds e
  

prop_run :: Editor -> Bool
prop_run e = run [] e == e

prop_insert_increases_length :: Char -> Editor -> Bool
prop_insert_increases_length c e =
  length (contents (run [Insert c] e)) == length (contents e) + 1


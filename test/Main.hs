module Main (main) where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

import Editor

main :: IO ()
main = do
  quickCheck prop_run
  quickCheck prop_insert_increases_length
  quickCheck prop_run_consistency


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

  

{--

-- prop_run :: [Char] -> [Char] -> Bool
-- prop_run xs ys =
--   let e = Editor {left = xs , right = ys}
--   in run [] e == e



-- Global Instance: Includes ALL constructors
-- Used by default for general tests
instance Arbitrary Command where
    arbitrary = oneof 
        [ pure MoveLeft
        , pure MoveRight
        , Insert <$> arbitrary
        , pure Delete
        ]


genMoveOnly :: Gen Command
genMoveOnly = oneof 
    [ pure MoveLeft
    , pure MoveRight
    ]

-- 4. Property Test: Uses the restricted list
prop_onlyMoves :: Property
prop_onlyMoves = 
    forAll (listOf genMoveOnly) $ \cmds ->
        -- Verify logic assuming no Insert/Delete occurred
        -- Optional sanity check:
        all isMove cmds
  where
    isMove MoveLeft  = True
    isMove MoveRight = True
    isMove _         = False

-- 5. General Property Test: Uses ALL constructors (default)
prop_allCommands :: [Command] -> Bool
prop_allCommands cmds = True 
    

instance Arbitrary Command where
    arbitrary = oneof 
        [ MoveLeft
        , MoveRight
        , Insert <$> arbitrary
        , Delete
        ]

genLimitedCommand :: Gen Command
genLimitedCommand = oneof 
    [ MoveLeft
    , MoveRight
    -- Insert and Delete excluded here
    ]


-- 4. Test using the FULL set (uses default 'arbitrary')
prop_allCommands :: [Command] -> Bool
prop_allCommands cmds = 
    -- Logic that assumes all constructor types might appear
    True 

-- 5. Test using the RESTRICTED set (uses 'forAll')
prop_onlyCreateDelete :: Property
prop_onlyCreateDelete = 
    forAll (listOf genLimitedCommand) $ \cmds ->
        -- Logic that only makes sense for Create/Delete
        -- We can even assert the restriction holds as a sanity check
        all isLimited cmds
  where
    isLimited (Create _) = True
    isLimited (Delete _) = True
    isLimited _          = False





prop_move :: [Char] -> [Char] -> Bool
prop_move xs ys =
  let e = Editor {left = xs , right = ys}
  in run [] e == e

--}


module Command
  ( -- core types
    Command(..),    
  ) where 


data Command
    = Insert Char
    | Delete
    | MoveLeft
    | MoveRight
    deriving (Show,Eq)

-- apply :: Command -> Editor -> Editor
-- apply (Insert c)  e = insert c e
-- apply (Delete)    e = delete e
-- apply (MoveLeft)  e = moveLeft e
-- apply (MoveRight) e = moveRight e

-- run :: [Command] -> Editor -> Editor
-- run [] e = e
-- run (h:t) e = run t (apply h e)



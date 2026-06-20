
module Editor
  ( -- core types
    Editor(..),
    Command(..),
    -- interpreter
    apply,
    run,
    -- observable api
    contents,
    cursorPosition,
    -- primitives
    insert,
    delete,
    moveLeft,
    moveRight,
    empty,
    exec
  ) where 

{-- a two list Editor

       abc | def     ==    abcdef
      left   right        contents 

--}

import Command 

type Text = [Char]

data Editor = Editor
    { left  :: [Char]
    , right :: [Char]
    } deriving (Show , Eq)

empty :: Editor
empty = Editor {left = "" , right = "" }


-- represents abc|def    
example1 = Editor {left="abc",right= "def"}

butlast :: [Char] -> [Char]
butlast [] = []
butlast xs = init xs

safeLast :: [Char] -> Maybe Char
safeLast [] = Nothing
safeLast (h:[]) = Just h
safeLast (h:t) = safeLast t

safeHead :: [Char] -> Maybe Char
safeHead [] = Nothing
safeHead (h:_) = Just h

contents :: Editor -> [Char]
contents (Editor {left=l , right=r}) = l ++ r 

insert :: Char -> Editor -> Editor
insert ch (Editor {left=l , right=r}) = Editor {left= l ++ [ch],right =r}

delete :: Editor -> Editor
delete (Editor {left=l , right=r}) = Editor {left= butlast l,right =r}

-- since we use pattern matching we can guarantee left has a last element
moveLeft :: Editor -> Editor
moveLeft (Editor {left=[] , right=r}) = (Editor {left=[] , right=r})
moveLeft (Editor {left=(h:t) , right=r}) =
  let a = last (h:t)
  in Editor {left= butlast (h:t),right = a : r}

moveRight :: Editor -> Editor
moveRight (Editor {left=l , right=[] }) = (Editor {left=l , right=[] })
moveRight (Editor {left=l , right=(h:t)}) =
   Editor {left= l ++ [h] ,right = t}

cursorPosition :: Editor -> Int
cursorPosition (Editor {left=l , right= _ }) = length l 

-- data Command
--     = Insert Char
--     | Delete
--     | MoveLeft
--     | MoveRight
--     deriving (Show,Eq)


apply :: Command -> Editor -> Editor
apply (Insert c)  e = insert c e
apply (Delete)    e = delete e
apply (MoveLeft)  e = moveLeft e
apply (MoveRight) e = moveRight e

run :: [Command] -> Editor -> Editor
run [] e = e
run (h:t) e = run t (apply h e)

exec :: [Command] -> [Char]
exec cmds = contents (run cmds empty)


example2 = run
  [ MoveLeft
  , MoveLeft
  , Insert 'A'
  , MoveRight
  , Delete
  , MoveRight
  ]
  example1





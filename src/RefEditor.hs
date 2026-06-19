


module RefEditor
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
    --
    empty 
  ) where 

{-- a two linked list editor , the difference here is that the first list
is in reverse

     cba | def    == abcdef
    left   right     contents

--}


type Text = [Char]

data Editor = Editor
    { left  :: [Char]
    , right :: [Char]
    } deriving (Show , Eq)

empty :: Editor 
empty = Editor { left = "" , right = "" }


butlast :: [Char] -> [Char]
butlast [] = []
butlast xs = init xs

butfirst :: [Char] -> [Char]
butfirst [] = []
butfirst (h:t) = t


safeLast :: [Char] -> Maybe Char
safeLast [] = Nothing
safeLast (h:[]) = Just h
safeLast (h:t) = safeLast t

safeHead :: [Char] -> Maybe Char
safeHead [] = Nothing
safeHead (h:_) = Just h

contents :: Editor -> [Char]
contents (Editor {left=l , right=r}) = (reverse l) ++ r 

-- insert = conses onto front of left list 
insert :: Char -> Editor -> Editor
insert ch (Editor {left=l , right=r}) = Editor {left= ch : l ,right =r}

-- delete drops the head of left list if it can
delete :: Editor -> Editor
delete (Editor {left=l , right=r}) = Editor {left= butfirst l,right =r}

-- move left will put head of left onto cons right
moveLeft :: Editor -> Editor
moveLeft (Editor {left=[] , right=r})    =  Editor {left=[] , right=r}
moveLeft (Editor {left=(h:t) , right=r}) =  Editor {left= t,right = h : r}

-- move right will put head of right onto head left
moveRight :: Editor -> Editor
moveRight (Editor {left=l , right=[] }) = (Editor {left=l , right=[] })
moveRight (Editor {left=l , right=(h:t)}) =
   Editor {left= h : l ,right = t}

-- position same computation 
cursorPosition :: Editor -> Int
cursorPosition (Editor {left=l , right= _ }) = length l 


data Command
    = Insert Char
    | Delete
    | MoveLeft
    | MoveRight
    deriving (Show,Eq)

apply :: Command -> Editor -> Editor
apply (Insert c)  e = insert c e
apply (Delete)    e = delete e
apply (MoveLeft)  e = moveLeft e
apply (MoveRight) e = moveRight e

run :: [Command] -> Editor -> Editor
run [] e = e
run (h:t) e = run t (apply h e)

-- fallback when Editor to Editor created a single string
-- but we create a similar two lists except first is reversed 

-- -- a conversion from one type of editor to this editor ??
-- toRef :: E.Editor -> R.Editor
-- toRef = contents

-- represents abc|def
example1 :: Editor
example1 = Editor {left="cba",right= "def"}

example2 :: Editor 
example2 = run
  [ MoveLeft
  , MoveLeft
  , Insert 'A'
  , MoveRight
  , Delete
  , MoveRight
  ]
  example1





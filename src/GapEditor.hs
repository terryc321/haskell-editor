
-- for gap buffer implementation 

module GapEditor
  ( -- core types
    Editor(..),
    Command(..),
    -- -- interpreter
    apply,
    run,
    -- -- observable api
    contents,
    cursorPosition,
    -- -- primitives
    insert,
    delete,
    moveLeft,
    moveRight,
    -- --
    empty 
  ) where 

import Command

import qualified Data.Vector as V 
import qualified Data.List as L 

data Cell
    = Gap
    | Ch Char
    deriving (Show, Eq)

data Editor = Editor
    { buffer  :: V.Vector Cell
    , gapStart :: Int
    , gapEnd :: Int
    } deriving (Show , Eq)


apply :: Command -> Editor -> Editor
apply (Insert c)  e = insert c e
apply (Delete)    e = delete e
apply (MoveLeft)  e = moveLeft e
apply (MoveRight) e = moveRight e

run :: [Command] -> Editor -> Editor
run [] e = e
run (h:t) e = run t (apply h e)

contents :: Editor -> [Char]
contents e =
  let xs = V.toList (buffer e)
  in let ys = filter (\e -> not (e == Gap)) xs  -- let map (\(Ch c) -> c) $
     in map (\(Ch c) -> c) ys

cursorPosition :: Editor -> Int
cursorPosition e = gapStart e 

editSize :: Editor -> Int
editSize e = V.length (buffer e) - 1 

empty :: Editor 
empty = let initialSize = 5 -- lets not have sz be zero or negative !        
        in Editor { buffer = V.replicate initialSize Gap ,
                    gapStart = 0 ,
                    gapEnd = initialSize - 1 }

moveLeft :: Editor -> Editor
moveLeft e  =
  let buf   = buffer e
      start = gapStart e
      end   = gapEnd e
  in if start > 0
     then let cell = buf V.! (start - 1)
          in let buf2 = V.update buf (V.fromList [(start - 1, Gap),(end,cell)])
             in e { buffer = buf2 , gapStart = max 0 (start - 1) , gapEnd = max 0 (end - 1)  }
     else e

moveRight :: Editor -> Editor
moveRight e =
  let buf   = buffer e
      start = gapStart e
      end   = gapEnd e
      sz    = (V.length (buffer e)) - 1
  in if (end + 1) <= sz 
     then let cell = buf V.! (end + 1)
          in let buf2 = V.update buf (V.fromList [(start, cell),(end+1,Gap)])
             in e { buffer = buf2 , gapStart = start + 1 , gapEnd = end + 1  }
     else e

delete :: Editor -> Editor
delete e  =
  let buf = buffer e
      start = gapStart e
      end   = gapEnd e
  in if start > 0
     then let buf2 = V.update buf (V.fromList [(start - 1, Gap)])
          in e { buffer = buf2 , gapStart = start - 1 }
     else e

insert :: Char -> Editor -> Editor
insert ch e  =
  let start = gapStart e
      end   = gapEnd e
  in if start >= end
     then growInsert ch e
     else plainInsert ch e
    
plainInsert :: Char -> Editor -> Editor    
plainInsert ch e =
  let buf   = buffer e
      start = gapStart e
      end   = gapEnd e 
  in let buf2 = V.update buf (V.fromList [(start, Ch ch)])
     in e {buffer = buf2 , gapStart = start + 1}

growInsert :: Char -> Editor -> Editor    
growInsert ch e = plainInsert ch $ grow e

copyUp :: Editor -> [(Int,Cell)]
copyUp e = copyUp2 0 []             
  where
    copyUp2 :: Int -> [(Int,Cell)] -> [(Int,Cell)]
    copyUp2 i acc = case (buffer e V.! i) of 
                          Gap -> acc
                          Ch c -> copyUp2 (i+1) ((i,Ch c) : acc)

copyDown :: Editor -> [(Int,Cell)]
copyDown e = copyDown2 (V.length (buffer e) - 1) []             
  where
    copyDown2 :: Int -> [(Int,Cell)] -> [(Int,Cell)]
    copyDown2 i acc = case (buffer e V.! i) of 
                          Gap -> acc
                          Ch c -> copyDown2 (i-1) ((i,Ch c) : acc)

growFixUp :: [(Int,Cell)] -> Int -> [(Int,Cell)]
growFixUp xs n = map fixup xs
  where fixup :: (Int,Cell) -> (Int,Cell)
        fixup (v,cell) = (v+n,cell)

grow :: Editor -> Editor
grow e =
  let buf   = buffer e
      start = gapStart e
      end   = gapEnd e
      sz    = editSize e  
  in let newSize = sz + 2
     in let newBuffer = V.replicate newSize Gap
        in let newBuffer2 = newBuffer V.// (copyUp e)
           in let newBuffer3 = newBuffer2 V.// (growFixUp (copyDown e) (newSize - sz))
              in Editor { buffer = newBuffer3 , gapStart= start,gapEnd= end + (newSize - sz) - 1 }
              






-- for gap buffer implementation 

module GapEditor
  ( -- core types
    Editor(..),
    Command(..),
    Cell(..),
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
    empty,
    mkEditor,
    trace,
    -- --
    charCount     
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


contents :: Editor -> [Char]
contents e = let xs = V.toList (buffer e)
                 ys = filter (\e -> not (e == Gap)) xs
             in map (\(Ch c) -> c) ys

  -- let map (\(Ch c) -> c) $                
-- contents e = V.toList $ V.filter (notGap) (buffer e)
--   where notGap = (\e -> e /= Gap)



charCount :: Editor -> Int
charCount e = V.length (V.filter (\e -> e /= Gap) (buffer e))



{--
if we have a gap equal to amount text length (xs ++ ys)

   0     len
        xs-1
   |<----->| <--------> |<----->|
     xs       gap          ys
   xs from 0 to lenXs - 1
gap from lenXs to lenXs + lenXs + lenYs
   ys from (lenXs + lenXs + lenYs) + 1
      to (lenXs + lenXs + lenYs) + 1 + lenYs 
 
   xs = 3  gap = 7  ys = 4

   X X  X  |  G G G G G G G     |  Y  Y  Y  Y
   0 1  2  |  3 4 5 6 7 8 9     |  10 11 12 13
       ^^^               ^^^      ^^        ^^^
      xs-1            xs+gap-1   xs+gap    xs+gap+ys-1                     
             

the smallest the buffer can be is clamped to be atleast 5

--}

mkEditor :: [Char] -> [Char] -> Editor
mkEditor xs ys =
  let lenXs = length xs
      lenYs = length ys
      lenGap = max (lenXs + lenYs) 5 
      lenTot = lenGap + lenXs + lenYs
      xc = map (\c -> Ch c) xs
      yc = map (\c -> Ch c) ys
      gc = L.replicate lenGap Gap
      all = xc ++ gc ++ yc
      st = lenXs
      en = lenXs + lenGap - 1
  in Editor { buffer = V.fromList all , gapStart = st , gapEnd = en }
      
-- test =
--   let x = 3
--       y = x + 1
--       z = x + y
--   in z
         

  

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
      sz    = (V.length (buffer e)) - 1
  in if start > 0 && start <= sz
     then let buf2 = V.update buf (V.fromList [(start - 1, Gap)])
          in e { buffer = buf2 , gapStart = start - 1 }
     else e

insert :: Char -> Editor -> Editor
insert ch e  =
  let start = gapStart e
      end   = gapEnd e
  in if start >= end - 1
     then growInsert ch e -- ignore growing
     else plainInsert ch e
    
plainInsert :: Char -> Editor -> Editor    
plainInsert ch e =
  let buf   = buffer e
      start = gapStart e
      end   = gapEnd e 
      sz    = (V.length (buffer e)) - 1
  in let buf2 = V.update buf (V.fromList [(start, Ch ch)])
     in e {buffer = buf2 , gapStart = start + 1}

growInsert :: Char -> Editor -> Editor    
growInsert ch e = plainInsert ch $ grow e

copyUp :: Editor -> [(Int,Cell)]
copyUp e =
  let lim = V.length (buffer e) - 1
  in copyUp2 0 lim []             
  where
    copyUp2 :: Int -> Int -> [(Int,Cell)] -> [(Int,Cell)]
    copyUp2 i lim acc =
      if i > lim then acc
      else case (buffer e V.! i) of 
             Gap -> acc
             Ch c -> copyUp2 (i+1) lim ((i,Ch c) : acc)

copyDown :: Editor -> [(Int,Cell)]
copyDown e =
  let lim = 0
  in copyDown2 (V.length (buffer e) - 1) lim []             
  where
    copyDown2 :: Int -> Int -> [(Int,Cell)] -> [(Int,Cell)]
    copyDown2 i lim acc =
      if i < lim then acc
      else case (buffer e V.! i) of 
             Gap -> acc
             Ch c -> copyDown2 (i-1) lim ((i,Ch c) : acc)
             

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
  in let newSize = sz * 2 --sz + 2
     in let newBuffer = V.replicate newSize Gap
        in let newBuffer2 = newBuffer V.// (copyUp e)
           in let newBuffer3 = newBuffer2 V.// (growFixUp (copyDown e) (newSize - sz - 1))
              in Editor { buffer = newBuffer3 , gapStart= start,gapEnd= end + newSize - sz - 1  }
              

{--

sanityCheck :: Editor
sanityCheck = let commands = replicate 100000 (G.Insert 'x')
                  eout = G.run commands G.empty
                    print (length (G.contents eout))
                 
--}


apply :: Command -> Editor -> Editor
apply (Insert c)  e = insert c e
apply (Delete)    e = delete e
apply (MoveLeft)  e = moveLeft e
apply (MoveRight) e = moveRight e

run :: [Command] -> Editor -> Editor
run [] e = e
run (h:t) e = run t (apply h e)


trace2 :: [Command] -> Editor -> IO ()
trace2 [] e = putStrLn $ "trace finished."
trace2 (h:t) e = do putStrLn $ "applying " ++ show h
                    putStrLn $ "editor = " ++ show e
                    let out = apply h e
                    putStrLn $ "out = " ++ show out
                    trace2 t e 

trace :: [Command] -> Editor -> IO ()
trace cmd e = do putStrLn $ "Tracer started "
                 putStrLn $ "commands = " ++ show cmd
                 putStrLn $ "editor = " ++ show e
                 trace2 cmd e
                    


g0 :: Editor
g0 = Editor { buffer = V.fromList [Gap,Ch 'z'] ,gapStart = 0, gapEnd = 0 }

g1 :: Editor
g1 = grow $ g0

g2 :: Editor
g2 = Editor { buffer = V.fromList [Ch 'z',Gap] ,gapStart = 1, gapEnd = 1 }

g3 :: Editor
g3 = grow $ g2 

g4 :: Editor
g4 = let cmds = replicate 1000 (Insert 'x')
     in run cmds empty

-- g5 :: Int
-- g5 = 
        

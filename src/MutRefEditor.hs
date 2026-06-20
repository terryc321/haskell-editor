
module MutRefEditor
  (
    Editor(..),
    -- init 
    empty,
    -- operations 
    insert,
    delete,
    -- execute 
    run,
    apply,
    -- read 
    contents,
    exec
  ) where 

import Command 

import Control.Monad.ST
import Data.STRef

data Editor s = Editor
  { leftRef  :: STRef s [Char]
  , rightRef :: STRef s [Char]
  }

empty :: ST s (Editor s)
empty = do
  l <- newSTRef []
  r <- newSTRef []
  pure (Editor l r)

  
insert :: Char -> Editor s -> ST s ()
insert c e = do
  xs <- readSTRef (leftRef e)
  writeSTRef (leftRef e) (c : xs)

delete :: Editor s -> ST s ()
delete e = do
  xs <- readSTRef (leftRef e)
  writeSTRef (leftRef e) (butfirst xs)

butfirst :: [Char] -> [Char]
butfirst [] = []
butfirst (h:t) = t


moveLeft :: Editor s -> ST s ()
moveLeft e = do
  left <- readSTRef (leftRef e)
  case left of
    [] -> pure ()
    (h:t) -> do right <- readSTRef (rightRef e)
                writeSTRef (leftRef e) t
                writeSTRef (rightRef e) (h:right)

moveRight :: Editor s -> ST s ()
moveRight e = do
  right <- readSTRef (rightRef e)
  case right of
    [] -> pure ()
    (h:t) -> do left <- readSTRef (leftRef e)
                writeSTRef (leftRef e) (h:left)
                writeSTRef (rightRef e) t

cursorPosition :: Editor s -> ST s Int
cursorPosition e = do
  left <- readSTRef (leftRef e)
  pure $ length left

run :: [Command] -> Editor s -> ST s ()
run [] e = pure ()
run (c:cs) e = do  apply c e
                   run cs e

-- run :: [Command] -> Editor s -> ST s [Char]
-- run [] e = do left <- readSTRef (leftRef e)
--               right <- readSTRef (rightRef e)
--               pure $ reverse left ++ right
-- run (c:cs) e = do
--   apply c e
--   run cs e



apply :: Command -> Editor s -> ST s ()
apply (Insert c) e = insert c e
apply (Delete) e = delete e
apply (MoveLeft) e = moveLeft e
apply (MoveRight) e = moveRight e


contents :: Editor s -> ST s [Char]
contents e = do
  l <- readSTRef (leftRef e)
  r <- readSTRef (rightRef e)
  pure (reverse l ++ r)
  
  
exec :: [Command] -> [Char]
exec cmds = runST $ do
  e <- empty
  run cmds e
  contents e


debug1 = exec [Insert 'x']
debug2 = exec [Insert 'x', Delete]

inspect :: Editor s -> ST s ([Char],[Char])
inspect e = do
  xs <- readSTRef (leftRef e)
  ys <- readSTRef (rightRef e)
  pure (xs,ys)
  -- putStrLn $ "im wrong " 
  -- pure ()
  -- pure (xs, ys)

test = runST $ do
  e <- empty
  insert 'x' e
  delete e
  inspect e  

{--
a two linked list editor , the difference here is that the first list
is in reverse

     cba | def    == abcdef
    left   right     contents

with a twist this is a mutation in place editor ?

insert
delete
moveLeft
moveRight
cursorPosition
contents

ST s ()

s is a phantom state thread variable

You can think of it as:

a hidden “world instance ID”

It ensures:

you cannot mix two different mutable worlds
you cannot leak references outside


what is pure ? 

pure :: a -> ST s a

pure means “wrap a normal value into the ST context without doing any mutation”


7. The mental model that will help you most

Inside ST:

do
  create mutable things
  modify them
  finally return a value using pure

So:

newSTRef = allocate memory
writeSTRef = mutate memory
readSTRef = observe memory
pure = finish computation

--}
  

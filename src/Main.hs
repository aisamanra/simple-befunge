{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.Array (Array, array, (!), (//))
import Data.Char (chr, ord)
import Data.Maybe (fromMaybe)
import Control.Monad (void, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State ( StateT(runStateT)
                           , get
                           , put
                           , modify
                           )
import System.Exit (exitSuccess)
import System.Environment (getArgs)
import System.IO (stdout, hFlush)
import System.Random (randomIO)

type Board = Array (Int, Int) Char
data Direction = U | R | D | L deriving (Eq, Show, Enum)
data FungeState = FS
  { location   :: (Int, Int)
  , stack      :: [Int]
  , direction  :: Direction
  , board      :: Board
  , stringMode :: Bool
  } deriving (Eq, Show)
type FungeM a = StateT FungeState IO a

terminate :: String -> FungeM a
terminate s = liftIO $ do
  putStrLn ("\n" ++ s)
  exitSuccess

debug :: FungeM ()
debug = do
  st <- get
  liftIO $ do
    putStrLn $ "FS {"
    putStrLn $ "   , loc=" ++ show (location st)
    putStrLn $ "   , stk=" ++ show (stack st)
    putStrLn $ "   , dir=" ++ show (direction st)
    putStrLn $ "   }"

pop :: FungeM Int
pop = do
  st <- get
  case stack st of
    []     -> terminate "Popping from empty stack!"
    (x:xs) -> do
      put st { stack = xs }
      return x

push :: Int -> FungeM ()
push x = modify go where go st = st { stack = x : stack st }

binOp :: (Int -> Int -> Int) -> FungeM ()
binOp op = do
  a <- pop
  b <- pop
  push (b `op` a)

unOp :: (Int -> Int) -> FungeM ()
unOp op = do
  a <- pop
  push (op a)

setDirection :: Direction -> FungeM ()
setDirection d = modify go where go st = st { direction = d }

randomDirection :: FungeM ()
randomDirection = do
  x <- liftIO randomIO
  setDirection (toEnum x)

fungeIf :: Direction -> Direction -> FungeM ()
fungeIf thenDir elseDir = do
  c <- pop
  setDirection (if c /= 0 then thenDir else elseDir)

toggleStringMode = modify go
  where go st = st { stringMode = not (stringMode st) }

goDir :: Direction -> (Int, Int) -> (Int, Int)
goDir U (x, y) = (x, y - 1)
goDir R (x, y) = (x + 1, y)
goDir D (x, y) = (x, y + 1)
goDir L (x, y) = (x - 1, y)

move :: FungeM ()
move = modify go
  where go st@(FS { direction = d, location = l }) =
           st { location = goDir d l }

getInstr :: FungeM ()
getInstr = do
  y <- pop
  x <- pop
  st <- get
  push (ord (board st ! (x, y)))

putInstr :: FungeM ()
putInstr = do
  y <- pop
  x <- pop
  c <- pop
  modify (\ st -> st { board = board st // [((x, y), chr c)] })

step :: Char -> FungeM ()
step '+' = binOp (+)
step '-' = binOp (-)
step '*' = binOp (*)
step '/' = binOp div
step '%' = binOp mod
step '!' = unOp go where go 0 = 1; go _ = 0
step '`' = binOp go where go x y | x > y = 1 | otherwise = 0
step '>' = setDirection R
step '<' = setDirection L
step '^' = setDirection U
step 'v' = setDirection D
step '?' = randomDirection
step '_' = fungeIf L R
step '|' = fungeIf U D
step '"' = toggleStringMode
step ':' = do { x <- pop; push x; push x }
step '\\' = do { x <- pop; y <- pop; push x; push y }
step '$' = void pop
step '.' = do { x <- pop; liftIO (print x) }
step ',' = do { x <- pop; liftIO (putChar (chr x) >> hFlush stdout) }
step '#' = move
step 'g' = getInstr
step 'p' = putInstr
step '&' = do
  liftIO (putStr "num> " >> hFlush stdout)
  n <- liftIO getLine
  push (read n)
step '~' = do
  liftIO (putStr "chr> " >> hFlush stdout)
  n <- liftIO getLine
  push (ord (head n))
step '@' = terminate "Finished"
step n | n >= '0' && n <= '9' = push (ord n - ord '0')
step _ = return ()

run :: FungeM ()
run = do
  st <- get
  let c = board st ! location st
  if | stringMode st && c == '"' -> toggleStringMode
     | stringMode st && c /= '"' -> push (ord c)
     | otherwise                 -> step c
  move

(!?) :: [[a]] -> (Int, Int) -> Maybe a
xs !? (x, y)
  | x < 0 || y < 0 = Nothing
  | y >= length xs = Nothing
  | x >= length (xs !! y) = Nothing
  | otherwise = Just ((xs !! y) !! x)

buildBoard :: String -> Board
buildBoard s =
  array ((0, 0), (width-1, height-1))
    [ ((x, y), c)
    | x <- [0..width-1]
    , y <- [0..height-1]
    , let c = fromMaybe ' ' (strs !? (x, y))
    ]
  where strs   = lines s
        height = length strs
        width  = maximum (map length strs)

buildInitialState :: String -> FungeState
buildInitialState s =
  FS { board = buildBoard s
     , stack = []
     , direction = R
     , location = (0, 0)
     , stringMode = False
     }

runFunge :: FungeState -> IO ()
runFunge st = void (runStateT (forever run) st)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "No filename given!"
    (f:_) -> do
      c <- readFile f
      runFunge (buildInitialState c)

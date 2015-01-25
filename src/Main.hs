{-# LANGUAGE MultiWayIf #-}

module Main where

import           Control.Monad          (forever, void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State    (StateT (runStateT), get, modify, put)
import           Data.Array             (Array, array, bounds, (!), (//))
import           Data.Char              (chr, isDigit, ord)
import           Data.Maybe             (fromMaybe, listToMaybe)
import           System.Environment     (getArgs)
import           System.Exit            (exitFailure, exitSuccess)
import           System.IO              (hFlush, stdout)
import           System.Random          (randomIO)

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

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

finish :: FungeM a
finish = liftIO $ do { putStrLn ""; exitSuccess }

terminate :: String -> FungeM a
terminate s = do
  st <- get
  liftIO $ do
    let l = location st
    let i = board st ! l
    putStr ("\nbefunge: instr (" ++ [i] ++ ") at " ++ show l ++ "\n  ")
    putStrLn s
    exitFailure

debug :: FungeM ()
debug = do
  st <- get
  liftIO $ do
    putStrLn "FS {"
    putStrLn ("   , loc=" ++ show (location st))
    putStrLn ("   , stk=" ++ show (stack st))
    putStrLn ("   , dir=" ++ show (direction st))
    putStrLn "   }"

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

inBounds :: (Int, Int) -> Board -> Bool
inBounds (x, y) board = x >= lx && y >= ly && x <= hx && y <= hy
  where ((lx, ly), (hx, hy)) = bounds board

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
move = do
  st <- get
  let newLoc = goDir (direction st) (location st)
  if inBounds newLoc (board st)
    then put st { location = newLoc }
    else terminate ("About to move to invalid location: " ++ show newLoc)

getInstr :: FungeM ()
getInstr = do
  y <- pop
  x <- pop
  st <- get
  if inBounds (x, y) (board st)
    then push (ord (board st ! (x, y)))
    else terminate ("Invalid board location: " ++ show (x, y))

putInstr :: FungeM ()
putInstr = do
  y <- pop
  x <- pop
  c <- pop
  st <- get
  if inBounds (x, y) (board st)
    then modify (\ st -> st { board = board st // [((x, y), chr c)] })
    else terminate ("Invalid board location: " ++ show (x, y))

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
  case maybeRead n of
    Just n' -> push n'
    Nothing -> terminate ("Invalid number: " ++ show n)
step '~' = do
  liftIO (putStr "chr> " >> hFlush stdout)
  n <- liftIO getLine
  case n of
    [c] -> push (ord c)
    []  -> terminate "No character given"
    _   -> terminate ("Expected character, got string: " ++ show n)
step '@' = finish
step n | isDigit n = push (ord n - ord '0')
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

usage :: String
usage = "USAGE: befunge [filename]"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn usage
    (f:_) -> do
      c <- readFile f
      runFunge (buildInitialState c)

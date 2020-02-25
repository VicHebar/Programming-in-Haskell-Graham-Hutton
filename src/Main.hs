{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import Data.Char
import Data.List
import System.IO
import Control.Monad
import Control.Applicative hiding (empty)
import System.Random hiding (split)

--main :: IO ()
--main = putStrLn "h"

fibo :: Int -> Int
fibo 1 = 1
fibo 2 = 1
fibo n = fibo (n-1) + fibo (n-2)

fibo' :: Int -> [Int]
fibo' 0 = []
fibo' n = (fibo' (n-1)) ++ [fibo n]

sUM :: [Int] -> Int
sUM [] = 0
sUM [a] = a
sUM (x:xs) = x + sUM xs

resta :: [Int] -> Int
resta [] = 0
resta [a] = -a
resta (x:xs) = -x + resta xs

tautologia :: [Bool] -> Bool
tautologia [] = True
tautologia (x:xs) = x && tautologia xs

holi :: [Bool] -> Bool
holi (x:xs) = x || holi xs

invertir :: String -> [Char]
invertir [] = []
invertir(x:xs) = (invertir xs) ++ [x]


inv :: String -> Int
inv xs = sUM [1 | _ <- xs]

lL :: [a] -> [[a]]
lL as = [[b] | b <- as]

interseccion :: Eq a => [a] -> [a] -> [a]
interseccion as bs = [elementos | elementos <- as, elementos' <- bs, elementos == elementos']


ejemplo1 :: [Int]-> Int -> [Int]
ejemplo1 enteros numero = [elementos | elementos <- enteros , elementos <= numero]

inter ::Eq a => [a] -> [a] -> [a]
inter as bs = [a | a <- as, elem a as && elem a bs]

union' :: [a] -> [a] -> [a]
union' as bs = as ++ bs





longitud :: String -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x + y) 0

let2int :: Char -> Int
let2int c = ord c - ord '0'

int2Str :: Int -> String
int2Str n = map intToDigit (reverse (int2LInt n) )

int2LInt :: Int -> [Int]
int2LInt 0 = []
int2LInt n = (n `mod` 10): int2LInt (n `div` 10)

getNum :: [String] -> [Int]
getNum as = map dec2int (dsti as)

dsti :: [String] -> [[Int]]
dsti as = map (map let2int) as

dsti' :: String -> Int
dsti' as = dec2int (map let2int as)

adder :: IO ()
adder = do putStr "How many numbers? "
           x <- getLine
           let n = dsti' x
           nmbrs <- abc' n []
           let y = sum (concat (dsti nmbrs))
           putStrLn ("The result is " ++ show y)
           return ()

adder' :: IO ()
adder' = do putStr "How many numbers? "
            x <- getLine
            let n = dsti' x
            nmbrs <- sequence [getLine | _ <- [1..n]]
            let y = sum (concat (dsti nmbrs))
            putStrLn ("The result is " ++ show y)


abc' :: Int -> [String] -> IO [String]
abc' 0 c = return c
abc' n c = do x <- getLine
              abc' (n-1) (x:c)

saySmthg :: IO ()
saySmthg = do x <- getLine
              putStrLn x
              return ()

getLine' :: IO String
getLine' = do x <- getChar
              if x == '\n' then
                return []
              else
                do xs <- getLine'
                   return (x:xs)

-------------------------------23/1/2020

size :: Int
size = 3

depth :: Int
depth = 9

data Player = O | B | X deriving (Eq, Ord, Show)
type Grid = [[Player]]
type Pos = (Int, Int)
data Tree a = Node a [Tree a] deriving Show
type Board = [Int]

--let fDepth = getDepth

next' :: Player -> Player
next' O = X
next' B = B
next' X = O

empty :: Grid
empty = replicate size (replicate size B)

empty' :: Grid
empty' = replicate size (replicate size O)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
  where
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)
    ps = concat g

turn' :: Grid -> Player
turn' g = if os < xs then O else X
  where
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)
    ps = concat g

wins :: Grid -> Player -> Bool
wins g p = any line (rows ++ cols ++ dias)
  where
    line = all (== p)
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won g = wins g O || wins g X

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

showRow :: [Player] -> [String]
showRow = foldr1 (zipWith (++)) . interleave (replicate size "|") . map showPlayer

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave [replicate ((size * 4) - 1) '-'] . map showRow

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p = if valid g i then [chop size (xs ++ [p] ++ ys)] else []
  where (xs,B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                     return (read xs)
                   else
                     do putStrLn "ERROR: Invalid number"
                        getNat prompt

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1,1)
             putGrid g
             run' g p

run' :: Grid -> Player -> IO ()
run' g p | wins g O = putStrLn "Player O wins!!"
         | wins g X = putStrLn "Player X wins!!"
         | full g = putStrLn "It's a draw"
         | otherwise =
             do i <- getNat (prompt p)
                case move g i p of
                  [] -> do putStrLn "ERROR Invalid move"
                           run' g p
                  [g'] -> do putGrid g'
                             run' g' (next' p)

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next' p) | g' <- moves g p]

lTree :: Grid -> Int
lTree g = length(cTree (prune depth (gametree g O)))

cTree :: Tree Grid -> [Grid]
cTree (Node g []) = [g]
cTree (Node g ts) = [g] ++ concat( map cTree ts)

moves :: Grid -> Player -> [Grid]
moves g p | won g = []
          | full g = []
          | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

----------------------------------------------------27/1/20

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
  | wins g O = Node (g, O) []
  | wins g X = Node (g, X) []
  | otherwise = Node (g, B) []
minimax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
    where
      ts' = map minimax ts
      ps = [p | Node (_, p) _ <- ts']

minimax' :: Tree Grid -> Tree (Grid, Player)
minimax' (Node g [])
  | wins g O = Node (g, O) []
  | wins g X = Node (g, X) []
  | otherwise = Node (g, B) []
minimax' (Node g ts)
  | turn' g == O = Node (g, minimum ps) ts'
  | turn' g == X = Node (g, maximum ps) ts'
    where
      ts' = map minimax' ts
      ps = [p | Node (_, p) _ <- ts']

minMax :: Player -> (Tree Grid -> Tree (Grid, Player))
minMax p = if p == O then minimax else minimax' 

bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g', p') _ <- ts, p' == best]
  where
    tree = prune depth (gametree g p)
    Node (_, best) ts = minimax tree

play2 :: Grid -> Player -> IO ()
play2 g p = do cls
               goto (1,1)
               putGrid g
               play2' g p

play2' :: Grid -> Player -> IO ()
play2' g p
  | wins g O = putStrLn "Player O wins."
  | wins g X = putStrLn "Player X wins."
  | full g = putStrLn "It's a draw"
  | p == O = do i <- getNat (prompt p)
                case move g i p of
                  [] -> do putStrLn "ERROR: Invalid move."
                           play2' g p
                  [g'] -> play2 g' (next' p)
  | p == X = do putStrLn "Player x is thinking..."
               -- g' <- randBestMove g p
                (play2 $! (bestmove g p)) (next' p)

bestmoveWOD :: Grid -> Player -> Int -> Grid
bestmoveWOD g p dep = head [g' | Node (g', p') _ <- ts, p' == best]
  where
    tree = prune dep (gametree g p)
    Node (_, best) ts = minimax tree

subMain :: IO ()
subMain = do putStrLn "Give me the depth: "
             dep <- getNat ""
             ticTTPlay empty O dep

ticTTPlay :: Grid -> Player -> Int -> IO ()
ticTTPlay g p dep = do cls
                       goto (1,1)
                       putGrid g
                       ticTT g p dep

ticTT :: Grid -> Player -> Int -> IO ()
ticTT g p dep
  | wins g O = putStrLn "Player O wins."
  | wins g X = putStrLn "Player X wins."
  | full g = putStrLn "It's a draw"
  | p == O = do i <- getNat (prompt p)
                case move g i p of
                  [] -> do putStrLn "ERROR: Invalid move."
                           ticTTPlay g p dep
                  [g'] -> ticTTPlay g' (next' p) dep
  | p == X = do putStrLn "Player x is thinking..."
                (ticTTPlay $! (bestmoveWOD g p dep)) (next' p) dep

-- bestmoveFinal :: Grid -> Player -> Tree (Grid, Player) -> (Grid, Tree (Grid, Player))
-- bestmoveFinal g p t = if p == plyr then  wnrTuple else bestmoveFinal g p (funcAuxiliar g gs)
--   where
--     Node (grd, plyr) gs = t
--     wnrMovs = [Node(g', p') gs' |  Node (g', p') gs' <- gs, p' == plyr]
--     Node (gW, pW) gWs = head wnrMovs
--     wnrTuple = (gW, Node (gW, pW) gWs)

bestmoveFinal :: Grid -> Player -> Player -> Tree Grid -> (Grid, Tree Grid)
bestmoveFinal g p fp t = head [(g', funcAuxiliar g' ts') | Node (g', p') _ <- ts, p' == plyr]
  where
    Node (grds, plyr) ts = (minMax fp)  t
    Node grd' ts' = t

funcAuxiliar :: Grid -> [Tree Grid] -> Tree Grid
funcAuxiliar _ [t] = t
funcAuxiliar g (t:ts) = if g == g' then t else funcAuxiliar g ts
  where
    Node g' _ = t

subMain2 :: IO ()
subMain2 = do dep <- getNat "Give me the depth: "
              gameRoll dep

gameRoll :: Int -> IO ()
gameRoll dep = do putStrLn "Do you wanna go first? (y/n)"
                  op <- getChar
                  case op of
                    'y' -> do let tree = prune dep (gametree empty O)
                              ticTTPlayFinal empty O O tree
                    'n' -> do let tree = prune dep (gametree empty X)
                              ticTTPlayFinal empty X X tree
                    otherwise -> gameRoll dep

ticTTPlayFinal :: Grid -> Player -> Player -> Tree Grid -> IO ()
ticTTPlayFinal g p fp t = do cls
                             goto (1,1)
                             putGrid g
                             ticTTFinal g p fp t

ticTTFinal :: Grid -> Player -> Player -> Tree Grid -> IO ()
ticTTFinal g p fp t
  | wins g O = putStrLn "O wins."
  | wins g X = putStrLn "X wins."
  | full g = putStrLn "It's a draw."
  | p == O = do i <- getNat (prompt p)
                case move g i p of
                  [] -> do putStrLn "ERROR: Invalid move."
                           ticTTPlayFinal g p fp t
                  [g'] -> do let Node grd ts = t
                                 t' = funcAuxiliar g' ts
                             ticTTPlayFinal g' (next' p) fp t'
  | p == X = do putStrLn "Player X is thinking..."
                let (g', t') = bestmoveFinal g p fp t
                ticTTPlayFinal g' (next' p) fp t'

main :: IO ()
main = do def <- getLine
          play3 empty O

play3 :: Grid -> Player -> IO ()
play3 g p = do cls
               goto (1,1)
               putGrid g
               play2'' g p

play2'' :: Grid -> Player -> IO ()
play2'' g p
  | wins g O = putStrLn "Player O wins."
  | wins g X = putStrLn "Player X wins."
  | full g = putStrLn "It's a draw"
  | p == O = do i <- getNat (prompt p)
                case move g i p of
                  [] -> do putStrLn "ERROR: Invalid move."
                           play3 g p
                  [g'] -> play3 g' (next' p)
  | p == X = do putStrLn "Player x is thinking..."
                (play3 $! (quickstMove 1 g p)) (next' p)


-- choices :: [a] -> [[a]]
-- choices = concat . map perms . subs

-- fstOcrnc :: Eq a => a -> [a] -> [a]
-- fstOcrnc e [] = []
-- fstOcrnc e (el:es) = if e == el then es else el : fstOcrnc e es

-- isChoice :: Eq a => [a] -> [a] -> Bool
-- isChoice [] as = True
-- isChoice (e:es) as = if elem e as then isChoice es (fstOcrnc e as) else False

split :: [a] -> [([a], [a])]
split [] = []
split [a] = [([],[a]), ([a],[])]
split (x:xs) = ([],x:xs): ([x], xs) : [(x:rl,rs ) | (rl,rs) <- split xs]

putStr' :: String -> IO ()
putStr' ls = sequence_ [putChar c | c <- ls]

pS :: IO ()
pS = putStr "Hello, Haskell coders!!"

--type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show num)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard' :: Board -> IO ()
putBoard' [] = return ()
putBoard' (r:rs) = do putRow (length (r:rs)) r
                      putBoard' rs

putBoard'' :: Board -> IO ()
putBoard'' rs = sequence_ [putRow n r | (n, r) <- zip [1..(length rs)] rs]

div2 :: Int -> [a] -> ([a], [a])
div2 n as = (take n as, drop n as)

split' :: [a] -> [([a], [a])]
split' as = [div2 n as |  n <- [0..(length as)]]

split'' :: [a] -> [([a], [a])]
split'' as = [div2 n as |  n <- [1..(length as - 1)]]

data Op = Add | Sub | Mul | Div deriving Ord
data Expr = Val Int | App Op Expr Expr

instance Eq Op where
  Add == Add = True
  Sub == Sub = True
  Mul == Mul = True
  Div == Div = True
  _ == _ = False

instance Eq Expr where
  Val i == Val j = i == j
  App o l r == App o' l' r' = o == o' && l == l' && r == r'
  _ == _ = False

instance Ord Expr where
  Val i < Val j = i < j
  App o l r < App o' l' r' = do let nop = numOp (App o l r)
                                let nop' = numOp (App o' l' r')
                                if snd (nop !! 3) < snd (nop' !! 3) then
                                  if snd (nop !! 2) < snd (nop' !! 2) then
                                    if snd (nop !! 1) < snd (nop' !! 1) then
                                      if snd (nop !! 0) < snd (nop' !! 0) then
                                        True
                                      else False
                                    else False
                                  else False
                                else False
  Val i <= Val j = i <= j
  App o l r <= App o' l' r' = (App o l r < App o' l' r') || (App o l r == App o' l' r')
  Val i > Val j = i > j
  App o l r > App o' l' r' = not (App o l r < App o' l' r')
  Val i >= Val j = i >= j
  App o l r >= App o' l' r' = (App o l r > App o' l' r') || (App o l r == App o' l' r')

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

numOp :: Expr -> [(Op, Int)]
numOp (Val _) = []
numOp r = [(o, nOp o r) | o <- ops]

nOp :: Op -> Expr -> Int
nOp _ (Val _) = 0
nOp o' (App o l r) = if o == o' then 1 + (nOp o' l) + (nOp o' r) else (nOp o' l) + (nOp o' r)

valid' :: Op -> Int -> Int -> Bool
valid' Add _ _ = True
valid' Sub x y = x > y
valid' Mul _ _ = True
valid' Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid' o x y]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where yss = subs xs

interleave' :: a -> [a] -> [[a]]
interleave' x [] = [[x]]
interleave' x (y:ys) = (x:y:ys) : map (y:) (interleave' x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave' x) (perms xs))

choices' :: [a] -> [[a]]
choices' xs = [ xs'' | a <- xs', xs'' <- perms a]
  where xs' = subs xs

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split' ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices' ns, e <- exprs ns', eval e == [n]]

qsort :: [(Int, Expr)] -> [(Int, Expr)]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [(n, e) | (n, e) <- xs, n <= fst x]
    larger = [(m, p) | (m, p) <- xs, m > fst x]

qsort' :: Ord a => [a] -> [a]
qsort' [] = []
qsort' (a:as) = qsort' smaller ++ [a] ++ qsort' larger
  where
    smaller = [b | b <- as, b <= a]
    larger = [c | c <- as, c > a]

--------------------------------------30/01/2020


bestmove' :: Grid -> Player -> [Grid]
bestmove' g p = [g' | Node (g', p') _ <- ts, p' == best]
  where
    tree = prune depth (gametree g p)
    Node (_, best) ts = minimax tree

randBestMove :: Grid -> Player -> IO Grid
randBestMove g p = do
  n <- (randomRIO (0, length bm - 1))
  return $ bm !! n
    where
      bm = bestmove' g p

quickstMove :: Int -> Grid -> Player -> Grid
quickstMove depth g p = bestmove g p
quickstMove i g p = if p == p' then g' else quickstMove (i+1) g p
  where
    tree = prune i (gametree g p)
    Node (_, p') ts = minimax tree
    ts' = [(h,k) | Node (h,k) _ <- ts]
    g' = head (map  fst (filter (\t -> (snd t) == p') ts'))

quickstMove' :: Int -> Grid -> Player -> Grid
quickstMove' depth g p = bestmove g p
quickstMove' i g p = if p == p' then g' else quickstMove (i+1) g p
  where
    tree = prune i (gametree g p)
    Node (_, p') ts = minimax' tree
    ts' = [(h,k) | Node (h,k) _ <- ts]
    g' = head (map  fst (filter (\t -> (snd t) == p') ts'))


play3' :: Grid -> IO ()
play3' g = do
             n <- randomRIO (0,8)
             let g' = head (move g n X)
             play3 g' O

play4'' :: Grid -> IO ()
play4'' g = do
              n <- randomRIO (0,8)
              let g' = head (move g n X)
              play4 g' O


play4 :: Grid -> Player -> IO ()
play4 g p = do cls
               goto (1,1)
               putGrid g
               play4' g p

play4' :: Grid -> Player -> IO ()
play4' g p
  | wins g O = putStrLn "Player O wins."
  | wins g X = putStrLn "Player X wins."
  | full g = putStrLn "It's a draw"
  | p == O = do i <- getNat (prompt p)
                case move g i p of
                  [] -> do putStrLn "ERROR: Invalid move."
                           play4 g p
                  [g'] -> play4 g' (next' p)
  | p == X = do putStrLn "Player x is thinking..."
                (play4 $! (bmove g p)) (next' p)

bmove :: Grid -> Player -> Grid
bmove g p = head [g' | Node (g', p') _ <- ts, p' == best]
  where
    tree = prune depth (gametree g p)
    Node (_, best) ts = minimax' tree


defPlay :: IO ()
defPlay = do
            putStrLn "Do you wanna go first? y/n"
            d <- getChar
            case d of
              'y' -> play3 empty O
              'n' -> play4'' empty

------------------------------------------21/02/2020

data Tree' a = Lf a | Nd (Tree' a) (Tree' a) deriving Show

tree' :: Tree' Char
tree' = Nd (Nd (Lf 'a') (Lf 'b')) (Lf 'c')

type State = Int

newtype ST a = S (State -> (a,State))

app :: ST a -> State -> (a,State)
app (S st) x = st x

instance Functor ST where
  fmap g st = S(\s -> let (x, s') = app st s in (g x, s'))

instance Applicative ST where
  pure x = S(\s -> (x, s))
  stf <*> stx = S(\s ->
                    let (f, s') = app stf s
                        (x, s'') = app stx s' in (f x, s''))

instance Monad ST where
  st >>= f = S(\s -> let (x, s') = app st s in app (f x) s')

fresh :: ST Int
fresh = S(\n -> (n, n+1))

alabel :: Tree' a -> ST (Tree' Int)
alabel (Lf _) = Lf <$> fresh
alabel (Nd l r) = Nd <$> alabel l <*> alabel r

mlabel :: Tree' a -> ST (Tree' Int)
mlabel (Lf _) = do n <- fresh
                   return (Lf n)
mlabel (Nd l r) = do l' <- mlabel l
                     r' <- mlabel r
                     return (Nd l' r')

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' f [] = return []
filterM' f (x:xs) = f x >>= (\b -> (filterM' f xs) >>= (\ys -> return (if b then x:ys else ys)))

--instance Monad [] where
  -- (>>=) :: [a] -> (a -> [b]) -> [b]
--  xs >>= f = [y | x <- xs, y <- f x]

--12.5 1

data TREE a = LEF | ND (TREE a) a (TREE a) deriving Show

instance Functor TREE where
  fmap _ LEF = LEF
  fmap f (ND l x r) = ND (fmap f l) (f x) (fmap f r)


--12.5 2

data (->>) a b = R(a -> b)

instance Functor ((->>) a) where
  fmap = (.)

--12.5 4

newtype ZipList' a = Z [a] deriving Show

instance Functor ZipList' where
  fmap g (Z xs) = Z $ map g xs

instance Applicative ZipList' where
  pure x = Z $ repeat x
  (Z gs) <*> (Z xs) = Z [g x | (g, x) <- zip gs xs]

--12.5 7

data Expr' a = Var' a | Val' Int | Add' (Expr' a) (Expr' a) deriving Show

instance Functor Expr' where
  fmap g (Var' v) = Var' $ g v
  fmap _ (Val' i) = Val' i
  fmap g (Add' l r) = Add' (fmap g l)  (fmap g r)

instance Applicative Expr' where
  pure x = Var' x
  (Var' f) <*> x = fmap f x
  (Val' i) <*> x = Val' i
  (Add' l r) <*> (Add' l' r') = Add' (l <*> l') (r <*> r')
  (Add' l r) <*> x = Add' (l <*> x) (r <*> x)

instance Monad Expr' where
  (Var' x) >>= f = f x
  (Val' i) >>= _ = Val' i
  (Add' l r) >>= f = Add' (l >>= f) (r >>= f)

-- data Maybe a = N | J a

-- data Reader a b = {runreader :: a -> b}
-- type ((->) a) b = \a -> b


-- fmap :: (x -> y) -> ((->) a) x -> ((->) b)
-- fmap = (.) 

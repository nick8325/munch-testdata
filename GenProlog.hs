module Main where

import Test.QuickCheck
import Test.QuickCheck.Gen
import System.Random

data Expr =
    Space Expr
  | Var Char
  | Fun Char [Expr]
  | Brack Expr

instance Show Expr where
  showsPrec _ (Space e) = (' ':) . shows e
  showsPrec _ (Var x) = (x:)
  showsPrec _ (Fun f []) = (f:)
  showsPrec _ (Fun f xs) = (f:) . brack (showsList xs)
    where showsList [x] = shows x
          showsList (x:xs) = shows x . (',':) . showsList xs
  showsPrec _ (Brack e) = brack (shows e)

brack f = ('(':) . f . (')':)

var = elements ['A'..'Z']
fun = elements ['a'..'z']

genExpr = sized $ \n -> frequency
  [(1, fmap Var var),
   (1, fmap Space genExpr),
   (1, fmap Brack genExpr),
   (if n > 0 then 3 else 0, resize (n `div` 2) genFun)]

genFun = do
  f <- fun
  len <- frequency [(2^(i `div` 2), return (10-i)) | i <- [0..10]]
  args <- vectorOf len genExpr
  return (Fun f args)

main = do
  g <- newStdGen
  mapM_ (putStr . show) (unGen (vectorOf 10000 genExpr) g 100)

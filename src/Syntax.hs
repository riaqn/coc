module Syntax where

type Index = Int

data Term where
  Box :: Term
  Star :: Term
  Var :: Index -> Term
  Pi :: Term -> Term -> Term
  Jux :: Term -> Term -> Term
  Lambda :: Term -> Term -> Term
  deriving (Eq)

instance Show Term where
  show t = case t of
    Box -> "#"
    Star -> "*"
    Var i -> show i
    Pi t0 t1 -> "pi " ++ show t0 ++ "." ++ show t1
    Jux t0 t1 -> show t1 ++ " " ++ show t0
    Lambda t0 t1 -> "lambda " ++ show t0 ++ "." ++ show t1

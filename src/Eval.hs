module Eval where

import Syntax

eval :: Term -> Term
eval t = case t of
  Pi t0 t1 -> Pi (eval t0) (eval t1)
  Jux t0 t1 -> case eval' t0 of
    Lambda t00 t01 -> eval $ shift (-1) $ subst 0 (shift 1 t1) t01
    t0' -> Jux t0' (eval t1)
  Lambda t0 t1 -> Lambda t0 (eval t1)
  _ -> t

-- reduce until next reduce will occur outside
-- i.e. is a Lambda
-- is in normal form
eval' :: Term -> Term
eval' t = case t of
  Pi t0 t1 -> Pi (eval t0) (eval t1)
  Jux t0 t1 -> case eval' t0 of
    Lambda t00 t01 -> eval' $ shift (-1) $ subst 0 (shift 1 t1) t01
    t0' -> Jux t0' (eval t1)
  _ -> t

shift :: Int -> Term -> Term
shift i t = let
  shift' j t = case t of
    Var x -> if x < j then t else Var (x + i)
    Lambda t0 t1 -> Lambda (shift' j t0) (shift' (j + 1) t1)
    Jux t0 t1 -> Jux (shift' j t0) (shift' j t1)
    Pi t0 t1 -> Pi (shift' j t0) (shift' (j + 1) t1)
    _ -> t
  in
    shift' 0 t

subst :: Int -> Term -> Term -> Term
subst x s t = let
  subst' j t = case t of
    Var y -> if y == x + j then shift j s else t
    Lambda t0 t1 -> Lambda (subst' j t0) (subst' (j + 1) t1)
    Jux t0 t1 -> Jux (subst' j t0) (subst' j t1)
    Pi t0 t1 -> Pi (subst' j t0) (subst' (j + 1) t1)
    _ -> t
  in
    subst' 0 t

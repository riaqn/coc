module Check where

import Syntax
import Utils
import Eval


data CheckError t =
  NotOfSort t t
  | OutOfBound Index
  | TypeNotMatch t t t t
  | NotPi t t

isSort :: Term -> Bool
isSort t = case t of
  Box -> True
  Star -> True
  _ -> False
  
type Context = [Term]
check :: Context -> Term -> Either (CheckError Term) Term
check c t = case t of
  Star -> Right Box
  Var i -> case index c i of
    Nothing -> Left $ OutOfBound i
    Just ty -> Right ty
  Jux t0 t1 -> do
    ty0 <- check c t0
    ty1 <- check c t1
    case ty0 of
      Pi t00 t01 -> if eqBeta t00 ty1 then Right t01
                    else Left $ TypeNotMatch t0 t1 ty1 t00
      _ -> Left $ NotPi t0 ty0
  Pi t0 t1 -> do
    ty0 <- check c t0
    _ <- do
      if isSort ty0 then Right () else Left $ NotOfSort t0 ty0
    ty1 <- check (t0 : c) t1
    _ <- do
      if isSort ty1 then Right () else Left $ NotOfSort t1 ty1
    return ty1
  Lambda t0 t1 -> do
    ty0 <- check c t0
    _ <- do
      if isSort ty0 then Right () else Left $ NotOfSort t0 ty0
    ty1 <- check (t0 : c) t1
    ty1' <- check (t0 : c) ty1
    _ <- do
      if isSort ty1' then Right () else Left $ NotOfSort ty1 ty1'
    Right $ Pi t0 ty1

eqBeta :: Term -> Term -> Bool
eqBeta t0 t1 = (eval t0) == (eval t1)

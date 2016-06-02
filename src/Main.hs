module Main where

import Text.Parsec
import Text.Format

import Parse
import Check
import Eval

main :: IO ()
main = do
  s <- getContents
  case parse parseTerm' "<stdin>" s of
    Left err -> print err
    Right t -> case check [] t of
      Left err -> putStrLn $ case err of
        NotOfSort t0 t1 -> format "{0} is of type {1}, expecting some sort" [show t0, show t1]
        OutOfBound i -> format "Out of bound index {0}" [show i]
        TypeNotMatch t0 t1 ty1 t00 -> format "applying {0} to {1}, which is of type {2}, expect {3}" [show t0, show t1, show ty1, show t00]
        NotPi t ty -> format "{0} is not type {1}, expect Pi" [show t, show ty]
      Right ty -> do
        putStrLn $ "checked type: " ++ show ty
        putStrLn $ "evaled term : " ++ (show $eval t)

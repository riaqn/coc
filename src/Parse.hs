module Parse where

import Text.Parsec
import Text.Parsec.Pos

import Syntax

type Parser = Parsec String ()
parseBox :: Parser Term
parseBox = do
  _ <- char '#'
  return Box

parseStar :: Parser Term
parseStar = do
  _ <- char '*'
  return Box

parseVar :: Parser Term
parseVar = do
  i <- read <$> many1 digit
  return $ Var i

parsePi :: Parser Term
parsePi = do
  i <- string "pi"
  spaces
  t0 <- parseTerm
  spaces
  _ <- char '.'
  t1 <- parseTerm
  return $ Pi t0 t1

parseLambda :: Parser Term
parseLambda = do
  i <- string "lambda"
  spaces
  t0 <- parseTerm
  spaces
  _ <- char '.'
  t1 <- parseTerm
  return $ Lambda t0 t1

parseNonJux :: Parser Term
parseNonJux = (do
                  _ <- char '('
                  spaces
                  t <- parseTerm
                  spaces
                  _ <- char ')'
                  return t)
  <|> parseLambda
  <|> parsePi
  <|> parseVar
  <|> parseBox
  <|> parseStar

parseTerm :: Parser Term
parseTerm = do
  chainl1 (do
              t <- parseNonJux
              spaces
              return t
          ) (return $ \p0 p1 -> Jux p1 p0)


parseTerm' :: Parser Term
parseTerm' = do
  spaces
  parseTerm


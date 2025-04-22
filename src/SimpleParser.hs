{-# LANGUAGE FlexibleContexts #-}

module SimpleParser where

import Text.ParserCombinators.Parsec
import Language.Haskell.TH.Syntax

{-
 simplyfiled Haskell type expression syntax (without ->)

 type  -> type atype
 atype -> conid | varid | ( type_1, ..., type_n ) | [ type ] | ( type ) 
-}

-- cf) https://wiki.haskell.org/index.php?title=Quasiquotation
lexeme p     = do { x <- p; spaces; return x  }
symbol name  = lexeme (string name)
keywords = ["data", "deriving"]
conid = try (lexeme $ do
  c <- upper
  cs0 <- many (letter <|> digit)
  let cs = c:cs0
  if cs `elem` keywords
     then fail ("reserved word: " ++ cs)
     else pure cs)
varid = try (lexeme $ do
  c <- lower <|> char '_'
  cs0 <- many (letter <|> digit)
  let cs = c:cs0
  if cs `elem` keywords
     then fail ("reserved word: " ++ cs)
     else pure cs)

atypeParser :: Parser Type
atypeParser =
           do { c <- conid; 
                pure $ ConT $ mkName c }
       <|> do { v <- varid; pure $ VarT $ mkName v }
       <|> do { symbol "["; t <- typeParser; symbol "]"; pure $ AppT ListT t }
       <|> do { symbol "("
              ; ts <- sepBy1 typeParser (symbol ",")
              ; symbol ")"
              ; let len = length ts
              ; if len == 1 then
                    pure $ head ts
                else
                    pure $ foldl AppT (TupleT (length ts)) ts
              }

typeParser :: Parser Type
typeParser = do
  t <- atypeParser
  ts <- many atypeParser
  pure $ foldl AppT t ts

-- dec :: Parser Dec
dec = do
  symbol "data"
  n <- typeParser
  symbol "<-"
  ts <- sepBy1 typeParser (symbol "|")
  symbol "deriving"
  symbol "("
  cs <- sepBy1 typeParser (symbol ",")
  symbol ")"
  pure (n, ts, cs)

parseCarrefourDec :: MonadFail m => (String, Int, Int) -> String -> m (Type, [Type], [Type])
parseCarrefourDec (file, line, col) s = do
  case runParser p () "" s of
    Left err -> fail (show err)
    Right d  -> pure d
  where
    p = do pos <- getPosition
           setPosition $
              flip setSourceName file $
              flip setSourceLine line $
              flip setSourceColumn col $
              pos
           spaces
           d <- dec
           eof
           return d
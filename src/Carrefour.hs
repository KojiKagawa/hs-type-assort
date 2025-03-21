{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleContexts #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}

module Carrefour (typeCarrefour, carrefour) where
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
-- import Language.Haskell.Exts.Syntax
-- import Language.Haskell.Exts.Parser
-- import Language.Haskell.Meta.Syntax.Translate
-- import Language.Haskell.Meta.Parse
-- import Language.Haskell.Syntax
import Text.ParserCombinators.Parsec

typeFArgs :: MonadFail m => Type -> m (Type, [Type])
typeFArgs (AppT t1 t2)     = do
  (n, ts) <- typeFArgs t1
  pure (n, ts ++ [t2])
typeFArgs (InfixT t1 n t2) = pure (ConT n, [t1, t2])
typeFArgs t                = pure (t, [])

typeToList :: MonadFail m => Type -> m [Type]
typeToList typ = do
  (n, ts) <- typeFArgs typ
  if n == TupleT (length ts)
    then pure ts
    else fail "typeToList: not a tuple"
  pure $ n : ts

dataHead :: Type -> Q (Name, [TyVarBndr ()])
dataHead typ = do
  (ConT n, ts) <- typeFArgs typ
  let tvs = map (\ (VarT n) -> n) ts
  pure (n, map (`PlainTV` ()) tvs)

substType :: [(Name, Type)] -> Type -> Type
substType env (VarT n) = case lookup n env of
  Just t  -> t
  Nothing -> VarT n
substType env (AppT t1 t2) = AppT (substType env t1) (substType env t2)
substType env (ConT n) = ConT n
substType env (TupleT n) = TupleT n
substType env ListT = ListT
substType env t = t      --- 他のコンストラクタは使われないはず
-- substType env ArrowT = ArrowT

instanceInfo :: Type -> Type -> Q ()
instanceInfo d c = do
  (ConT n, ts) <- typeFArgs c
  -- runIO $ print n
  -- runIO $ print d
  let ts1 = map (substType [(mkName "_Self", d)]) ts
  insts <- reifyInstances n ts1
  -- runIO $ print insts
  runIO $ putStrLn $ pprint insts

typeCarrefour :: Type -> [Type] -> [Type] -> Q [Dec]
typeCarrefour typ ds cs = do
  (n, tvs) <- dataHead typ
  let dcs =  [(d0, c0) | d0 <- ds, c0 <- cs]
  mapM_ (uncurry instanceInfo) dcs
  let dec = DataD [] n tvs
            Nothing [NormalC n [(Bang NoSourceUnpackedness NoSourceStrictness, TupleT 0)]] []
  runIO $ putStrLn $ pprint dec
  pure [dec]

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
           do { c <- conid; pure $ ConT $ mkName c }
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
              (flip setSourceName) file $
              (flip setSourceLine) line $
              (flip setSourceColumn) col $
              pos
           spaces
           d <- dec
           eof
           return d

carrefourDec :: String -> Q [Dec]
carrefourDec s = do
  loc <- TH.location
  let pos = (TH.loc_filename loc, fst (TH.loc_start loc), snd (TH.loc_start loc))
  (t, ds, cs) <- parseCarrefourDec pos s
  typeCarrefour t ds cs
  -- runIO $ print dec
  pure []

-- for test only
carrefourExp :: String -> Q Exp
carrefourExp s = do
  pure $ LitE (StringL "hello")

carrefour = QuasiQuoter { 
                          quoteExp  = carrefourExp,
                          quotePat  = undefined,
                          quoteType = undefined,
                          quoteDec  = carrefourDec
                        }

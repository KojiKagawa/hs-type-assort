{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Use <$>" #-}

module Carrefour where

-- https://hackage.haskell.org/package/template-haskell-2.19.0.0/docs/Language-Haskell-TH.html
import Language.Haskell.TH as TH
-- https://hackage.haskell.org/package/template-haskell-2.19.0.0/docs/Language-Haskell-TH-Syntax.html
import Language.Haskell.TH.Syntax
-- https://hackage.haskell.org/package/template-haskell-2.19.0.0/docs/Language-Haskell-TH-Quote.html
import Language.Haskell.TH.Quote

import FMap
import SimpleParser (parseCarrefourDec)
import MyTypeLib
import Data.List (nub, (\\))



getClassInfo :: Type -> Q (Name, [Dec])
getClassInfo c = do
  (ConT n, ts) <- typeFArgs c
  ClassI (ClassD cxt _ tvs deps ms) _ <- reify n  -- instance info is not necessary here.
  let subst = map getNameTyVar tvs `zip` ts
  let ms1 = map (substSig subst) ms
  -- let cxt1 = map (substType subst) cxt
  -- let ret = ClassD cxt1 n tvs [{-deps-}] ms1 
  -- runIO $ putStrLn $ pprint ms1
  pure(n, ms1)

mkNames :: Int -> String -> [Name]
mkNames n s = map (mkName . (s ++) . show) [1..n]

defineAllData :: Name -> [TyVarBndr ()] -> [Name] -> [Type] -> Q [Dec]
defineAllData tyconName tvs conNames ds = do
  let conDecls = zipWith (\ n t
        -> NormalC n [(Bang NoSourceUnpackedness NoSourceStrictness, t)]) conNames ds
  let dec = DataD [] tyconName tvs Nothing conDecls []
  runIO $ putStrLn $ pprint dec
  -- Todo: to 関数の定義とインスタンス宣言を作る
  pure [dec]


-- Self -> Self -> t    ===> (2, t)
-- Self -> Self -> Self -> t ===> (3, t)
countSelfArgs :: Name -> Type -> (Int, Type)
countSelfArgs n (AppT (AppT ArrowT (VarT m)) t2)
    | m == n = let (k, t) = countSelfArgs n t2 in (k + 1, t)
    | otherwise = (0, AppT (AppT ArrowT (VarT m)) t2)
countSelfArgs _ t = (0, t)

lookIntoMethod :: Dec -> Q [Dec]
lookIntoMethod (SigD n t) = do
  runIO $ putStr $ pprint n
  runIO $ putStr " :: "
  runIO $ putStrLn $ pprint t
  pure []
lookIntoMethod _ = fail "lookIntoMethod: not a signature declaration"

mkPatterns :: Int -> [Name] -> [[Pat]]
mkPatterns n cs = aux n n cs
  where
    aux :: Int -> Int -> [Name] -> [[Pat]]
    aux 0 _ _  = [[]]
    aux i n cs =
      [ p:ps | ps <- aux (i - 1) n cs
             , p  <- map (\ c -> ConP c [] [VarP $ mkName ("x" ++ show i)]) cs ]

applyFMap :: Maybe (Maybe Exp) -> Exp -> Q Exp
applyFMap Nothing _ = fail "applyFMap: cannot define fmap"
applyFMap (Just Nothing) e = pure e
applyFMap (Just (Just e)) e1 = pure $ AppE e e1

defineMethod :: Type -> [Name] -> Dec -> Q [Dec]
defineMethod typ cstrs (SigD n t) = do
  let (num1, t1) = countSelfArgs (mkName "_Self") t
  fmapFn <- mkFMap (mkName "_Self") t1 (VarE $ mkName "inj")
  ret <- applyFMap fmapFn $ foldl AppE (VarE n) (map VarE (mkNames num1 "x"))
  let pats = mkPatterns num1 cstrs
      ds = map (\ ps -> FunD n [Clause ps {- Body -}(NormalB ret) [{-Dec-}]]) pats
  -- runIO $ putStrLn $ pprint ds
  pure ds
defineMethod _ _ _  = pure []

-- temporary
getInstanceCxt :: Type -> Type -> Q Cxt
getInstanceCxt d c = do
  (ConT n, ts) <- typeFArgs c
  -- runIO $ print n
  -- runIO $ print d
  -- Todo: binary method に対応するため _Self1, _Self2, ... のすべての組み合わせを考える
  let ts1 = map (substType [(mkName "_Self", d)]) ts
      c1  = foldl AppT (ConT n) ts1
  insts <- reifyInstances n ts1
  -- runIO $ print insts
  -- runIO $ putStrLn $ show c ++ ", " ++ show c1
  -- runIO $ putStrLn $ pprint insts
  case insts of
    [] -> fail $ "getInstanceCxt: no instance for " ++ pprint c1
    [InstanceD Nothing cxt t decs]  -> do 
      s <- unifyType t c1
      pure $ map (substType s) cxt
    other -> pure $ [c1] -- Overlapping している場合、元の Pred を使う

defineInstance :: Type -> [Type] -> [Name] -> [Type] -> (Type, (Name, [Dec])) -> Q [Dec]
defineInstance typ cs cstrs ds (c, (n, ms)) = do
  -- decs <- mapM lookIntoMethod ms
  -- runIO $ putStrLn $ pprint $ concat decs
  -- Todo: _Self1, _Self2, ... のすべての組み合わせを考える
  -- 考え方 _Self が一種類のとき（binary method がないとき）
  --          _Self にすべての型を当てはめる
  --       _Self が複数のとき（binary method 以上があるとき）
  --          _Self1, _Self2, ... というように、独立な型変数に附番する
  --          従属な型変数は、_Self を使う
  --          インスタンスは _Self1, _Self2, ... に対してすべての組み合わせを考える
  cxtss <- mapM (flip getInstanceCxt c) ds
  mdecs <- mapM (defineMethod typ cstrs) ms
  let s = [(mkName "_Self", typ)]
      cxts = nub (map (substType s) (concat cxtss)) \\ cs
      ret = InstanceD Nothing cxts (substType s c) (concat mdecs)
  runIO $ putStrLn $ pprint ret
  pure [ret]

typeCarrefour :: Type -> [Type] -> [Type] -> Q [Dec]
typeCarrefour typ ds cs = do
  (n, tvs) <- dataHead typ
  let nBase = nameBase n
  let tyconName = mkName nBase
  let consts  = mkNames (length ds) nBase
  let s = [(mkName "_Self", typ)]
  let ds1 = map (substType s) ds
  runIO $ putStrLn "---- defineAllData"
  dataDec <- defineAllData tyconName tvs consts ds1 
  -- runIO $ putStrLn "---- getClassInfo"
  cis <- mapM getClassInfo cs
  runIO $ putStrLn "---- defineInstance"
  let cs1 = map (substType s) cs
  mapM_ (defineInstance typ cs1 consts ds) $ zip cs cis
  pure dataDec

replaceConst :: Type -> Q Type
replaceConst (ConT n) = do
  mn <- lookupTypeName $ nameBase n
  case mn of
    Just n1 -> pure $ ConT n1
    _ -> pure $ ConT n
replaceConst (AppT t1 t2) = do
  t1' <- replaceConst t1
  t2' <- replaceConst t2
  pure $ AppT t1' t2'
replaceConst t  = pure t

carrefourDec :: String -> Q [Dec]
carrefourDec s = do
  loc <- TH.location
  let pos = (TH.loc_filename loc, fst (TH.loc_start loc), snd (TH.loc_start loc))
  (t, ds, cs) <- parseCarrefourDec pos s
  t1 <- replaceConst t
  ds1 <- mapM replaceConst ds 
  cs1 <- mapM replaceConst cs
  typeCarrefour t1 ds1 cs1
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

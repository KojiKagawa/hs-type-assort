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

getInstanceInfo :: Type -> Type -> Q ()
getInstanceInfo d c = do
  (ConT n, ts) <- typeFArgs c
  -- runIO $ print n
  -- runIO $ print d
  let ts1 = map (substType [(mkName "_Self", d)]) ts
  insts <- reifyInstances n ts1
  -- runIO $ print insts
  runIO $ putStrLn $ pprint insts

getClassInfo :: Type -> Q Dec
getClassInfo c = do
  (ConT n, ts) <- typeFArgs c
  ClassI (ClassD cxt _ tvs deps ms) _ <- reify n  -- instance info is not necessary here.
  let subst = map getNameTyVar tvs `zip` ts
  let ms1 = map (substSig subst) ms
  let cxt1 = map (substType subst) cxt
  let ret = ClassD cxt1 n tvs [{-deps-}] ms1 -- Todo: substitute tvs
  runIO $ putStrLn $ pprint ret
  pure ret

mkNames :: Int -> String -> [Name]
mkNames n s = map (mkName . (s ++) . show) [1..n]

defineAllData :: Name -> [TyVarBndr ()] -> [Name] -> [Type] -> Q [Dec]
defineAllData tyconName tvs conNames ds = do
  let conDecls = zipWith (\ n t
        -> NormalC n [(Bang NoSourceUnpackedness NoSourceStrictness, t)]) conNames ds
  let dec = DataD [] tyconName tvs Nothing conDecls []
  runIO $ putStrLn $ pprint dec
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
defineMethod typ cs (SigD n t) = do
  let (num1, t1) = countSelfArgs (mkName "_Self") t
  fmapFn <- mkFMap (mkName "_Self") t1 (VarE $ mkName "inj")
  ret <- applyFMap fmapFn $ foldl AppE (VarE n) (map VarE (mkNames num1 "x"))
  let pats = mkPatterns num1 cs
      ds = map (\ ps -> FunD n [Clause ps {- Body -}(NormalB ret) [{-Dec-}]]) pats
  -- runIO $ putStrLn $ pprint ds
  pure ds
defineMethod _ _ _ = pure []

defineInstance :: Type -> [Name] -> Dec -> Q [Dec]
defineInstance typ cs (ClassD cxt n tvs deps ms) = do
  decs <- mapM lookIntoMethod ms
  -- runIO $ putStrLn $ pprint $ concat decs
  mdecs <- mapM (defineMethod typ cs) ms
  let ret = InstanceD Nothing [{- Todo: Cxt -}] (AppT (ConT n) typ) (concat mdecs)
  runIO $ putStrLn $ pprint ret
  pure [ret]

typeCarrefour :: Type -> [Type] -> [Type] -> Q [Dec]
typeCarrefour typ ds cs = do
  (n, tvs) <- dataHead typ
  let nBase = nameBase n
  let tyconName = mkName nBase
  let conNames  = mkNames (length ds) nBase
  dataDec <- defineAllData tyconName tvs conNames
                          (map (substType [(mkName "_Self", typ)]) ds)
  let dcs =  [(d0, c0) | d0 <- ds, c0 <- cs]
  runIO $ putStrLn "---- getInstanceInfo"
  mapM_ (uncurry getInstanceInfo) dcs
  runIO $ putStrLn "---- getClassInfo"
  cis <- mapM getClassInfo cs
  runIO $ putStrLn "---- defineInstance"
  mapM_ (defineInstance typ conNames) cis
  pure dataDec

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

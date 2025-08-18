{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# HLINT ignore "Use section" #-}

module Carrefour where

-- https://hackage.haskell.org/package/template-haskell-2.20.0.0/docs/Language-Haskell-TH.html
import Language.Haskell.TH as TH
-- https://hackage.haskell.org/package/template-haskell-2.20.0.0/docs/Language-Haskell-TH-Syntax.html
import Language.Haskell.TH.Syntax
-- https://hackage.haskell.org/package/template-haskell-2.20.0.0/docs/Language-Haskell-TH-Quote.html
import Language.Haskell.TH.Quote

import FMap
import SimpleParser (parseCarrefourDec)
import MyTypeLib
import Data.Char (toLower)
import Data.List (nub, (\\))
import Data.Data (Data)
import Data.Maybe (catMaybes, mapMaybe)
import Control.Monad (zipWithM)

class Cast a b where
  cast :: a -> b

instance Cast a a where
  cast = id

data CastClass = CastFrom Name | CastTo Name 
                 deriving (Show, Data)

-- getMethodTypes t
--   t: a type class constraint
--   return: [Dec]
--   --   [Dec]:  types of methods (type vars are replaced with the type args in *t*)
getMethodTypes :: Type -> Q [Dec]
getMethodTypes c = do
  let (ConT n, ts) = typeFArgs c
  ClassI (ClassD cxt _ tvs deps ms) _ <- reify n  -- instance info is not necessary here.
  let subst = map getNameTyVar tvs `zip` ts
      ms1 = map (substSig subst) ms
  -- runIO $ putStrLn $ pprint ms1
  pure ms1

-- makeNames n s
--   n:  number of names to make
--   s:  prefix string for the names, e.g. "s" for "s1", "s2", ...
--   return: [Name] with the prefix string and numbers, e.g. ["s1", "s2", ...]
mkNames :: Int -> String -> [Name]
mkNames n s = map (mkName . (s ++) . show) [1..n]

-- defineToDecls tyconName tvs conNames ds
--   tyconName:  type constructor name, e.g. AllTurtle
--   tvs:        type parameters, e.g. ["s"]
--   conNames:   data constructor names, e.g. ["AllTurtle1", "AllTurtle2", ...]
--   ds:         data types, e.g. [Turtle s, ColorTurtle s, Turtle3D s, TwistedTurtle _Self]
--   return: [Dec] for the To type class and its instances
--   The To type class is defined as follows:
--   1. classDecl
--   class ToAllTurtle _self s | _self -> s where   
--       toAllTurtle :: _self -> AllTurtle s
--   The instances are defined as follows:
--   2. instDecls
--   instance ToAllTurtle (Turtle s) s where
--       toAllTurtle = AllTurtle1
--   ... for other data constructors
--   3. eiDecl
--   instance (ToAllTurtle a s, ToAllTurtle b s) => ToAllTurtle (Either a b) s where
--       toAllTurtle = either toAllTurtle toAllTurtle
--   4. castDecl
--   instance ToAllTurtle a s => Cast a (AllTurtle s) where
--       cast _self = toAllTurtle _self
defineToDecls :: Name -> [Name] -> [Name] -> [Type] -> [Dec]
defineToDecls tyconName tvs conNames ds = let
    toNameL = mkName $ "To" ++ nameBase tyconName
    toNameS = mkName $ "to" ++ nameBase tyconName
    self    = mkName "_self"
    selfv   = PlainTV self BndrReq
    ty      = foldl AppT (ConT tyconName) (map VarT tvs)
    classDecl = ClassD [] toNameL (selfv : map (\ v -> PlainTV v BndrReq) tvs)
                       [FunDep [self] tvs] [SigD toNameS (ArrowT `AppT` VarT self `AppT` ty)]
    instDecls = zipWith (\ n t ->
         InstanceD Nothing [] (foldl AppT (ConT toNameL) (t : map VarT tvs))
                   [ValD (VarP toNameS) (NormalB (ConE n)) []]) conNames ds
    a = mkName "a"
    b = mkName "b"
    eiDecl =  InstanceD Nothing
                  [foldl AppT (ConT toNameL) (map VarT (a : tvs)), foldl AppT (ConT toNameL) (map VarT (b : tvs))]
                  (foldl AppT (ConT toNameL) (ConT ''Either `AppT` VarT a `AppT` VarT b : map VarT tvs))
                  [ValD (VarP toNameS) (NormalB (VarE 'either `AppE` VarE toNameS `AppE` VarE toNameS)) []]
    castDecl = InstanceD Nothing [foldl AppT (ConT toNameL) (VarT a : map VarT tvs)]
                        (foldl AppT (ConT ''Cast) [VarT a, ty])
                        [FunD 'cast [Clause [VarP self] (NormalB (VarE toNameS `AppE` VarE self)) []]]
  in classDecl : eiDecl : castDecl: instDecls

-- defineFromDecls tyconName tvs conNames ds
--   tyconName:  type constructor name, e.g. AllTurtle  
--   tvs:        type parameters, e.g. ["s"]
--   conNames:   data constructor names, e.g. ["AllTurtle1", "AllTurtle2", ...]
--   ds:         data types, e.g. [Turtle s, ColorTurtle s, Turtle3D s, TwistedTurtle _Self]
--   return: [Dec] for the From type class and its instances
--   The From type class is defined as follows:
--   1. classDecl
--   class FromAllTurtle _self s | _self -> s where
--       fromAllTurtle :: AllTurtle s -> Maybe _self
--   The instances are defined as follows:
--   2. instDecls
--   instance FromAllTurtle (Turtle s) s where
--       fromAllTurtle (AllTurtle1 x) = Just x
--       fromAllTurtle _ = Nothing
--   ... for other data constructors
defineFromDecls :: Name -> [Name] -> [Name] -> [Type] -> [Dec]
defineFromDecls tyconName tvs conNames ds = let
    fromNameL = mkName $ "From" ++ nameBase tyconName
    fromNameS = mkName $ "from" ++ nameBase tyconName
    self    = mkName "_self"
    selfv   = PlainTV self BndrReq
    ty      = foldl AppT (ConT tyconName) (map VarT tvs)
    classDecl = ClassD [] fromNameL (selfv : map (\ v -> PlainTV v BndrReq) tvs)
                       [FunDep [self] tvs]
                       [SigD fromNameS (ArrowT `AppT` ty `AppT` (ConT ''Maybe `AppT` VarT self))]
    instDecls = zipWith (\ n t ->
        let x = mkName "x" in
          InstanceD Nothing [] (foldl AppT (ConT fromNameL) (t : map VarT tvs))
                    [FunD fromNameS [ Clause [ConP n [] [VarP x]] (NormalB (ConE 'Just `AppE` VarE x)) []
                                    , Clause [WildP] (NormalB (ConE 'Nothing)) [] ]]) conNames ds
  in classDecl : instDecls

{-
-- Self 型を考慮に入れる必要がある
defineCastClass' :: Name -> Int -> [Dec]
defineCastClass' n nArgs =
    let self    = mkName "_self"
        selfv   = PlainTV self BndrReq
        n0 = nameBase n
        fcStr  = "From" ++ n0
        fmStr  = "from" ++ n0
        fcName = mkName fcStr
        fmName = mkName fmStr
        vs0 = map (\ i -> PlainTV (mkName ("s" ++ show i)) BndrReq) [1..nArgs]
        vs = map (\ i -> VarT $ mkName ("s" ++ show i)) [1..nArgs]
        t = foldl AppT (ConT n) vs
        fDecl = ClassD [] fcName (selfv : vs0) [] [
                SigD fmName (AppT (AppT ArrowT t) (VarT self))
            ]
        tcStr = "To" ++ n0
        tmStr = "to" ++ n0
        tcName = mkName tcStr
        tmName = mkName tmStr
        tDecl = ClassD [] tcName (selfv : vs0) [] [
                SigD tmName (AppT (AppT ArrowT (VarT self)) (ConT ''Maybe `AppT` t))
            ]
        fcExp = VarE 'mkName `AppE` LitE (StringL fcStr)
        tcExp = VarE 'mkName `AppE` LitE (StringL tcStr)
        annDecl = PragmaD (AnnP (TypeAnnotation n) (AppE (AppE (ConE ''CastClass) fcExp) tcExp))
      in [fDecl, tDecl, annDecl]

defineCastClass :: Name -> Q [Dec]
defineCastClass n = do
    info <- reify n
    case info of
      TyConI (DataD _ _ vs _ _ _) -> do
        let nArgs = length vs
        pure $ defineCastClass' n nArgs
      TyConI (NewtypeD _ _ vs _ _ _) -> do
        let nArgs = length vs  -- Newtype has only one constructor
        pure $ defineCastClass' n nArgs
      _ -> pure []
-}

methodName :: Name -> Name
methodName n = mkName $ methodName' (nameBase n)
  where
    methodName' "" = ""
    methodName' (x:xs) = toLower x : xs

defineCastInstance :: CastClass -> Name -> [Name] -> Name -> Type -> [Dec]
defineCastInstance (CastFrom fcName) tyconName tvs conName t =
  let (_, vs) = typeFArgs t
      self = mkName "_self"
      finstDecl = InstanceD Nothing [] (foldl AppT (ConT fcName) (VarT self : vs)) [
              FunD (methodName fcName) [
                  Clause [VarP self] (NormalB (ConE conName `AppE` VarE self)) []
              ]
          ]
      {-
      tinstDecl = InstanceD Nothing [] (foldl AppT (ConT tcName) (VarT self: vs)) [
              FunD (methodName tcName) [
                  Clause [ConP conName [] [VarP self]] (NormalB (ConE 'Just `AppE` VarE self)) []
                , Clause [WildP] (NormalB (ConE 'Nothing)) [] 
              ]
          ]
      -}
    in [finstDecl]

-- defineCastDecl 
defineCastDecl :: Name -> [Name] -> Name -> Type -> Q [Dec]
defineCastDecl tyconName tvs conName t = do
  let (ConT n, vs) = typeFArgs t
  cs <- ((reifyAnnotations (AnnLookupName n)) :: Q [CastClass])
  -- let (decls, cs') = case cs of
  --       [] -> let classDecls = defineCastClass' n (length vs)
  --                 fcName = mkName $ "From" ++ nameBase n
  --                 tcName = mkName $ "To" ++ nameBase n
  --               in (classDecls, [CastClass { from = fcName, to = tcName }])
  --       cs -> ([], cs)
  let ds = concatMap (\ c -> defineCastInstance c tyconName tvs conName t) cs
  pure ds
  
defineCastDecls :: Name -> [Name] -> [Name] -> [Type] -> Q [Dec]
defineCastDecls tyconName tvs conNames ds = do
   dss <- zipWithM (defineCastDecl tyconName tvs) conNames ds
   return $ concat dss

-- defineAllSumData tyconName tvs conNames ds
--   tyconName:  type constructor name, e.g. AllTurtle
--   tvs:        type parameters, e.g. ["s"]
--   conNames:   data constructor names, e.g. ["AllTurtle1", "AllTurtle2", ...]
--   ds:         data types, e.g. [Turtle s, ColorTurtle s, Turtle3D s, TwistedTurtle _Self]
defineAllSumData :: Name -> [Name] -> [Name] -> [Type] -> Q [Dec]
defineAllSumData tyconName tvs conNames ds = do
  let conDecls = zipWith (\ n t
        -> NormalC n [(Bang NoSourceUnpackedness NoSourceStrictness, t)]) conNames ds
  let dec = DataD [] tyconName (map (\ v -> PlainTV v BndrReq) tvs) Nothing conDecls []
  -- runIO $ putStrLn $ pprint dec
      toDecls   = defineToDecls tyconName tvs conNames ds
      fromDecls = defineFromDecls tyconName tvs conNames ds
      ret = dec : toDecls ++ fromDecls
--  runIO $ putStrLn $ pprint ret
  casts <- defineCastDecls tyconName tvs conNames ds
  pure (ret ++ casts)

-- countSelfArgs n t
--   n:  name of the type variable for Self, e.g. "_Self"
--   t:  type expression, e.g. "_Self -> _Self -> t"
-- Self -> Self -> t    ===> (2, t)
-- Self -> Self -> Self -> t ===> (3, t)
countSelfArgs :: Name -> Type -> (Int, Type)
countSelfArgs n (AppT (AppT ArrowT (VarT m)) t2)
    | m == n = let (k, t) = countSelfArgs n t2 in (k + 1, t)
    | otherwise = (0, AppT (AppT ArrowT (VarT m)) t2)
countSelfArgs _ t = (0, t)

{-
lookIntoMethod :: Dec -> Q [Dec]
lookIntoMethod (SigD n t) = do
  runIO $ putStr $ pprint n
  runIO $ putStr " :: "
  runIO $ putStrLn $ pprint t
  pure []
lookIntoMethod _ = fail "lookIntoMethod: not a signature declaration"
-}

-- mkPatterns n cs
--   n:  number of arguments, e.g. 2
--   cs: data constructor names, e.g. ["C1", "C2", "C3"]
--  return: [[Pat]] for the patterns, e.g. [[C1 x1, C1 x2], [C1 x1, C2 x2], ..., [C3 x1, C2 x2], [C3 x1, C3 x2]]
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

-- defineMethod nm cstrs (SigD n t)
--  nm:     data constructor name, e.g. AllTurtle 
--  cstrs:  data constructor names, e.g. ["C1", "C2", ...]
--  (SigD n t):  method signature, e.g. foo :: _Self -> _Self -> _Self
--  return: a method definition for all the combinations of data constructors
--    --  e.g. foo (C1 x1) (C1 x2) = toAllTurtle (foo x1 x2)
--    --       foo (C1 x1) (C2 x2) = toAllTurtle (foo x1 x2)
--    --       ... for all the combinations of data constructors
defineMethod :: String -> [Name] -> Dec -> Q (Maybe Dec)
defineMethod nBase cstrs (SigD n t) = do
  let (num1, t1) = countSelfArgs (mkName "_Self") t
      {- sb = [ (mkName "__Self", VarT $ mkName "_Self") ] -} -- ここでだけ __Self と _Self を同一視
  fmapFn <- mkFMap (mkName "_Self") ({- substType sb -} t1) (VarE $ mkName ("to" ++ nBase))
  ret <- applyFMap fmapFn $ foldl AppE (VarE n) (map VarE (mkNames num1 "x"))
  let pats = mkPatterns num1 cstrs
      ds = map (\ ps -> Clause ps {- Body -}(NormalB ret) [{-Dec-}]) pats
  -- runIO $ putStrLn $ pprint ds
  pure $ Just (FunD n ds)
defineMethod _ _ _  = pure Nothing

-- クラス n から他に依存している型変数を取得する
getDependentParams :: Name -> Q [Bool]
getDependentParams n = do
  ClassI (ClassD _ _ tvs deps _) _ <- reify n
  let ds = loop deps []
  pure $ map (\ tv -> tyVarName tv `elem` ds) tvs
    -- loop deps acc
    --   deps:  functional dependencies, e.g. [FunDep [a] [b, c], FunDep [b] [c]]
    --   acc:   accumulated dependent parameters
    --   return: a list of dependent parameters
    --   The function finds all the dependent parameters recursively.
    --   e.g. FunDep [a] [b, c] and FunDep [b] [c] will return [a, b, c]
  where
    tyVarName (PlainTV n _)    = n
    tyVarName (KindedTV n _ _) = n
    dependentParams deps = concatMap (\ (FunDep _ ns) -> ns) deps 
                            \\ concatMap(\ (FunDep as _) -> as) deps
    removeParam ns (FunDep as rs)  = 
      let rs' = rs \\ ns
        in if null rs' then Nothing 
                       else Just (FunDep as rs')
    removeDependentParams ns = mapMaybe (removeParam ns)
    loop deps acc = let
      ds = dependentParams deps
      in if null ds then acc
         else
          let deps' = removeDependentParams ds deps
            in loop deps' (acc ++ ds)

-- getInstanceContext d c
--   d:  type expression, e.g. TwistedTurtle _Self
--   c:  class constraint expression, e.g. TurtleLike _Self s
--   return: Cxt (e.g. [Pred]) for the class constraint, e.g. [TurtleLike _Self s]
getInstanceContext :: Type -> Type -> Q Cxt
getInstanceContext d c = do
  let (ConT n, ts) = typeFArgs c
  flags <- getDependentParams n 
  -- runIO $ print n
  -- runIO $ print d
  ts1 <- zipWithM (\ flag t -> if flag then do
                                              n <- newName "__s"
                                              pure $ substType [(mkName "_Self", VarT n)] t
                                            -- 依存している型変数の中の Self は新しい型変数に置き換える
                                       else pure $ substType [(mkName "_Self", d)] t) 
                    flags ts -- ここでは __Self と _Self を同一視しない
  let c1  = foldl AppT (ConT n) ts1
  insts <- reifyInstances n ts1
  -- runIO $ print insts
  -- runIO $ putStrLn $ show c ++ ", " ++ show c1
  -- runIO $ putStrLn $ pprint insts
  case insts of
    [] -> fail $ "getInstanceContext: no instance for " ++ pprint c1
    [InstanceD Nothing cxt t decs]  -> do
      s <- unifyType t c1
      pure $ map (substType s) cxt
    other -> pure [c1] -- Overlapping している場合、元の Pred を使う

-- Either の使用に対応できているはず
-- defineInstance typ nbase cs cstrs ds (c, ms)
--  typ:      type expression, e.g. AllTurtle s
--  nBase:    base name for the type, e.g. "AllTurtle"
--  cs:       class constraints, e.g. [TurtleLike _Self s, HasColor _Self s]
--  cstrs:    data constructor names, e.g. ["AllTurtle1", "AllTurtle2", ...]
--  ds:       data types, e.g. [Turtle s, ColorTurtle s, Turtle3D s, TwistedTurtle _Self]
--  (c, ms):  class name and its methods (e.g. (TurtleLike _Self s, [(forward, forward's type), (turn, turn's type), ...]))
--  return:   the instance declaration, e.g.
--    instance TurtleLike (AllTurtle s) s where
--        forward (AllTurtle1 t) = forward t
--        forward (AllTurtle2 t) = forward t
--        ...
defineInstance :: Type -> String -> [Type] -> [Name] -> [Type] -> (Type, [Dec]) -> Q Dec
defineInstance typ nBase cs cstrs ds (c, ms) = do
  -- decs <- mapM lookIntoMethod ms
  -- runIO $ putStrLn $ pprint $ concat decs
  -- _Self1, _Self2, ... のすべての組み合わせを考える
  -- 考え方 _Self が一種類のとき（binary method がないとき）
  --          _Self にすべての型を当てはめる
  --       _Self が複数のとき（binary method 以上があるとき）
  --          _Self1, _Self2, ... というように、独立な型変数に附番する
  --          従属な型変数は、__Self (_が２個)を使う
  --          インスタンスは _Self1, _Self2, ... に対してすべての組み合わせを考える
  -- 
  -- d や cs, ms の中で _Self の代わりに型名（e.g. AllTurtle s）が使われても対応できるようにする
  -- Todo: constructor class （e.g. Functor AllTurtle）のように、unsaturated の場合にも対応できるように
  let rev = [(typ, VarT $ mkName "_Self")]
      s = [(mkName "_Self", typ){- , (mkName "__Self", typ) -}]
  cxtss <- mapM (flip getInstanceContext (replaceType rev c) . replaceType rev) ds
  mdecs <- mapM (defineMethod nBase cstrs) ms
  let cxts = nub (map (substType s) (concat cxtss)) \\ cs
      ret = InstanceD Nothing cxts (substType s c) (catMaybes mdecs)
--  runIO $ putStrLn $ pprint ret
  pure ret


-- typeCarrefour typ ds cs
--   typ:  type expression, e.g. AllTurtle s
--   ds:   data type expressions, e.g. [Turtle s, ColorTurtle s, Turtle3D s, TwistedTurtle _Self]
--   cs:   class constraints, e.g. [TurtleLike _Self s, HasColor _Self s]
--   return: [Dec] for the type, data constructors, and class instances
typeCarrefour :: Type -> [Type] -> [Type] -> Q [Dec]
typeCarrefour typ ds cs = do
  let (n, tvs) = dataHead typ
      nBase = nameBase n
      tyconName = mkName nBase
      consts  = mkNames (length ds) nBase
      s = [(mkName "_Self", typ)]
      rev = [(typ, VarT $ mkName "_Self")]
      ds1 = map (substType s) ds
--  runIO $ putStrLn "---- defineAllSumData"
  dataDec <- defineAllSumData tyconName tvs consts ds1
--  -- runIO $ putStrLn "---- getMethodTypes"
  mts <- mapM (getMethodTypes . replaceType rev) cs
--  runIO $ putStrLn "---- defineInstance"
  let cs1 = map (substType s) cs
  insts <- mapM (defineInstance typ nBase cs1 consts ds) $ zip cs mts
  let ret = dataDec ++ insts
  runIO $ putStrLn $ pprint ret
  pure ret

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

-- e.g. 
-- [carrefour| 
-- data AllTurtle s <- Turtle s | ColorTurtle s | Turtle3D s | TwistedTurtle _Self 
--    deriving (TurtleLike _Self s, HasColor _Self s) 
-- |]
carrefourDec :: String -> Q [Dec]
carrefourDec s = do
  loc <- TH.location
  let pos = (TH.loc_filename loc, fst (TH.loc_start loc), snd (TH.loc_start loc))
  (t, ds, cs) <- parseCarrefourDec pos s
  t1 <- replaceConst t
  ds1 <- mapM replaceConst ds
  cs1 <- mapM replaceConst cs
  typeCarrefour t1 ds1 cs1

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

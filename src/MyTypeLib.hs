module MyTypeLib where

import Language.Haskell.TH (pprint)
import Language.Haskell.TH.Syntax

typeFArgs ::Type -> (Type, [Type])
typeFArgs (AppT t1 t2)     =
  let (n, ts) = typeFArgs t1
    in (n, ts ++ [t2])
typeFArgs (InfixT t1 n t2) = (ConT n, [t1, t2])
typeFArgs t                = (t, [])

typeToList :: MonadFail m => Type -> m [Type]
typeToList typ = do
  let (n, ts) = typeFArgs typ
  if n == TupleT (length ts)
    then pure ts
    else fail "typeToList: not a tuple"
  pure $ n : ts

dataHead :: Type ->(Name, [Name])
dataHead typ =
  let (ConT n, ts) = typeFArgs typ
      tvs = map (\ (VarT n) -> n) ts
    in (n, tvs)

replaceType :: [(Type, Type)] -> Type -> Type
replaceType env t = case lookup t env of
  Just t' -> t'
  Nothing -> case t of
    AppT t1 t2 -> AppT (replaceType env t1) (replaceType env t2)
    InfixT t1 n t2 -> InfixT (replaceType env t1) n (replaceType env t2)
    VarT n -> VarT n
    ConT n -> ConT n
    TupleT n -> TupleT n
    ListT -> ListT
    ArrowT -> ArrowT
    _ -> t  -- 他のコンストラクタは使われないはず

replaceDec :: [(Type, Type)] -> Dec -> Dec
replaceDec env (SigD n t) = SigD n (replaceType env t)
replaceDec _ d            = d

substType :: [(Name, Type)] -> Type -> Type
substType env (VarT n) = case lookup n env of
  Just t  -> t
  Nothing -> VarT n
substType env (AppT t1 t2) = AppT (substType env t1) (substType env t2)
substType env (ConT n) = ConT n
substType env (TupleT n) = TupleT n
substType env ListT = ListT
substType env ArrowT = ArrowT
substType env t = t      --- 他のコンストラクタは使われないはず
-- substType env ArrowT = ArrowT

substCompose :: [(Name, Type)] -> [(Name, Type)] -> [(Name, Type)]
substCompose s1 s2 = [(n, substType s1 t) | (n, t) <- s2] ++ s1

getNameTyVar :: TyVarBndr flg -> Name
getNameTyVar (PlainTV n _) = n
getNameTyVar (KindedTV n _ _) = n

substSig :: [(Name, Type)] -> Dec -> Dec
substSig env (SigD n t) = SigD n (substType env t)
substSig env d = d

unifyType :: Type -> Type -> Q [(Name, Type)]
unifyType (VarT n) t = pure [(n, t)] -- 左オペランドを優先する
unifyType t (VarT n) = pure [(n, t)]
unifyType (ConT n1) (ConT n2) | n1 == n2 = pure []
unifyType (AppT t1 t2) (AppT t3 t4) = do
  s1 <- unifyType t1 t3
  s2 <- unifyType (substType s1 t2) (substType s1 t4)
  pure $ substCompose s2 s1
unifyType (TupleT n1) (TupleT n2) | n1 == n2 = pure []
unifyType ListT ListT = pure []
unifyType ArrowT ArrowT = pure []
unifyType t1 t2 = fail $ "unifyType: " ++ pprint t1 ++ " and " ++ pprint t2

unifyTypes :: [Type] -> [Type] -> Q [(Name, Type)]
unifyTypes [] [] = pure []
unifyTypes (t1:ts1) (t2:ts2) = do
  s1 <- unifyType t1 t2
  s2 <- unifyTypes (map (substType s1) ts1) (map (substType s1) ts2)
  pure $ substCompose s2 s1
unifyTypes _ _ = fail "unifyTypes: different length"

matchType :: Type -> Type -> Q [(Name, Type)]
matchType (VarT n) t = pure [(n, t)]
matchType (ConT n1) (ConT n2) | n1 == n2 = pure []
matchType (AppT t1 t2) (AppT t3 t4) = do
  s1 <- matchType t1 t3
  s2 <- matchType t2 (substType s1 t4)
  pure $ substCompose s2 s1
matchType (TupleT n1) (TupleT n2) | n1 == n2 = pure []
matchType ListT ListT = pure []
matchType ArrowT ArrowT = pure []
matchType t1 t2 = fail $ "matchType: " ++ pprint t1 ++ " and " ++ pprint t2

matchTypes :: [Type] -> [Type] -> Q [(Name, Type)]
matchTypes [] [] = pure []
matchTypes (t1:ts1) (t2:ts2) = do
  s1 <- matchType t1 t2
  s2 <- matchTypes ts1 (map (substType s1) ts2)
  pure $ substCompose s2 s1
matchTypes _ _ = fail "matchTypes: different length"

fvs :: Type -> [Name] -> [Name]
fvs (VarT n) acc         = n : acc
fvs (AppT t1 t2) acc     = fvs t1 (fvs t2 acc)
fvs (InfixT t1 n t2) acc = fvs t1 (fvs t2 acc)
fvs (ConT _) acc         = acc
fvs (TupleT _) acc       = acc
fvs ListT acc            = acc
fvs ArrowT acc           = acc
fvs _ acc                = acc

fvsList :: [Type] -> [Name] -> [Name]
fvsList [] acc = acc
fvsList (t:ts) acc = fvsList ts (fvs t acc)

refreshFvs :: Type -> Q Type
refreshFvs t = do
  let names = fvs t []
  newNames <- newNames (length names) "x"
  let env = zip names (map VarT newNames)
  return $ substType env t

refreshFvsList :: [Type] -> Q [Type]
refreshFvsList ts = do
  let names = fvsList ts []
  newNames <- newNames (length names) "x"
  let env = zip names (map VarT newNames)
  return $ map (substType env) ts

newNames :: Int -> String -> Q [Name]
newNames n s = mapM (newName . (s ++) . show) [1..n]

module DefaultingPlugin where

-- https://downloads.haskell.org/ghc/latest/docs/users_guide/extending_ghc.html
-- https://hackage-content.haskell.org/package/ghc-9.6.7/docs/GHC-Plugins.html

import System.IO

import Data.Data
import Data.Maybe (catMaybes,mapMaybe)
import Data.List ((\\))
import Data.List.NonEmpty ( NonEmpty(..), toList)

import Language.Haskell.TH.Syntax (Name, nameBase, nameModule, namePackage)

import GHC.Plugins hiding (nameModule, namePackage)
import GHC.Core.Opt.Monad
import GHC.Core.Class
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Tc.Types
import GHC.Tc.Plugin
import GHC.Utils.Ppr
import GHC.Tc.Solver (approximateWC)
import GHC.Data.Bag (bagToList)
import GHC.Tc.Types.Constraint (Ct, WantedConstraints, ctPred, isEmptyWC, insolubleWC, tyCoVarsOfCt)
import GHC.Tc.Utils.TcType (isMetaTyVar, isTyConableTyVar)
import GHC.Data.List.SetOps (equivClasses)
import GHC.Types.SourceText (StringLiteral(..), SourceText(..))

import Carrefour (ForDefault(..), CastClass(..), sourceOfCast, classOfCast, MyName, myNameOfName)
import MyTypeLib (dataHead)

plugin :: Plugin
plugin = defaultPlugin {
--    installCoreToDos = install
    defaultingPlugin = defaultingP
}

-- see tryTypeClassDefaulting in https://github.com/ghc/ghc/blob/master/compiler/GHC/Tc/Solver/Default.hs
defaultingP :: GHC.Plugins.DefaultingPlugin
defaultingP _ = Just $ GHC.Tc.Types.DefaultingPlugin {
    dePluginInit = do
        tcPluginIO $ putStrLn "Defaulting plugin initialized"
        return ()
  , dePluginRun = tryDefaulting
  , dePluginStop = \ s -> do
        tcPluginIO $ putStrLn "Defaulting plugin stopped"
        return ()
}

-- ghc/compiler/GHC/Tc/Solver/Defaults.hs の tryTypeClassDefaulting を参考にする
-- data DefaultingProposal
--   = DefaultingProposal
--     { deProposals :: [[(TcTyVar, Type)]]
--       -- ^ The type variable assignments to try.
--     , deProposalCts :: [Ct]
--       -- ^ The constraints against which defaults are checked.
--   }
tryDefaulting :: () -> WantedConstraints -> TcPluginM [DefaultingProposal]
tryDefaulting s wanteds 
  | isEmptyWC wanteds || insolubleWC wanteds = return []
  | otherwise = do
      let simples :: [Ct]
          simples = bagToList $ approximateWC True wanteds
      if null simples
        then return []
        else do
          tcPluginIO $ do 
              putStrLn "Defaulting plugin running"
              printSDocLn defaultSDocContext (PageMode False) stdout (ppr simples)
          findProposal simples

nameOfMyName :: MyName -> TcPluginM GHC.Plugins.Name
nameOfMyName (base, mod, pkg) = do
    mod <- case mod of
        Nothing -> do
            (tcg, _) <- getEnvs
            return (tcg_mod tcg)
        Just m  -> do
            let pq = case pkg of
                         Nothing -> NoPkgQual
                         Just n  -> ThisPkg (UnitId (mkFastString n)) -- OtherPkg でなくてよいのか？
                mn = mkModuleName m
            result <- findImportedModule mn pq
            case result of
                Found _ mod -> return mod
                _           -> fail $ "nameOfMyName: module not found: " ++ show mn
    lookupOrig mod (mkTcOcc base)

nameEq :: Language.Haskell.TH.Syntax.Name -> MyName -> Bool
n1 `nameEq` (nb2, nm2, np2) = 
    nameBase n1 == nb2 
    && case (nameModule n1, nm2) of
        (Just m1, Just m2) -> m1 == m2
        (Nothing, Nothing) -> True
        _                  -> False
    && case (namePackage n1, np2) of
        (Just p1, Just p2) -> p1 == p2
        (Nothing, Nothing) -> True
        _                  -> False

forDefaultToClasses :: ForDefault -> TcPluginM (GHC.Plugins.Name, [GHC.Plugins.Name])
forDefaultToClasses (Derivings name types classes) = do
    anns <- getAnnotationsForData :: TcPluginM (UniqFM GHC.Plugins.Name [CastClass])
    -- -- ts <- mapM nameOfMyName types
    tcPluginIO $ do
        putStrLn "forDefaultToClasses:"
        printSDocLn defaultSDocContext (PageMode False) stdout (ppr anns)
    --     -- printSDocLn defaultSDocContext (PageMode False) stdout (ppr ts)
    --     -- putStrLn (show $ map getUnique ts)
    let anns2 :: [CastClass]
        anns2 = concat $ nonDetEltsUFM anns
        classes2 :: [MyName]
        classes2 = map (myNameOfName . classOfCast)  
                      $ concat $ map (\dn -> filter (\ cc -> sourceOfCast cc `nameEq` dn ) anns2) types
    tcPluginIO $ do
        putStrLn "----"
        putStrLn (show name)
        putStrLn (show types)
        putStrLn (show anns2)
        putStrLn (show classes2)
        -- printSDocLn defaultSDocContext (PageMode False) stdout (ppr classes2)
    classes' <- mapM nameOfMyName (classes ++ classes2) 
    tcPluginIO $ do
        putStrLn "---- return"
        printSDocLn defaultSDocContext (PageMode False) stdout (ppr classes')     
    name' <- nameOfMyName name
    tcPluginIO $ do
        printSDocLn defaultSDocContext (PageMode False) stdout (ppr name')
        putStrLn "forDefaultToClasses: end"
    return (name', classes')

tnnameToTyCon :: GHC.Plugins.Name -> TcPluginM Type
tnnameToTyCon n = do
    tycon <- tcLookupTyCon n
    let n = tyConArity tycon
        vars = tyConTyVars tycon -- Todo: これでよいのか？
    vars' <- mapM copyTyVar vars
    return (mkTyConApp tycon (map mkTyVarTy vars'))
  where
      copyTyVar :: TyVar -> TcPluginM TcTyVar
      copyTyVar tv = newFlexiTyVar (varType tv)
          
-- Haskell のソース https://gitlab.haskell.org/ghc/ghc の
-- ghc/compiler/GHC/Tc/Solver/Defaults.hs 
-- (https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Tc/Solver/Default.hs?ref_type=heads) 
-- の findDefaultableGroups, disambigGroup までを参考にする
--   なお、disambigProposalSequences は run_defaulting_plugin が呼び出すはず
findProposal :: [Ct] -> TcPluginM [DefaultingProposal]
-- よくわからないが Ct には ambiguity に関する型変数を含む Constraint しか含まれていないようだ
-- Todo: 上記を確認する
findProposal simples = do
    tcPluginIO $ putStrLn "findProposal: Start"
    let preds = map ctPred simples
    tcPluginIO $ do
        putStrLn "findProposal: -- preds"
        printSDocLn defaultSDocContext (PageMode False) stdout (ppr preds)
    -- 今ここ
    anns <- getAnnotationsForData :: TcPluginM (UniqFM GHC.Plugins.Name [ForDefault])
    let annList :: [ForDefault]
        annList = concat $ nonDetEltsUFM anns
    tcPluginIO $ do
        putStrLn "-- annotations"
        putStrLn (show annList)
    candidates <- mapM forDefaultToClasses annList
    tcPluginIO $ do
        putStrLn "-- candidates"
        printSDocLn defaultSDocContext (PageMode False) stdout (ppr candidates)
        putStrLn "findProposal: End"
    return []
  where
    (ptcs, nonPtcs) = partitionWith findPTC simples
    ptcs :: [(Ct, Class, TcTyVar)]
    nonPtcs :: [Ct]
    ptcGroups       = equivClasses cmp_tv ptcs
    ptcGroups :: [NonEmpty (Ct, Class, TcTyVar)]
    cmp_tv (_,_,tv1) (_,_,tv2) = tv1 `compare` tv2
    defaultable_tyvar tv others = 
        isTyConableTyVar tv 
        && not (tv `elemVarSet` mapUnionVarSet tyCoVarsOfCt nonPtcs) 
        && not (tv `elemVarSet` mapUnionVarSet tyCoVarsOfCt others)
    defaultable_classes classes candidates = 
        map fst $ filter (\ (t, cs) -> all (\ c -> c `elem` cs) classes) candidates
    proposalOf candidates = [ (tv, t) 
              | (group'@((_, _, tv) :| _), rest)  <- holes ptcGroups
              , let group = toList group'
              , defaultable_tyvar tv (map fstOf3 $ concatMap toList rest)
              , t <- defaultable_classes (map sndOf3 group) candidates]

-- find parametric type classes (classes with only one dependent parameter)
findPTC :: Ct -> Either (Ct, Class, TcTyVar) Ct 
findPTC cc
    | Just (cls, tys) <- getClassPredTys_maybe (ctPred cc)
    , [ty] <- {- uniq? $ -} catMaybes $ zipWith (\ b ty -> if b then Just ty else Nothing) (getIndependentParams cls) tys
    , Just tv <- getTyVar_maybe ty
    , isMetaTyVar tv -- 必要？
    = Left (cc, cls, tv)
    | otherwise = Right cc

-- basically, reimplementation of getDependentParams in Carrefour.hs but for GHC.Core.Class
getIndependentParams :: Class -> [Bool]
getIndependentParams cls = let
    (vars, deps) = classTvsFds  cls
    ds = loop deps [] 
  in  map (`elem` ds) vars
  where
    dependentParams deps = concatMap (\ (_, ns) -> ns) deps 
                            \\ concatMap(\ (is, _) -> is) deps
    removeParam ns (as, rs) = case rs \\ ns of 
        []  -> Nothing
        rs' -> Just (as, rs')
    removeDependentParams ns = mapMaybe (removeParam ns)
    loop deps acc = case dependentParams deps of
      [] -> acc
      ds -> loop (removeDependentParams ds deps) (acc ++ ds)

-- ghc/compiler/GHC/Tc/Gen/Splice.hs の reifyAnnotations を参考にする
-- さらに GHC.Types.Annotations の findAnns, deserializeAnns を使う
getAnnotationsForData :: Data a => TcPluginM (UniqFM GHC.Plugins.Name [a])
getAnnotationsForData = do
    topEnv   <- getTopEnv -- getAnnotations はこちらを使っていない？
    epsHptAnns <- tcPluginIO $ prepareAnnotations topEnv Nothing
    (tcg, _) <- getEnvs
    -- Todo: ModuleEnv も使用する
    let (menv1, env1) = deserializeAnns deserializeWithData epsHptAnns
        (menv2, env2) = deserializeAnns deserializeWithData (tcg_ann_env tcg)
    return (env1 `plusUFM` env2)

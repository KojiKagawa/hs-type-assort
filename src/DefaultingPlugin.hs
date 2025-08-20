module DefaultingPlugin where

-- https://downloads.haskell.org/ghc/latest/docs/users_guide/extending_ghc.html
-- https://hackage-content.haskell.org/package/ghc-9.6.7/docs/GHC-Plugins.html

import System.IO

import Data.Data
import Data.Maybe (catMaybes,mapMaybe)
import Data.List ((\\))
import Data.List.NonEmpty ( NonEmpty(..), toList)

import GHC.Plugins
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

import Carrefour (ForDefault(..))

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

-- ghc/compiler/GHC/Tc/Solver/Defaults.hs の findDefaultableGroups, disambigGroup までを参考にする
--   なお、disambigProposalSequences は run_defaulting_plugin が呼び出すはず
findProposal :: [Ct] -> TcPluginM [DefaultingProposal]
-- よくわからないが Ct には ambiguity に関する型変数を含む Constraint しか含まれていないようだ
-- Todo: 上記を確認する
findProposal simples = do
    let preds = map ctPred simples
    tcPluginIO $ printSDocLn defaultSDocContext (PageMode False) stdout (ppr preds)  
    -- 今ここ
    anns <- findForDefaultAnn :: TcPluginM (UniqFM Name [ForDefault])
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
    defaultable_classes classes = True -- 今ここ
    group = [ (tv, map fstOf3 group) 
              | (group'@((_, _, tv) :| _), rest)  <- holes ptcGroups
              , let group = toList group'
              , defaultable_tyvar tv (map fstOf3 $ concatMap toList rest)
              , defaultable_classes (map sndOf3 group) ]


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

findForDefaultAnn :: Data a => TcPluginM (UniqFM Name [a])
findForDefaultAnn = do
    topEnv   <- getTopEnv
    epsHptAnns <- tcPluginIO $ prepareAnnotations topEnv Nothing
    (tcg, _) <- getEnvs
    let (_, env1) = deserializeAnns deserializeWithData epsHptAnns
        (_, env2) = deserializeAnns deserializeWithData (tcg_ann_env tcg)
    return (env1 `plusUFM` env2)

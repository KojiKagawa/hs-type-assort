module DefaultingPlugin where

-- https://downloads.haskell.org/ghc/latest/docs/users_guide/extending_ghc.html
-- https://hackage-content.haskell.org/package/ghc-9.6.7/docs/GHC-Plugins.html

import System.IO

import GHC.Plugins
import GHC.Core.Opt.Monad
import GHC.Tc.Types
import GHC.Tc.Plugin
import GHC.Utils.Ppr
import GHC.Tc.Solver (approximateWC)
import GHC.Data.Bag (bagToList)
import GHC.Tc.Types.Constraint (Ct, ctPred)

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
  , dePluginRun = \ s wanteds -> do 
        let simples :: [Ct]
            simples = bagToList $ approximateWC True wanteds
        if null simples
          then do
            tcPluginIO $ putStrLn "No defaulting needed"
            return []
          else do
            tcPluginIO $ do 
                putStrLn "Defaulting plugin running"
                printSDocLn defaultSDocContext (PageMode False) stdout (ppr simples)
            findProposal simples
  , dePluginStop = \ s -> do
        tcPluginIO $ putStrLn "Defaulting plugin stopped"
        return ()
}

findProposal :: [Ct] -> TcPluginM [DefaultingProposal]
-- よくわからないが Ct には ambiguity に関する型変数を含む Constraint しか含まれていないようだ
-- Todo: 上記を確認する
findProposal simples = do
    -- Implement your proposal finding logic here
    let preds = map ctPred simples
    tcPluginIO $ printSDocLn defaultSDocContext (PageMode False) stdout (ppr preds)
    return []

-- install :: [CommandLineOption] -> [CoreToDo] -> [CoreToDo]
-- install _ tod = do
--     return todo
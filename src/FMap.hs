{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module FMap where

import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax

import Data.Bifunctor
import Control.Arrow (Arrow)
import Control.Monad.ST

fmapTest :: Type -> Q (Maybe (Maybe Exp))
fmapTest (ForallT [PlainTV self _] [] t) = do
       inj <- newName "inj"
       mkFMap self t (VarE inj)
fmapTest (ForallVisT [PlainTV self _] t) = do
       inj <- newName "inj"
       mkFMap self t (VarE inj)
fmapTest _ = fail "fmapTest: not a ForallT/ForallVisT"

-- usage:
-- ghci> :set -XExplicitForAll
-- ghci> :set -XTemplateHaskellQuotes
-- ghci> fmapIO [t| forall self. Int -> self |]
fmapIO :: Q Type -> IO ()
fmapIO qt = do
  t <- runQ qt
  runQ (fmapTest t) >>= \case
    Nothing -> putStrLn "not found"
    Just Nothing -> putStrLn "identity"
    Just (Just f1) -> putStrLn $ pprint f1


-- fmap, contramap, -- c.f.) https://hackage.haskell.org/package/thorn-0.2/docs/Data-Thorn.html
-- ここのは簡易版 
mkFMap :: Name -> Type -> Exp -> Q (Maybe (Maybe Exp))
-- Name はたいてい '_Self である -- Type の中の Self Type を表す型変数
-- Exp は　injection function
-- 戻り値:
-- Nothing:       存在しない
-- Just Nothing:  fmap は 恒等関数
-- Just (Just e): fmap は e
mkFMap n (VarT m) inj = if n == m then pure $ Just (Just inj) else pure $ Just Nothing
-- [], Maybe, ->, (,), (,,),     Either, ST, SR, IO, STRef, IORef などに対応する
mkFMap n (ConT m) inj = pure $ Just Nothing
mkFMap n (AppT ListT t) inj = do
  fmap1 <- mkFMap n t inj
  case fmap1 of
    Nothing -> pure Nothing
    Just Nothing -> pure $ Just Nothing
    Just (Just e1) -> do
      ret <- [| map $(pure e1) |]
      pure $ Just $ Just ret
mkFMap n (AppT (ConT maybeName) t) inj | maybeName == ''Maybe = do
  fmap1 <- mkFMap n t inj
  case fmap1 of
    Nothing -> pure Nothing
    Just Nothing -> pure $ Just Nothing
    Just (Just e1) -> do
      ret <- [| map $(pure e1) |]
      pure $ Just $ Just ret
mkFMap n (TupleT 0) inj = pure $ Just Nothing
mkFMap n (AppT (AppT (TupleT 2) t1) t2) inj = do
  fmap1 <- mkFMap n t1 inj
  fmap2 <- mkFMap n t2 inj
  case (fmap1, fmap2) of
    (Nothing, _) -> pure Nothing
    (_, Nothing) -> pure Nothing
    (Just Nothing, Just Nothing) -> pure $ Just Nothing
    (Just (Just e1), Just (Just e2)) -> do
      ret <- [| Data.Bifunctor.bimap $(pure e1) $(pure e2) |]
      pure $ Just $ Just ret
    (Just Nothing, Just (Just e2)) -> do
      ret <- [| Data.Bifunctor.second $(pure e2) |]
      pure $ Just $ Just ret
    (Just (Just e1), Just Nothing) -> do
      ret <- [| Data.Bifunctor.first $(pure e1) |]
      pure $ Just $ Just ret
mkFMap n (AppT (AppT ArrowT t1) t2) inj = do
  cmap1 <- mkContraMap n t1 inj
  fmap2 <- mkFMap n t2 inj
  case (cmap1, fmap2) of
    (Nothing, _) -> pure Nothing
    (_, Nothing) -> pure Nothing
    (Just Nothing, Just Nothing) -> pure $ Just Nothing
    (Just (Just e1), Just (Just e2)) -> do
      ret <- [|\ m x -> $(pure e2) (m ($(pure e1) x))|]
      pure $ Just $ Just ret
    (Just Nothing, Just (Just e2)) -> do
      ret <- [|\ m x -> $(pure e2) (m x)|]
      pure $ Just $ Just ret
    (Just (Just e1), Just Nothing) -> do
      ret <- [|\ m x -> m ($(pure e1) x)|]
      pure $ Just $ Just ret
mkFMap n ty@(AppT (AppT (ConT c) t1) t2) inj
    | c == ''ST = do
        fmap1 <- mkFMap n t1 inj
        fmap2 <- mkFMap n t2 inj
        case (fmap1, fmap2) of
            (Nothing, _) -> pure Nothing
            (Just (Just _), _) -> pure Nothing
            (_, Nothing) -> pure Nothing
            (Just Nothing, Just Nothing) -> pure $ Just Nothing
            (Just Nothing, Just (Just e2)) -> do
                ret <- [| map $(pure e2) |]
                pure $ Just $ Just ret
    | otherwise = fail ("mkFMap: not supported type" ++ pprint ty ++ " (" ++ show ty ++")")
mkFMap n ty _ = fail ("mkFMap: not supported type" ++ pprint ty ++ " (" ++ show ty ++")")

mkContraMap :: Name -> Type -> Exp -> Q (Maybe (Maybe Exp))
mkContraMap n (VarT m) inj = if n == m then pure Nothing else pure $ Just Nothing
mkContraMap n (ConT m) inj = pure $ Just Nothing
mkContraMap n (AppT (AppT ArrowT t1) t2) inj = do
  fmap1 <- mkFMap n t1 inj
  cmap2 <- mkContraMap n t2 inj
  case (fmap1, cmap2) of
    (Nothing, _) -> pure Nothing
    (_, Nothing) -> pure Nothing
    (Just Nothing, Just Nothing) -> pure $ Just Nothing
    (Just (Just e1), Just (Just e2)) -> do
      ret <- [|\ m x -> $(pure e2) (m ($(pure e1) x))|]
      pure $ Just $ Just ret
    (Just Nothing, Just (Just e2)) -> do
      ret <- [|\ m x -> $(pure e2) (m x)|]
      pure $ Just $ Just ret
    (Just (Just e1), Just Nothing) -> do
      ret <- [|\ m x -> m ($(pure e1) x)|]
      pure $ Just $ Just ret
mkContraMap n ty _ = fail ("mkContraMap: not supported type" ++ pprint ty)

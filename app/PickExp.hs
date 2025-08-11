{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module PickExp where

import Exp
import Times

import Data.Either


class Pick0 a b c | a b -> c where
    pick0 :: a -> b -> c

instance Pick0 Lit Lit Lit where
    pick0 e1 e2 = e1

instance Pick0 (Plus e1 e2) (Plus e3 e4) (Either (Plus e1 e2) (Plus e3 e4))  where  -- Todo: internal Either を調べてみる
    pick0 e1 e2 = Left e1

instance Pick0 Lit (Plus e1 e2) (Either Lit (Plus e1 e2)) where
    pick0 e1@(Lit 0) e2 = Left e1
    pick0 e1 e2         = Right e2

instance Pick0 (Plus e1 e2) Lit (Either Lit (Plus e1 e2)) where
    pick0 e1 e2@(Lit 0) = Left e2
    pick0 e1 e2         = Right e1

instance Pick0 (Times e1 e2) (Times e3 e4) (Either (Times e1 e2) (Times e3 e4))  where
    pick0 e1 e2 = Right e2

instance Pick0 Lit (Times e3 e4) (Either Lit (Times e3 e4))  where
    pick0 e1@(Lit 1) e2 = Left e1
    pick0 e1 e2         = Right e2

instance Pick0 (Times e1 e2) Lit (Either Lit (Times e1 e2)) where
    pick0 e1 e2@(Lit 1) = Left e2
    pick0 e1 e2         = Right e1

instance Pick0 (Plus e1 e2) (Times e3 e4) (Either (Plus e1 e2) (Times e3 e4))  where
    pick0 e1 e2 = Right e2

instance Pick0 (Times e1 e2) (Plus e3 e4) (Either (Times e1 e2) (Plus e3 e4))  where
    pick0 e1 e2 = Left e1
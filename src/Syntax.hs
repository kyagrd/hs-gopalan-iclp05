{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}

module Syntax    where

import           Unbound.LocallyNameless

type Nm = Name Tm

data Tm = V Nm
        | B Nm
        | Lam (Bind Nm Tm)
        | App Tm Tm
     deriving (Eq,Ord,Show)

instance Eq (Bind Nm Tm) where (==) = aeq
instance Ord (Bind Nm Tm) where compare = acompare

$(derive [''Tm])

instance Alpha Tm

instance Subst Tm Tm where
  isvar (V x) = Just (SubstName x)
  isvar (B x) = Just (SubstName x)
  isvar _     = Nothing

lam x = Lam . bind x
app = App

occurs :: Alpha t => Nm -> t -> Bool
occurs x t = x `elem` (fv t :: [Nm])

type Prefix = [[Nm]]
{-
forall w1 w2 w3, exists x1 x2 x3, forall y1 y2 3, ... is
encoded as [[y3,y2,y1],[x3,x2,x1],[w3,w2,w1]]. That is,
(!!n) gets a block of universal quatifications for even n
and a block of existential quantificiations for odd n.
-}

rigid :: Nm -> Prefix -> Bool
rigid x prefix | i < 0     = error $ show x ++ " is not bound"
               | otherwise = even i
  where i = blockIndex x prefix

-- non zero result of blockIndex means x is not found in prefix
blockIndex :: Nm -> Prefix -> Int
blockIndex x prefix = length (dropWhile (notElem x) prefix) - 1

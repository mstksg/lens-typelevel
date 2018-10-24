{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TemplateHaskell         #-}
{-# LANGUAGE TupleSections           #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeInType              #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}

module Data.Type.Lens (
  -- * Setting
    ASetter
  , Over, sOver, over
  , Set, sSet, set
  , Sets_, Sets, sSets, sets
  -- * Getting
  , Getting
  , View, sView, view
  , To_, To, sTo, to
  , ToListOf, sToListOf, toListOf
  -- * Lenses
  , LensLike
  , MkLens, sMkLens, mkLens
  -- * Samples
  , L1_, L1, sL1, l1
  , L2_, L2, sL2, l2
  -- * Defunctionalization Symbols
  , ASetterSym0, ASetterSym1, ASetterSym2, ASetterSym3, ASetterSym4
  , OverSym0, OverSym1, OverSym2, OverSym3
  , SetSym0, SetSym1, SetSym2, SetSym3
  , SetsSym0, SetsSym1, SetsSym2, SetsSym3
  , GettingSym0, GettingSym1, GettingSym2, GettingSym3
  , ViewSym0, ViewSym1, ViewSym2
  , ToSym0, ToSym1, ToSym2, ToSym3
  , ToListOfSym0, ToListOfSym1, ToListOfSym2
  , LensLikeSym0, LensLikeSym1, LensLikeSym2, LensLikeSym3, LensLikeSym4, LensLikeSym5
  , MkLensSym0, MkLensSym1, MkLensSym2, MkLensSym3, MkLensSym4
  , L1Sym0, L1Sym1, L1Sym2
  , L2Sym0, L2Sym1, L2Sym2
  ) where

import           Control.Applicative
import           Data.Functor.Identity
import           Data.Singletons.Prelude.Const
import           Data.Singletons.Prelude.Functor
import           Data.Singletons.Prelude.Identity
import           Data.Singletons.Prelude.Tuple
import           Data.Singletons.TH

$(singletons [d|
  type LensLike f s t a b = (a -> f b) -> (s -> f t)
  type ASetter    s t a b = LensLike Identity  s t a b
  type Getting  r s   a   = LensLike (Const r) s s a a

  over :: ASetter s t a b -> (a -> b) -> (s -> t)
  over l f x = case l (Identity . f) x of
      Identity y -> y

  set :: ASetter s t a b -> b -> s -> t
  set l y = over l (\_ -> y)

  view :: Getting a s a -> s -> a
  view l x = case l Const x of
      Const y -> y

  sets :: ((a -> b) -> (s -> t)) -> ASetter s t a b
  sets f g = Identity . f (\x -> case g x of Identity y -> y)

  to :: (s -> a) -> Getting r s a
  to f g x = case g (f x) of
      Const y -> Const y

  mkLens
      :: Functor f
      => (s -> a)
      -> (s -> b -> t)
      -> LensLike f s t a b
  mkLens v s f x = s x <$> f (v x)

  toListOf :: Getting [a] s a -> s -> [a]
  toListOf l x = case l (Const . (:[])) x of
      Const ys -> ys

  l1 :: Functor f => LensLike f (a, c) (b, c) a b
  l1 f (x, y) = (\x' -> (x', y)) <$> f x

  l2 :: Functor f => LensLike f (a, b) (a, c) b c
  l2 f (x, y) = (\y' -> (x, y')) <$> f y
  |])

-- type MyTuple = Set  L1Sym0 'True     '("hello", 6)
-- type MyNat   = View L2Sym0           '("hello", 6)
-- type MyNat'  = View (ToSym1 SndSym0) '("hello", 6)

type To_   f = ToSym1   f
type Sets_ f = SetsSym1 f

type L1_ = L1Sym0
type L2_ = L2Sym0

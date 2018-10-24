{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Lens (
  -- * Setting
    ASetter
  -- ** Using
  , Over, type (%~), sOver, over
  , Set, type (.~), sSet, set
  -- ** Making
  , Sets_, Sets, sSets, sets
  -- * Getting
  , Getting
  -- ** Using
  , View, type (^.), sView, view
  -- ** Making
  , To_, To, sTo, to
  -- * Lenses
  , LensLike
  -- ** Making
  , MkLens_, MkLens, sMkLens, mkLens
  -- * Traversals and Folds
  -- ** Using
  , Preview, type (^?), sPreview, preview
  , ToListOf, type (^..), sToListOf, toListOf
  , UnsafePreview, type (^?!), sUnsafePreview, unsafePreview
  -- ** Making
  , Folding_, Folding, sFolding, folding
  , Folded_, Folded, sFolded, folded
  , Traverse_, Traverse, sTraverse, traverse
  -- * Util
  , type (.@)
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
  , FoldingSym0, FoldingSym1, FoldingSym2, FoldingSym3
  , FoldedSym0, FoldedSym1, FoldedSym2
  , L1Sym0, L1Sym1, L1Sym2
  , L2Sym0, L2Sym1, L2Sym2
  ) where

import           Control.Applicative
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Monoid
import           Data.Singletons.Prelude.Const
import           Data.Singletons.Prelude.Foldable hiding (Traverse_)
import           Data.Singletons.Prelude.Functor
import           Data.Singletons.Prelude.Identity
import           Data.Singletons.Prelude.Maybe
import           Data.Singletons.Prelude.Monoid
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

  preview :: Getting (First a) s a -> s -> Maybe a
  preview l x = case l (Const . First . Just) x of
      Const (First y) -> y

  unsafePreview :: Getting (First a) s a -> s -> a
  unsafePreview l x = case preview l x of
      Just y  -> y
      Nothing -> error "Failed indexing into empty traversal"

  folding :: (Foldable f, Monoid r) => (s -> f a) -> Getting r s a
  folding f g x = case traverse_ g (f x) of
      Const y -> Const y

  folded :: (Foldable f, Monoid r) => Getting r (f a) a
  folded f x = case traverse_ f x of
      Const y -> Const y

  l1 :: Functor f => LensLike f (a, c) (b, c) a b
  l1 f (x, y) = (\x' -> (x', y)) <$> f x

  l2 :: Functor f => LensLike f (a, b) (a, c) b c
  l2 f (x, y) = (\y' -> (x, y')) <$> f y
  |])

type l %~  f = OverSym2 l f
type l .~  x = SetSym2 l x
type x ^.  l = View l x
type x ^?  l = Preview l x
type x ^?! l = UnsafePreview l x
type x ^.. l = ToListOf l x

infixr 4 %~
infixr 4 .~
infixl 8 ^.
infixl 8 ^?
infixl 8 ^?!
infixl 8 ^..

type f .@ g = f .@#@$$$ g

infixr 9 .@

type To_     f   = ToSym1   f
type Sets_   f   = SetsSym1 f
type MkLens_ f g = MkLensSym2 f g

type L1_ = L1Sym0
type L2_ = L2Sym0

type Traverse_ = TraverseSym0
type Folding_ f = FoldingSym1 f
type Folded_   = FoldedSym0

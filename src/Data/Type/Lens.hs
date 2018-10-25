{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Lens (
  -- * Setting
    ASetter
  -- ** Using
  , Over, type (%~), sOver
  , Set, type (.~), sSet
  -- ** Making
  , Sets_, Sets, sSets
  -- * Getting
  , Getting
  -- ** Using
  , View, type (^.), sView
  -- ** Making
  , To_, To, sTo
  -- * Lenses
  , LensLike, LensLike'
  -- ** Making
  , MkLens_, MkLens, sMkLens
  -- * Traversals and Folds
  -- ** Using
  , Preview, type (^?), sPreview
  , ToListOf, type (^..), sToListOf
  , UnsafePreview, type (^?!), sUnsafePreview
  -- ** Making
  , Folding_, Folding, sFolding
  , Folded_, Folded, sFolded
  , Traverse_, Traverse, sTraverse
  -- * Util
  , type (.@)
  -- * Samples
  -- ** Tuple
  , L1_, L1, sL1
  , L2_, L2, sL2
  -- ** List
  , N(..), SN
  , IxList_, IxList, sIxList
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
  , LensLike'Sym0, LensLike'Sym1, LensLike'Sym2, LensLike'Sym3
  , MkLensSym0, MkLensSym1, MkLensSym2, MkLensSym3, MkLensSym4
  , FoldingSym0, FoldingSym1, FoldingSym2, FoldingSym3
  , FoldedSym0, FoldedSym1, FoldedSym2
  , L1Sym0, L1Sym1, L1Sym2
  , L2Sym0, L2Sym1, L2Sym2
  , ZSym0, SSym0, SSym1
  , IxListSym0, IxListSym1, IxListSym2, IxListSym3
  -- * Sing
  , Sing (SZ, SS)
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
  type LensLike  f s t a b = (a -> f b) -> (s -> f t)
  type LensLike' f s   a   = LensLike f         s s a a
  type ASetter     s t a b = LensLike Identity  s t a b
  type Getting   r s   a   = LensLike (Const r) s s a a

  data N = Z | S N
  |])

$(singletonsOnly [d|
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

  ixList :: Applicative f => N -> LensLike' f [a] a
  ixList _     _ []     = pure []
  ixList Z     f (x:xs) = (:xs) <$> f x
  ixList (S i) f (x:xs) = (x:)  <$> ixList i f xs
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

type Traverse_  = TraverseSym0
type Folding_ f = FoldingSym1 f
type Folded_    = FoldedSym0

type L1_       = L1Sym0
type L2_       = L2Sym0
type IxList_ i = IxListSym1 i


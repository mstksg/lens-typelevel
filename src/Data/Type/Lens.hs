{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Data.Type.Lens
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Lenses and optics for manipulating DataKind-based types, powered by
-- /singletons/ defunctionalization.
--
-- See "Data.Type.Lens.Examples" for example usage and syntax.
--
-- For the most part, you should be able to use them just like you'd use
-- the functions from the /lens/ or /microlens/ libraries; just remember
-- to capitalize names like 'Over' and 'Set', since they are type families.
--
-- Note that the ways of "creating" a lens or optic ('Sets_', 'Traverse_',
-- 'To_', 'MkLens_', etc. are all suffixed with @_@ for convenience, to
-- reserve the underscoreless identifiers for the fully applied type family
-- as per /singletons/ library convention.
module Data.Type.Lens (
    LensLike, LensLike'
  -- * Setting
  , ASetter
  -- ** Using
  -- | Ways of consuming a setter.
  , Over, type (%~), sOver, over
  , Set, type (.~), sSet, set
  -- ** Making
  -- | Ways of creating a setter-only.
  , Sets_, Sets, sSets, sets
  -- * Getting
  , Getting
  -- ** Using
  -- | Ways of consuming a getter
  , View, type (^.), sView, view
  -- ** Making
  -- | Ways of creating a getter-only.
  , To_, To, sTo, to
  -- * Lenses
  , ALens
  -- ** Making
  -- | Ways of creating a lens
  , MkLens_, MkLens, sMkLens, mkLens
  -- ** Cloning
  , CloneLens_, CloneLens, sCloneLens
  -- * Traversals and Folds
  , ATraversal
  -- ** Using
  -- | Ways of consuming traversals and folds
  , Preview, type (^?), sPreview, preview
  , ToListOf, type (^..), sToListOf, toListOf
  , UnsafePreview, type (^?!), sUnsafePreview, unsafePreview
  -- ** Making
  -- | Ways of creating traversals and folds
  , Folding_, Folding, sFolding, folding
  , Folded_, Folded, sFolded, folded
  , Traverse_, Traverse, sTraverse, traverse
  -- ** Cloning
  , CloneTraversal_, CloneTraversal, sCloneTraversal
  -- * Samples
  -- | Some sample lenses and traversals
  --
  -- ** Tuple
  , L1_, L1, sL1, l1
  , L2_, L2, sL2, l2
  -- ** List
  , N(..), SN
  , IxList_, IxList, sIxList, ixList
  -- * Util
  , type (.@)
  , Sing (SZ, SS, SMkContext)
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
  , CloneLensSym0, CloneLensSym1, CloneLensSym2, CloneLensSym3
  , FoldingSym0, FoldingSym1, FoldingSym2, FoldingSym3
  , FoldedSym0, FoldedSym1, FoldedSym2
  , L1Sym0, L1Sym1, L1Sym2
  , L2Sym0, L2Sym1, L2Sym2
  , ZSym0, SSym0, SSym1
  , IxListSym0, IxListSym1, IxListSym2, IxListSym3
  ) where

import           Control.Applicative
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Monoid
import           Data.Singletons.Prelude.Const
import           Data.Singletons.Prelude.Foldable hiding (Traverse_)
import           Data.Singletons.Prelude.Function hiding (Const, ConstSym0)
import           Data.Singletons.Prelude.Functor
import           Data.Singletons.Prelude.Identity
import           Data.Singletons.Prelude.Maybe
import           Data.Singletons.Prelude.Monoid
import           Data.Singletons.TH
import           Data.Type.Lens.Internal
import           Data.Typeable                           (Typeable)
import           GHC.Generics                            (Generic)

-- | The general shape of optics in this library. ("van Laarhoven")
--
-- For different levels of polymorphism on @f@, you get different types of
-- optics:
--
--     * If @f@ can be any 'Functor', you have a Lens (see 'ALens')
--     * If @f@ is only 'Identity', you have a setter (see 'ASetter')
--     * If @f@ is only @'Const' R@ for a specific @R@, you have a getter
--       of @R@ (see 'Getting')
--     * If @f@ can be @'Const' r@ for any 'Monoid' @r@, you have a Fold.
--     * If @f@ can be any 'Applicative', you have a Traversal (see
--       'ATraversal')
--
-- Normal lens libraries implement the constraints for lenses, folds, and
-- traversals using RankN types, but we don't do that here to avoid working
-- with RankN kinds.
type LensLike  f s t a b = (a -> f b) -> (s -> f t)

-- | A 'LensLike' that does not change any types.
type LensLike' f s   a   = LensLike f         s s a a

-- | A settable "lens".  Usable with 'Over' ('%~'), constructable with 'To'
-- or any of the general lens constructors.
--
-- See 'LensLike' for more information.
type ASetter     s t a b = LensLike Identity  s t a b

-- | A retrieving "lens".  If @r@ is fixed to a type, it's a Getter for
-- that type.  If @r@ is polymorphic over all 'Monoid', then it's a Fold
-- over @a@s.
--
-- As a Getter, usable with 'View' ('^.'); as a Fold, usable with
-- 'ToListOf' ('^..'), 'Preview' ('^?'), etc.
--
-- Normal lens libraries implement the constraints for folds using RankN
-- types, but we don't do that here to avoid working with RankN kinds.
--
-- See 'LensLike' for more information.
type Getting   r s   a   = LensLike (Const r) s s a a

-- | Peano nats, used for implementation of list index traversals in
-- a termination-sane way.
data N = Z | S N
  deriving (Show, Eq, Ord, Read, Generic, Typeable)

genSingletons [''LensLike, ''LensLike', ''ASetter, ''Getting, ''N]
singEqInstance ''N
singOrdInstance ''N
singDecideInstance ''N
singShowInstance ''N

-- | If a function expects an 'ALens', it can be given any Lens (a
-- @'LensLike' f@ that works for any 'Functor' f).
--
-- You can use an 'ALens' as a normal lens by using 'CloneLens_'.
type ALens s t a b = LensLike (Context a b) s t a b

-- | If a function expects an 'ATraversal', it can be given any Traversal
-- (a @'LensLike' f@ that works for any 'Applicative' f).
--
-- You can use an 'ATraversal' as a normal traversal by using
-- 'CloneTraversal_'.
type ATraversal s t a b = LensLike (Bazaar a b) s t a b

$(singletons [d|
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

$(singletonsOnly [d|
  cloneLens
      :: Functor f
      => LensLike (PContext a b) s t a b
      -> LensLike f s t a b
  cloneLens l f x = case l (\y -> MkContext id y) x of
      MkContext g y -> g <$> f y

  cloneTraversal
      :: Applicative f
      => LensLike (Bazaar a b) s t a b
      -> LensLike f s t a b
  cloneTraversal l f xs = unBazaar f $ l (`More` Done id) xs
  |])


-- | Infix application of 'Over'
type l %~  f = OverSym2 l f

-- | Infix application of 'Set'
type l .~  x = SetSym2 l x

-- | Infix application of 'View'
type x ^.  l = View l x

-- | Infix application of 'Preview'
type x ^?  l = Preview l x

-- | Infix application of 'UnsafePreview'
type x ^?! l = UnsafePreview l x

-- | Infix application of 'ToListOf'
type x ^.. l = ToListOf l x

-- | Shorter name for type-level function composition
type f .@ g = f .@#@$$$ g

infixr 4 %~
infixr 4 .~
infixl 8 ^.
infixl 8 ^?
infixl 8 ^?!
infixl 8 ^..
infixr 9 .@

-- | Create a Getter from a getting function.
--
-- @
-- 'To_' :: (a ~> b) -> 'Getting' b a b
-- @
type To_     f   = ToSym1   f

-- | Create a Setter from a setting function.
--
-- @
-- 'Sets_' :: ((a ~> b) ~> (s ~> t)) -> 'ASetter' s t a b
-- @
type Sets_   f   = SetsSym1 f

-- | Create a Lens from a setter and a getter.
--
-- @
-- 'MkLens_'
--     :: 'Functor' f
--     => (s ~> a)
--     -> (s ~> b ~> t)
--     -> 'LensLike' f s t a b
-- @
type MkLens_ f g = MkLensSym2 f g

-- | "Clone" a polymorphic lens so it can be used as more than one type of
-- thing (getter or setter).
--
-- @
-- 'CloneLens_'
--     :: 'Functor' f
--     => 'LensLike' (Context a b) s t a b
--     -> 'LensLike' f s t a b
-- @
--
-- Useful for writing a function that takes a lens and uses it in more than
-- one way; if you have it take an 'ALens', you can then use 'CloneLens_' to
-- use it as a getter or setter.
type CloneLens_ l = CloneLensSym1 l

-- | "Clone" a polymorphic traversal so it can be used as more than one type of
-- thing (fold, traversal, getter, setter...).
--
-- @
-- 'CloneTraversal_'
--     :: 'Functor' f
--     => 'LensLike' (Bazaar a b) s t a b
--     -> 'LensLike' f s t a b
-- @
--
-- Useful for writing a function that takes a traversal and uses it in more
-- than one way; if you have it take an 'ATraversal', you can then use
-- 'CloneTraversal_' to use it as a fold or traversal or anything else.
type CloneTraversal_ l = CloneTraversalSym1 l

-- | The canonical Traversal for any instance of 'Traversable'.
--
-- @
-- 'Traverse_'
--     :: 'Applicative' f
--     => 'LensLike' f (t a) (t b) a b
-- @
type Traverse_  = TraverseSym0

-- | Create a Fold from a "folding function":
--
-- @
-- 'Folding_'
--     :: ('Foldable' f, 'Monoid' r)
--     => (s ~> f a)
--     -> 'Getting' r s a
-- @
type Folding_ f = FoldingSym1 f

-- | The canonical Fold for any instance of 'Foldable'.
--
-- @
-- 'Folded_'
--     :: 'Monoid' r
--     => 'Getting' r (t a) a
-- @
type Folded_    = FoldedSym0

-- | Lens into the first field of a tuple
type L1_       = L1Sym0

-- | Lens into the second field of a tuple
type L2_       = L2Sym0

-- | @'IxList' i@ is a Traversal into the i-th item into a list.  Defined
-- in terms of 'N' to allow for sane termination guaruntees.
--
-- @
-- 'IxList_'
--     :: 'Applicative' f
--     => 'N'
--     -> 'LensLike'' f [a] a
-- @
type IxList_ i = IxListSym1 i

-- TODO: value level poerators

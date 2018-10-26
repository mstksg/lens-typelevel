{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Lens.Internal (
    Context(..)
  , Bazaar(..)
  , UnBazaar, sUnBazaar
  , Sing (SMkContext, SDone, SMore)
  -- * Defunctionalization SYmbols
  , MkContextSym0, MkContextSym1, MkContextSym2
  , DoneSym0, DoneSym1, MoreSym0, MoreSym1, MoreSym2
  , UnBazaarSym0, UnBazaarSym1, UnBazaarSym2
  ) where

import           Control.Applicative
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Kind
import           Data.Monoid
import           Data.Singletons.Prelude.Const
import           Data.Singletons.Prelude.Foldable hiding (Traverse_)
import           Data.Singletons.Prelude.Function hiding (Const, ConstSym0)
import           Data.Singletons.Prelude.Functor
import           Data.Singletons.Prelude.Identity
import           Data.Singletons.Prelude.Maybe
import           Data.Singletons.Prelude.Monoid
import           Data.Singletons.TH

-- | A partially applied lens
data Context a b t = MkContext (b ~> t) a

type MkContextSym0     = TyCon2 'MkContext
type MkContextSym1 f   = TyCon1 ('MkContext f)
type MkContextSym2 f x = 'MkContext f x

data instance Sing :: forall a b t. Context a b t -> Type where
    SMkContext
        :: Sing f
        -> Sing x
        -> Sing ('MkContext f x)

$(singletonsOnly [d|
  fmapContext :: (t -> q) -> Context a b t -> Context a b q
  fmapContext f (MkContext g x) = MkContext (f . g) x
  |])

instance PFunctor (Context a b) where
    type Fmap f c = FmapContext f c

instance SFunctor (Context a b) where
    sFmap = sFmapContext

-- | A partially applied traversal
data Bazaar a b t = Done t
                  | More a (Bazaar a b (b ~> t))

type DoneSym0   = TyCon1 'Done
type DoneSym1 x = 'Done x

type MoreSym0     = TyCon2 'More
type MoreSym1 x   = TyCon1 ('More x)
type MoreSym2 x b = 'More x b

data instance Sing :: forall a b t. Bazaar a b t -> Type where
    SDone :: Sing x -> Sing ('Done x)
    SMore :: Sing x -> Sing b -> Sing ('More x b)

$(singletonsOnly [d|
  fmapBazaar :: (t -> q) -> Bazaar a b t -> Bazaar a b q
  fmapBazaar f (Done t  ) = Done (f t)
  fmapBazaar f (More x b) = More x (fmapBazaar (f .) b)

  pureBazaar :: t -> Bazaar a b t
  pureBazaar = Done

  liftA2Bazaar :: (t -> r -> s) -> Bazaar a b t -> Bazaar a b r -> Bazaar a b s
  liftA2Bazaar f (Done x  ) c = fmapBazaar (f x) c
  liftA2Bazaar f (More x b) c = More x (liftA2Bazaar (\g r y -> f (g y) r) b c)

  apBazaar :: Bazaar a b (t -> q) -> Bazaar a b t -> Bazaar a b q
  apBazaar (Done f  ) c = fmapBazaar f c
  apBazaar (More a b) c = More a (liftA2Bazaar flip b c)

  unBazaar :: Applicative f => (a -> f b) -> Bazaar a b t -> f t
  unBazaar _ (Done x)   = pure x
  unBazaar f (More x b) = (&) <$> f x <*> unBazaar f b
  |])

instance PFunctor (Bazaar a b) where
    type Fmap f c = FmapBazaar f c

instance SFunctor (Bazaar a b) where
    sFmap = sFmapBazaar

instance PApplicative (Bazaar a b) where
    type Pure x       = PureBazaar x
    type LiftA2 f x y = LiftA2Bazaar f x y
    type f <*> x      = ApBazaar f x

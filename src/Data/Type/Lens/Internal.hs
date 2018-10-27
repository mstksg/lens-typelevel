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

module Data.Type.Lens.Internal (
    Context'(..), PContext, Context
  , Bazaar(..)
  , UnBazaar, sUnBazaar
  , Sing (SMkContext, SDone, SMore)
  -- * Defunctionalization Symbols
  , MkContextSym0, MkContextSym1, MkContextSym2
  , DoneSym0, DoneSym1, MoreSym0, MoreSym1, MoreSym2
  , UnBazaarSym0, UnBazaarSym1, UnBazaarSym2
  ) where

import           Data.Kind
import           Data.Singletons.Prelude.Const
import           Data.Singletons.Prelude.Function hiding (Const, ConstSym0)
import           Data.Singletons.Prelude.Functor
import           Data.Singletons.TH

-- | A partially applied lens
data Context' p a b t = MkContext (p @@ b @@ t) a

type PContext = Context' (~>@#@$)
type Context  = Context' (TyCon2 (->))

data MkContextSym0 :: (b ~> t) ~> a ~> PContext a b t
data MkContextSym1 :: (b ~> t) -> a ~> PContext a b t

type instance Apply MkContextSym0 f = MkContextSym1 f
type instance Apply (MkContextSym1 f) x = 'MkContext f x

-- type MkContextSym0     = (TyCon2 'MkContext :: (b ~> t) ~> a ~> PContext a b t)
-- type MkContextSym1 (f :: b ~> t) = (TyCon1 ('MkContext f) :: a ~> PContext a b t)
type MkContextSym2 (f :: b ~> t) (x :: a) = ('MkContext f x :: PContext a b t)

data instance Sing :: forall a b t. PContext a b t -> Type where
    SMkContext
        :: Sing (f :: b ~> t)
        -> Sing (x :: a)
        -> Sing ('MkContext f x :: PContext a b t)

instance (SingKind a, SingKind b, SingKind t) => SingKind (PContext a b t) where
    type Demote (PContext a b t) = Context (Demote a) (Demote b) (Demote t)
    fromSing (SMkContext f x) = MkContext (fromSing f) (fromSing x)
    toSing (MkContext f x) = withSomeSing f $ \sF ->
                             withSomeSing x $ \sX ->
        SomeSing $ SMkContext sF sX

$(singletonsOnly [d|
  fmapContext :: (t -> q) -> PContext a b t -> PContext a b q
  fmapContext f (MkContext g x) = MkContext (f . g) x
  |])

instance PFunctor (PContext a b) where
    type Fmap f c = FmapContext f c

instance SFunctor (PContext a b) where
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

  unBazaar :: Applicative f => (a -> f b) -> Bazaar a b t -> f t
  unBazaar _ (Done x  ) = pure x
  unBazaar f (More x b) = liftA2 (&) (f x) (unBazaar f b)
  |])

instance PFunctor (Bazaar a b) where
    type Fmap f c = FmapBazaar f c

instance SFunctor (Bazaar a b) where
    sFmap = sFmapBazaar

instance PApplicative (Bazaar a b) where
    type Pure x       = PureBazaar x
    type LiftA2 f x y = LiftA2Bazaar f x y

instance SApplicative (Bazaar a b) where
    sPure   = sPureBazaar
    sLiftA2 = sLiftA2Bazaar

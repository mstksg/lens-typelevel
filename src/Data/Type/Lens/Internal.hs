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

module Data.Type.Lens.Internal (
    Context'(..), PContext, Context
  , UnContext, sUnContext, unContext
  , Bazaar'(..), PBazaar, Bazaar
  , UnBazaar, sUnBazaar, unBazaar
  , Sing (SMkContext, SDone, SMore)
  -- * Defunctionalization Symbols
  , MkContextSym0, MkContextSym1, MkContextSym2
  , UnContextSym0, UnContextSym1, UnContextSym2
  , DoneSym0, DoneSym1, MoreSym0, MoreSym1, MoreSym2
  , UnBazaarSym0, UnBazaarSym1, UnBazaarSym2
  ) where

import           Control.Applicative
import           Data.Function
import           Data.Kind
import           Data.Singletons.Prelude.Const
import           Data.Singletons.Prelude.Function hiding (Const, ConstSym0)
import           Data.Singletons.Prelude.Functor
import           Data.Singletons.TH

-- | A partially applied lens
data Context' p a b t = MkContext (p @@ b @@ t) a

type PContext = Context' (~>@#@$)
type Context  = Context' (TyCon2 (->))

type MkContextSym0     = TyCon2 'MkContext
type MkContextSym1 f   = TyCon1 ('MkContext f)
type MkContextSym2 f x = 'MkContext f x

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
  instance Functor (PContext a b) where
    fmap f (MkContext g x) = MkContext (f . g) x

  unContext :: ((b -> t) -> a -> r) -> PContext a b t -> r
  unContext f (MkContext g x) = f g x
  |])

instance Functor (Context a b) where
  fmap f (MkContext g x) = MkContext (f . g) x

unContext :: ((b -> t) -> a -> r) -> Context a b t -> r
unContext f (MkContext g x) = f g x

-- | A partially applied traversal
data Bazaar' p a b t = Done t
                     | More a (Bazaar' p a b (p @@ b @@ t))

type PBazaar = Bazaar' (~>@#@$)
type Bazaar  = Bazaar' (TyCon2 (->))

type DoneSym0   = TyCon1 'Done
type DoneSym1 x = 'Done x

data MoreSym0 :: a ~> PBazaar a b (b ~> t) ~> PBazaar a b t
data MoreSym1 :: a -> PBazaar a b (b ~> t) ~> PBazaar a b t
type MoreSym2 x b = 'More x b

type instance Apply MoreSym0     x = MoreSym1 x
type instance Apply (MoreSym1 x) z = 'More x z

data instance Sing :: forall a b t. PBazaar a b t -> Type where
    SDone :: Sing (x :: t)
          -> Sing ('Done x :: PBazaar a b t)
    SMore :: Sing (x :: a)
          -> Sing (z :: PBazaar a b (b ~> t))
          -> Sing ('More x z :: PBazaar a b t)

$(singletonsOnly [d|
  instance Functor (PBazaar a b) where
    fmap f (Done t  ) = Done (f t)
    fmap f (More x b) = More x (fmap (f .) b)

  instance Applicative (PBazaar a b) where
    pure = Done
    liftA2 f (Done x  ) c = f x <$> c
    liftA2 f (More x b) c = More x (liftA2 (\g r y -> f (g y) r) b c)

  unBazaar :: Applicative f => (a -> f b) -> PBazaar a b t -> f t
  unBazaar _ (Done x  ) = pure x
  unBazaar f (More x b) = liftA2 (&) (f x) (unBazaar f b)
  |])

instance Functor (Bazaar a b) where
  fmap f (Done t  ) = Done (f t)
  fmap f (More x b) = More x (fmap (f .) b)

instance Applicative (Bazaar a b) where
  pure = Done
  liftA2 f (Done x  ) c = f x <$> c
  liftA2 f (More x b) c = More x (liftA2 (\g r y -> f (g y) r) b c)

unBazaar :: Applicative f => (a -> f b) -> Bazaar a b t -> f t
unBazaar _ (Done x  ) = pure x
unBazaar f (More x b) = liftA2 (&) (f x) (unBazaar f b)

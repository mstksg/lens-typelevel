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
    Context(..)
  , MkContextSym0, MkContextSym1, MkContextSym2
  , Sing (SMkContext)
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

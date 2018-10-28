{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Data.Type.Lens.Example
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- See source for examples of using type-level lenses from
-- "Data.Type.Lens".
module Data.Type.Lens.Example (
  -- * Prefix functions
    SetExample
  , ViewExample
  , ToExample
  , OverExample
  , TraversalExample
  , NestedExample
  , FoldExample
  , UnsafeExample
  , IxExample
  , CloneExample
  -- * Operators
  , SetExample'
  , ViewExample'
  , ToExample'
  , OverExample'
  , TraversalExample'
  , NestedExample'
  , FoldExample'
  , IxExample'
  , UnsafeExample'
  , CloneExample'
  ) where

import           Data.Singletons.Prelude
import           Data.Singletons.Prelude.Function
import           Data.Type.Lens

-- |
-- >>> :kind! SetExample
-- '( 'True, 6 )
type SetExample       = Set  L1_       'True   '("hello", 6     )

-- |
-- >>> :kind! ViewExample
-- 6
type ViewExample      = View L2_               '("hello", 6     )

-- |
-- >>> :kind! ToExample
-- 6
type ToExample        = View (To_ SndSym0)     '("hello", 6     )

-- |
-- >>> :kind! TraversalExample
-- '( "hello", 'False )
type OverExample      = Over L2_       NotSym0 '("hello", 'True )

-- |
-- >>> :kind! TraversalExample
-- '[ 'False, 'True, 'True ]
type TraversalExample = Over Traverse_ NotSym0 '[ 'True, 'False, 'False ]

-- |
-- >>> :kind! NestedExample
-- 6
type NestedExample    = View (L2_ .@ L1_)      '("hello", '(6, 'False ) )

-- |
-- >>> :kind! FoldExample
-- '["hello", "world", "curry"]
type FoldExample      = ToListOf (Traverse_ .@ L1_)
                          '[ '("hello", 'True )
                           , '("world", 'False)
                           , '("curry", 'False)
                           ]

-- |
-- >>> :kind! UnsafeExample
-- Error "Failed indexing into empty traversal"
type UnsafeExample    = UnsafePreview Traverse_ '[]

-- |
-- >>> :kind! IxExample
-- '["hello", "haskell", "curry"]
type IxExample        = Set   (IxList_ ('S 'Z)) "haskell"
                          '["hello", "world", "curry"]

-- |
-- >>> :kind! CloneExample L1_
-- 'False
-- >>> :kind! CloneExample L2_
-- 'True
type CloneExample l   = View (CloneLens_ l) (Over (CloneLens_ l) NotSym0 '( 'True, 'False ))


type SetExample'       = '("hello", 6     )         &  L1_             .~ 'True
type ViewExample'      = '("hello", 6     )         ^. L2_
type ToExample'        = '("hello", 6     )         ^. To_ SndSym0
type OverExample'      = '("hello", 'True )         &  L2_             %~ NotSym0
type TraversalExample' = '[ 'True, 'False, 'False ] &  Traverse_       %~ NotSym0
type NestedExample'    = '("hello", '(6, 'False ) ) ^. L2_ .@ L1_
type FoldExample'      = '[ '("hello", 'True )
                          , '("world", 'False)
                          , '("curry", 'False)
                          ] ^.. Traverse_ .@ L1_
type UnsafeExample'    = '[] ^?! Traverse_
type IxExample'        = '["hello","world","curry"] &  IxList_ ('S 'Z) .~ "haskell"
type CloneExample' l   = ('( 'True, 'False ) & CloneLens_ l %~ NotSym0) ^. CloneLens_ l

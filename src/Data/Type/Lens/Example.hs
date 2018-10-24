{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Data.Type.Lens.Example (
    SetExample
  , ViewExample
  , ToExample
  , OverExample
  , TraversalExample
  , NestedExample
  , FoldExample
  ) where

import           Data.Singletons.Prelude
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
type NestedExample    = View (L2_ .@#@$$$ L1_) '("hello", '(6, 'False ) )

-- |
-- >>> :kind! FoldExample
-- '["hello", "world", "curry"]
type FoldExample      = ToListOf (Traverse_ .@#@$$$ L1_)
                          '[ '("hello", 'True )
                           , '("world", 'False)
                           , '("curry", 'False)
                           ]

# [lens-typelevel][]

[![lens-typelevel on Hackage](https://img.shields.io/hackage/v/lens-typelevel.svg?maxAge=86400)](https://hackage.haskell.org/package/lens-typelevel)
[![Build Status](https://travis-ci.org/mstksg/lens-typelevel.svg?branch=master)](https://travis-ci.org/mstksg/lens-typelevel)


([Rendered off-hackage documentation][docs])

[docs]: https://mstksg.github.io/lens-typelevel/

van Laarhoven lenses at the type level using *[singletons][]* defunctionalization.

[lens-typelevel]: http://hackage.haskell.org/package/lens-typelevel
[singletons]: https://hackage.haskell.org/package/singletons

```haskell
ghci> :kind! '("hello", 6 ) & L1_ .~ 'True
'( 'True, 6 )

ghci> :kind! '("hello", 6 ) ^. L2_
6

ghci> :kind! '("hello", 6 ) ^. To_ SndSym0
6

ghci> :kind! '("hello", 'True ) & L2_ %~ NotSym0
'("hello", 'False )

ghci> :kind! '[ 'True, 'False, 'False ] & Traverse_ %~ NotSym0
'[ 'False, 'True, 'True ]

ghci> :kind! '("hello", '(6, 'False ) ) ^. L2_ .@ L1_
6

ghci> type TestList = '[ '("hello", 'True), '("world", 'False), '("curry", 'False)]
ghci> :kind! TestLst ^.. Traverse_ .@ L1_
'["hello", "world", "curry"]

ghci> :kind! '[] ^?! Traverse_
Error "Failed indexing into empty traversal"

ghci> :kind! '["hello", "world", "curry"] & IxList_ ('S 'Z) .~ "haskell"
'["hello", "haskell", "curry"]
```

It's pretty much the exact same representation as the *lens* library; it's
essentially an API-faithful port with the same representation and essentially
the same implementation.  We even have `CloneLens_` and `CloneTraversal_`
implemented using type-level versions of `Context` and `Bazaar`:

```haskell
ghci> type CloneExample l   = ('( 'True, 'False ) & CloneLens_ l %~ NotSym0)
                                                  ^. CloneLens_ l
ghci> :kind! CloneExample L1_
'False
ghci> :kind! CloneExample L2_
'True
```

Using prefix function names:

```haskell
ghci> :kind! Set  L1_       'True        '("hello", 6     )
'( 'True, 6 )

ghci> :kind! View L2_                    '("hello", 6     )
6

ghci> :kind! View (To_ SndSym0)          '("hello", 6     )
6

ghci> :kind! Over L2_       NotSym0      '("hello", 'True )
'("hello", 'False )

ghci> :kind! Over Traverse_ NotSym0      '[ 'True, 'False, 'False ]
'[ 'False, 'True, 'True ]

ghci> :kind! View (L2_ .@ L1_)           '("hello", '(6, 'False ) )
6

ghci> type TestList = '[ '("hello", 'True), '("world", 'False), '("curry", 'False)]
ghci> :kind! ToListOf (Traverse_ .@ L1_) TestList
'["hello", "world", "curry"]

ghci> :kind! UnsafePreview Traverse_     '[]
Error "Failed indexing into empty traversal"

ghci> :kind! Set (IxList_ ('S 'Z)) "haskell" '["hello", "world", "curry"]
'["hello", "haskell", "curry"]
```

Defining lenses
---------------

There are two main ways to define optics.

First, you can write them by hand using `singletonsOnly`:

```haskell
$(singletonsOnly [d|
  l1 :: Functor f => LensLike (a, c) (b, c) a b
  l1 f (x, y) = (\x' -> (x', y)) <$> f x

  l1Alt :: Functor f => LensLike (a, c) (b, c) a b
  l1Alt = mkLens fst (\(_, y) x -> (x', y))

  getFirst :: Getting a (a, b) a
  getFirst = to fst
  |])
```

This creates the *type families* `L1`, `L1Alt`, and `GetFirst`; however, these
aren't lenses, because they aren't partially applied.  The lactual lenses are
`L1Sym0`, `L1AltSym0`, and `GetFirstSym0`.  As a convention, I
recommend aliasing the *actual* lenses with an underscore suffix:

```haskell
-- L1_       :: Functor f => LensLike f (a, c) (b, c) a b
type L1_       = L1Sym0

-- L1Alt_    :: Functor f => LensLike f (a, c) (b, c) a b
type L1Alt     = L1AltSym0

-- GetFirst_ :: Getting a (a, b) a
type GetFirst_ = GetFirstSym0
```

The number after the `Sym` is determined by how many arguments you need to
apply to your function before you get to the actual lens.  For example,
`IxList` requires one argument (the index) to get to the actual traversal, so
the definition in the library is:

```haskell
type IxList_ i = IxListSym1 i
```

Second, you can write them directly at the type level using combinators like
`MkLens_` and `To_`:

```haskell
type GetFirst_ = To_ FstSym0
```

(`FstSym0` is the promotion of `fst` from the *singletons* library)

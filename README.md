# [lens-typelevel][]

van Laarhoven lenses at the type level using *[singletons][]* defunctionalization.

[lens-typelevel]: https://mstksg.github.io/lens-typelevel
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
```

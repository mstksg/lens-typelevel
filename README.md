# lens-typelevel

```haskell
ghci> :kind! Set  L1_       'True   '("hello", 6     )
'( 'True, 6 )

ghci> :kind! View L2_               '("hello", 6     )
6

ghci> :kind! Over L2_       NotSym0 '("hello", 'True )
6

ghci> :kind! Over Traverse_ NotSym0 '[ 'True, 'False, 'False ]
'[ 'False, 'True, 'True ]

ghci> :kind! View (L2_ .@#@$$$ L1_) '("hello", '(6, 'False ) )
6

ghci> type TestList = '[ '("hello", 'True), '("world", 'False), '("curry", 'False)]
ghci> :kind! ToListOf (Traverse_ .@#@$$$ L1_) TestList
'["hello", "world", "curry"]
```

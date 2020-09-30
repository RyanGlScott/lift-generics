## 0.2 [2020.09.30]
* `genericLiftTyped` and `genericLiftTypedWithPkg` now return a `Code` instead
  of a `TExp` to reflect the type of `liftTyped` changing in
  `template-haskell-2.17.0.0`. New functions `genericLiftTypedTExp` and
  `genericLiftTypedTExpWithPkg` have been added for those who wish to return
  `TExp` specifically. In addition, the functions `genericLiftTypedCompat` and
  `genericLiftTypedCompatWithPkg` have been introduced which return a `Code`
  on `template-haskell-2.17.0.0` or later, but a `TExp` on older versions of
  `template-haskell`. These functions are most useful for implementing
  `liftTyped` in a `Lift` instance in a backwards-compatible way.

  The `th-compat` library is used to backport the `Code` data type back to
  versions of `template-haskell` that do not define it.
* The functions in `Language.Haskell.TH.Lift.Generics` are now generalized to
  work over any `Quote` instance instead of hardcoding `Q`. Again, the
  `th-compat` library is used to backport `Quote` to old versions of
  `template-haskell` that do not define it.
* Make `genericLift` work properly for empty data types.

### 0.1.3 [2019.11.26]
* Add `genericLiftTyped` and `genericLiftTypedWithPkg`.

### 0.1.2
* Add a case for `V1` (i.e., empty data types), which diverges in the way
  you'd expect.

### 0.1.1
* Fix test suite on GHC 8.0

## 0.1
* Initial commit

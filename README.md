# `lift-generics`

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/lift-generics.svg)](http://packdeps.haskellers.com/reverse/lift-generics)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Build Status](https://github.com/RyanGlScott/lift-generics/workflows/Haskell-CI/badge.svg)](https://github.com/RyanGlScott/lift-generics/actions?query=workflow%3AHaskell-CI)

[Hackage: lift-generics]:
  http://hackage.haskell.org/package/lift-generics
    "lift-generics package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

This package provides a `GHC.Generics`-based `genericLiftWithPkg` function (intended for GHC 7.10 and earlier), as well as a `genericLift` function (only available on GHC 8.0 and later), both of which can be used for providing a `Language.Haskell.TH.Syntax.lift` implementation. See the documentation in the `Language.Haskell.TH.Lift.Generics` module to get started.

Credit goes to Matthew Pickering for [suggesting this idea](https://ghc.haskell.org/trac/ghc/ticket/1830#comment:12).

Note that due to API limitations, `GHC.Generics` wasn't powerful enough to come up with the entirety of a `lift` implementation prior to GHC 8.0. For this reason, `genericLiftWithPkg` requires you to produce the package name yourself, which proves to be no small feat (see the documentation for more info).

Luckily, you don't have to jump through as many hoops on GHC 8.0 and later: simply use the `genericLift` function, and life is good.

{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE EmptyCase            #-}
#endif

{-|
Module:      Language.Haskell.TH.Lift.Generics
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott

"GHC.Generics"-based approach to implementing `lift`.
-}
module Language.Haskell.TH.Lift.Generics (
      genericLiftWithPkg
#if MIN_VERSION_template_haskell(2,9,0)
    , genericLiftTypedWithPkg
#endif
#if __GLASGOW_HASKELL__ >= 711
    , genericLift
    , genericLiftTyped
#endif
    , GLift(..)
    , GLiftDatatype(..)
    , GLiftArgs(..)
    -- * 'Lift' reexport
    , Lift(..)
    ) where

#if MIN_VERSION_template_haskell(2,8,0)
import Data.Char (ord)
import Data.Word (Word8)
#endif

import Generics.Deriving

import GHC.Base (unpackCString#)
import GHC.Exts

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

#undef CURRENT_PACKAGE_KEY
-- | "GHC.Generics"-based 'lift' implementation.
--
-- API limitations of early versions of GHC (7.10 and earlier) require the user
-- to produce the package name themselves. This isn't as easy to come up with as
-- it sounds, because GHC 7.10 uses a hashed package ID for a name. To make things
-- worse, if you produce the wrong package name, you might get bizarre compilation
-- errors!
--
-- There's no need to fear, though—the code sample below shows an example of how to
-- properly use 'genericLiftWithPkg' without shooting yourself in the foot:
--
-- @
-- &#123;-&#35; LANGUAGE CPP, DeriveGeneric &#35;-&#125;
-- -- part of package foobar
-- module Foo where
--
-- import GHC.Generics
-- import Language.Haskell.Lift.Generics
--
-- &#35;ifndef CURRENT_PACKAGE_KEY
-- import Data.Version (showVersion)
-- import Paths_foobar (version)
-- &#35;endif
--
-- pkgName :: String
-- &#35;ifdef CURRENT_PACKAGE_KEY
-- pkgName = CURRENT_PACKAGE_KEY
-- &#35;else
-- pkgName = "foobar-" ++ showVersion version
-- &#35;endif
--
-- data Foo = Foo Int Char String
--   deriving Generic
--
-- instance Lift Foo where
--   lift = genericLiftWithPkg pkgName
-- @
--
-- As you can see, this trick only works if (1) the current package key is known
-- (i.e., the 'Lift' instance is defined in the same package as the datatype), or
-- (2) you're dealing with a package that has a fixed package name (e.g., @base@,
-- @ghc-prim@, @template-haskell@, etc.).
--
-- Once the @Lift Foo@ instance is defined, you can splice @Foo@ values directly
-- into Haskell source code:
--
-- @
-- &#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;
-- module Bar where
--
-- import Foo
-- import Language.Haskell.TH.Syntax
--
-- foo :: Foo
-- foo = $(lift (Foo 1 'a' "baz"))
-- @
genericLiftWithPkg :: (Generic a, GLift (Rep a)) => String -> a -> Q Exp
genericLiftWithPkg pkg = glift pkg . from

#if MIN_VERSION_template_haskell(2,9,0)
-- | Like 'genericLiftWithPkg', but returns a 'TExp' instead of an 'Exp'.
genericLiftTypedWithPkg :: (Generic a, GLift (Rep a)) => String -> a -> Q (TExp a)
genericLiftTypedWithPkg pkg = unsafeTExpCoerce . genericLiftWithPkg pkg
#endif

#if __GLASGOW_HASKELL__ >= 711
-- | "GHC.Generics"-based 'lift' implementation. Only available on GHC 8.0 and later
-- due to API limitations of earlier GHC versions.
--
-- Unlike 'genericLiftWithPkg', this function does all of the work for you:
--
-- @
-- &#123;-&#35; LANGUAGE DeriveGeneric &#35;-&#125;
-- module Foo where
--
-- import GHC.Generics
-- import Language.Haskell.Lift.Generics
--
-- data Foo = Foo Int Char String
--   deriving Generic
--
-- instance Lift Foo where
--   lift = genericLift
-- @
--
-- Now you can splice @Foo@ values directly into Haskell source code:
--
-- @
-- &#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;
-- module Bar where
--
-- import Foo
-- import Language.Haskell.TH.Syntax
--
-- foo :: Foo
-- foo = $(lift (Foo 1 'a' "baz"))
-- @
genericLift :: (Generic a, GLift (Rep a)) => a -> Q Exp
genericLift = glift "" . from

-- | Like 'genericLift', but returns a 'TExp' instead of an 'Exp'.
genericLiftTyped :: (Generic a, GLift (Rep a)) => a -> Q (TExp a)
genericLiftTyped = unsafeTExpCoerce . genericLift
#endif

-- | Class of generic representation types which can be converted to Template
-- Haskell expressions. You shouldn't need to use this typeclass directly; it is
-- only exported for educational purposes.
class GLift f where
    glift :: String -- ^ The package name (not used on GHC 8.0 and later)
          -> f a    -- ^ The generic value
          -> Q Exp  -- ^ The resulting Template Haskell expression

instance (Datatype d, GLiftDatatype f) => GLift (D1 d f) where
    glift _pkg d@(M1 x) = gliftWith pName mName x
      where
        pName, mName :: String
#if __GLASGOW_HASKELL__ >= 711
        pName = packageName d
#else
        pName = _pkg
#endif
        mName = moduleName d

-- | Class of generic representation types which can be converted to Template
-- Haskell expressions, given a package and module name. You shouldn't need to use
-- this typeclass directly; it is only exported for educational purposes.
class GLiftDatatype f where
    gliftWith :: String -- ^ The package name
              -> String -- ^ The module name
              -> f a    -- ^ The generic value
              -> Q Exp  -- ^ The resulting Template Haskell expression

instance (Constructor c, GLiftArgs f) => GLiftDatatype (C1 c f) where
    gliftWith pName mName c@(M1 x) =
      appsE (conE (mkNameG_d pName mName cName) : gliftArgs x)
      where
        cName :: String
        cName = conName c

instance (GLiftDatatype f, GLiftDatatype g) => GLiftDatatype (f :+: g) where
    gliftWith pName mName (L1 l) = gliftWith pName mName l
    gliftWith pName mName (R1 r) = gliftWith pName mName r

-- | Class of generic representation types which can be converted to a list of
-- Template Haskell expressions (which represent a constructors' arguments). You
-- shouldn't need to use this typeclass directly; it is only exported for educational
-- purposes.
class GLiftArgs f where
    gliftArgs :: f a -> [Q Exp]

instance GLiftArgs V1 where
    gliftArgs x =
      (:[]) $ return $ case x of
#if __GLASGOW_HASKELL__ >= 708
                         {}
#else
                         !_ -> undefined
#endif

instance GLiftArgs U1 where
    gliftArgs U1 = []

instance Lift c => GLiftArgs (K1 i c) where
    gliftArgs (K1 x) = [lift x]

instance GLiftArgs f => GLiftArgs (S1 s f) where
    gliftArgs (M1 x) = gliftArgs x

instance (GLiftArgs f, GLiftArgs g) => GLiftArgs (f :*: g) where
    gliftArgs (f :*: g) = gliftArgs f ++ gliftArgs g

instance GLiftArgs UAddr where
    gliftArgs (UAddr a) = [litE (stringPrimL (word8ify (unpackCString# a)))]
      where
#if MIN_VERSION_template_haskell(2,8,0)
        word8ify :: String -> [Word8]
        word8ify = map (fromIntegral . ord)
#else
        word8ify :: String -> String
        word8ify = id
#endif

#if MIN_VERSION_template_haskell(2,11,0)
instance GLiftArgs UChar where
    gliftArgs (UChar c) = [litE (charPrimL (C# c))]
#endif

instance GLiftArgs UDouble where
    gliftArgs (UDouble d) = [litE (doublePrimL (toRational (D# d)))]

instance GLiftArgs UFloat where
    gliftArgs (UFloat f) = [litE (floatPrimL (toRational (F# f)))]

instance GLiftArgs UInt where
    gliftArgs (UInt i) = [litE (intPrimL (toInteger (I# i)))]

instance GLiftArgs UWord where
    gliftArgs (UWord w) = [litE (wordPrimL (toInteger (W# w)))]

{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
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

"GHC.Generics"-based approach to implementing `lift`. Different sets of
functions are available for use with different GHC and @template-haskell@
versions.

=== General recommendations

* If you only need to support GHC 8.0 (@base@ 4.9.0) and later, then you may
either use the \"friendly\" functions here or skip this package entirely and use
the built-in `DeriveLift` mechanism, which has the advantage of working for
GADTs and existential types.

* If you only need to support GHC 7.4 (@base@ 4.5) and later, and you have
'Typeable' instances for the relevant type constructors (or can derive them),
then you should use the \"friendly\" functions here.

* If you must support GHC versions before 7.4 (@base@ 4.5), or types in other
packages without 'Typeable' instances, then you should seriously consider using
the
<https://hackage.haskell.org/package/th-lift th-lift>
package to derive 'Lift' instances. If you choose to continue with this package:

    * If you have 'Typeable' instances, then you should use the \"less friendly\"
    functions here. These take an argument for the package name, but they ignore it
    (acting just like the friendly ones) for GHC 7.4 (@base@ 4.5) and later.

    * If you lack a 'Typeable' instance for a type constructor, then you'll
    need to use the \"unfriendly\" functions. These rely solely on the
    user-provided package name. On GHC 7.10, it is extremely difficult to
    obtain the correct package name for an external package.

=== A note on 'Typeable' constraints

For relevant versions, the \"friendly\" and \"less friendly\" functions require
the type to satisfy an @OuterTypeable@ constraint.  A type is @OuterTypable@ if
its /type constructor/ is 'Typeable'. For example, @'Maybe' a@ is only
'Typeable' if @a@ is 'Typeable', but it is always @OuterTypeable@.

Before GHC 7.8, only the last seven arguments are fully
supported by @OuterTypeable@. So given a type

@
data Foo a b c d e f g h i = ...
  deriving ('Typeable')
@

@OuterTypeable (Foo a b c d e f g h i)@ requires that @a@
and @b@ be 'Typeable', but the rest of the arguments need
not be. In practice, this means that if you use these
functions to write a 'Lift' instance for a type with more
than seven parameters on GHC < 7.8, then you'll need
'Typeable' constraints on the first few.
-}
module Language.Haskell.TH.Lift.Generics (
#if MIN_VERSION_base(4,5,0)
    -- * Friendly "GHC.Generics"-based 'lift' implementations
    --
    -- $friendlyFunctions
      genericLift
# if MIN_VERSION_template_haskell(2,9,0)
    , genericLiftTyped
    , genericLiftTypedTExp
    , genericLiftTypedCompat
# endif
    ,
#endif

    -- * Less friendly and unfriendly "GHC.Generics"-based 'lift'
    -- implementations
    --
    -- $lessFriendlyFunctions

    -- ** Less friendly implementations
    --
    -- | These implementations should be used when support for versions
    -- before GHC 7.4 (@base@ 4.5) is required, but a 'Data.Typeable.Typeable'
    -- instance is available. The 'Data.Typeable.Typeable' instance will be used
    -- to get the package name for GHC 7.4 and later.
      genericLiftWithPkgFallback
#if MIN_VERSION_template_haskell(2,9,0)
    , genericLiftTypedWithPkgFallback
    , genericLiftTypedTExpWithPkgFallback
    , genericLiftTypedCompatWithPkgFallback
#endif

    -- ** Unfriendly implementations
    --
    -- | These implementations should be used when support for versions
    -- before GHC 8.0 (@base@ 4.9) is required and a 'Data.Typeable.Typeable' instance
    -- is /not/ available. These functions are termed "unfriendly" because
    -- they're extremely hard to use under GHC 7.10 when working with types
    -- defined in other packages. The only way to do so is to use a 'Typeable'
    -- type in the same package to get the \"package name\", which will be a
    -- hash value.
    , genericLiftWithPkg
#if MIN_VERSION_template_haskell(2,9,0)
    , genericLiftTypedWithPkg
    , genericLiftTypedTExpWithPkg
    , genericLiftTypedCompatWithPkg
#endif
    -- * 'Generic' classes
    --
    -- | You shouldn't need to use any of these
    -- classes directly; they are only exported for educational purposes.
    , GLift(..)
    , GLiftDatatype(..)
    , GLiftArgs(..)
    -- * 'Lift' reexport
    , Lift(..)
    ) where

#if MIN_VERSION_base(4,5,0) && !MIN_VERSION_base(4,9,0)
import Language.Haskell.TH.Lift.Generics.Internal.OuterTypeable
#endif

import Control.Monad (liftM, (>=>))

import Generics.Deriving

import GHC.Base (unpackCString#)
import GHC.Exts (Double(..), Float(..), Int(..), Word(..))
#if __GLASGOW_HASKELL__ < 708
import GHC.Conc (pseq)
#endif

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Syntax.Compat

#if MIN_VERSION_template_haskell(2,8,0)
import Data.Char (ord)
import Data.Word (Word8)
#endif

#if MIN_VERSION_template_haskell(2,11,0)
import GHC.Exts (Char(..))
#endif

#if MIN_VERSION_base(4,5,0) && !MIN_VERSION_base(4,9,0)
import qualified Data.Typeable as T
#endif

#if MIN_VERSION_base(4,5,0) && !MIN_VERSION_base(4,9,0)
import Data.Proxy (Proxy (..))
#endif

-- We don't want to expand this in the Haddocks!
#undef CURRENT_PACKAGE_KEY

-- $lessFriendlyFunctions
--
-- API limitations of early versions of GHC (7.2 and earlier) require the user
-- to produce the package name themselves. This is also occasionally necessary
-- for later versions of GHC when dealing with types from other packages that
-- lack 'Data.Typeable.Typeable' instances. The package name isn't always as easy to come
-- up with as it sounds, especially because GHC 7.10 uses a hashed package ID
-- for that name. To make things worse, if you produce the wrong package name,
-- you might get bizarre compilation errors!
--
-- There's no need to fear, though—in most cases it's possible to obtain the
-- correct package name anyway, at least for types defined /in the current package/.
-- When compiling a library with @Cabal@, the current package
-- name is obtained using the CPP macro @CURRENT_PACKAGE_KEY@. When compiling
-- an application, or compiling without Cabal, it takes a bit more work to get
-- the name. The code sample below shows an example of how to
-- properly use 'genericLiftWithPkgFallback' or 'genericLiftWithPkg' without
-- shooting yourself in the foot:
--
-- @
-- &#123;-&#35; LANGUAGE CPP, DeriveGeneric, DeriveDataTypeable &#35;-&#125;
-- -- part of package foobar
-- module Foo where
--
-- import GHC.Generics
-- import Data.Typeable
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
--   deriving (Generic, Typeable)
--
-- instance Lift Foo where
--   lift = genericLiftWithPkgFallback pkgName
-- &#35;if MIN&#95;VERSION&#95;template_haskell(2,9,0)
--   liftTyped = genericLiftTypedCompatWithPkgFallback pkgName
-- &#35;endif
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
-- foo = $(lift (Foo 1 \'a\' \"baz\"))
-- @

-- | Generically produce an implementation of 'lift', given a user-provided
-- (but correct!) package name. The provided package name is ignored for
-- GHC 7.4 (@base@ 4.5) and later.
--
-- === Note
--
-- A @'Data.Typeable.Typeable' a@ instance is required for 7.4 <= GHC < 8.0 (4.5 <= @base@ < 4.9).
#if MIN_VERSION_base (4,9,0) || !MIN_VERSION_base (4,5,0)
genericLiftWithPkgFallback :: (Quote m, Generic a, GLift (Rep a)) => String -> a -> m Exp
#else
genericLiftWithPkgFallback :: (Quote m, Generic a, GLift (Rep a), OuterTypeable a) => String -> a -> m Exp
#endif
#if MIN_VERSION_base(4,5,0)
genericLiftWithPkgFallback _pkg = genericLift
#else
genericLiftWithPkgFallback = genericLiftWithPkg
#endif

#if MIN_VERSION_template_haskell(2,9,0)
-- | Like 'genericLiftWithPkgFallback', but returns a 'Code' instead of an 'Exp'.
# if MIN_VERSION_base (4,9,0) || !MIN_VERSION_base (4,5,0)
genericLiftTypedWithPkgFallback :: (Quote m, Generic a, GLift (Rep a)) => String -> a -> Code m a
# else
genericLiftTypedWithPkgFallback :: (Quote m, Generic a, GLift (Rep a), OuterTypeable a) => String -> a -> Code m a
# endif
# if MIN_VERSION_base(4,5,0)
genericLiftTypedWithPkgFallback _pkg = genericLiftTyped
# else
genericLiftTypedWithPkgFallback = genericLiftTypedWithPkg
# endif

-- | Like 'genericLiftWithPkgFallback', but returns a 'TExp' instead of an 'Exp'.
# if MIN_VERSION_base (4,9,0) || !MIN_VERSION_base (4,5,0)
genericLiftTypedTExpWithPkgFallback :: (Quote m, Generic a, GLift (Rep a)) => String -> a -> m (TExp a)
# else
genericLiftTypedTExpWithPkgFallback :: (Quote m, Generic a, GLift (Rep a), OuterTypeable a) => String -> a -> m (TExp a)
# endif
# if MIN_VERSION_base(4,5,0)
genericLiftTypedTExpWithPkgFallback _pkg = genericLiftTypedTExp
# else
genericLiftTypedTExpWithPkgFallback = genericLiftTypedTExpWithPkg
# endif

-- | Like 'genericLiftWithPkg', but returns:
--
-- * A 'Code' (if using @template-haskell-2.17.0.0@ or later), or
-- * A 'TExp' (if using an older version of @template-haskell@)
--
-- This function is ideal for implementing the 'liftTyped' method of 'Lift'
-- directly, as its type changed in @template-haskell-2.17.0.0@.
# if MIN_VERSION_base (4,9,0) || !MIN_VERSION_base (4,5,0)
genericLiftTypedCompatWithPkgFallback :: (Quote m, Generic a, GLift (Rep a)) => String -> a -> Splice m a
# else
genericLiftTypedCompatWithPkgFallback :: (Quote m, Generic a, GLift (Rep a), OuterTypeable a) => String -> a -> Splice m a
# endif
# if MIN_VERSION_template_haskell(2,17,0)
genericLiftTypedCompatWithPkgFallback = genericLiftTypedWithPkgFallback
# else
genericLiftTypedCompatWithPkgFallback = genericLiftTypedTExpWithPkgFallback
# endif
#endif

-- ---

-- | Generically produce an implementation of 'lift', given a user-provided
-- (but correct!) package name. The provided package name is ignored for
-- GHC 8.0 (@base@ 4.9) and later.
genericLiftWithPkg :: (Quote m, Generic a, GLift (Rep a)) => String -> a -> m Exp
genericLiftWithPkg pkg = glift pkg . from

#if MIN_VERSION_template_haskell(2,9,0)
-- | Like 'genericLiftWithPkg', but returns a 'Code' instead of an 'Exp'.
genericLiftTypedWithPkg :: (Quote m, Generic a, GLift (Rep a)) => String -> a -> Code m a
genericLiftTypedWithPkg pkg = unsafeCodeCoerce . genericLiftWithPkg pkg

-- | Like 'genericLiftWithPkg', but returns a 'TExp' instead of an 'Exp'.
genericLiftTypedTExpWithPkg :: (Quote m, Generic a, GLift (Rep a)) => String -> a -> m (TExp a)
genericLiftTypedTExpWithPkg pkg = unsafeTExpCoerceQuote . genericLiftWithPkg pkg

-- | Like 'genericLiftWithPkg', but returns:
--
-- * A 'Code' (if using @template-haskell-2.17.0.0@ or later), or
-- * A 'TExp' (if using an older version of @template-haskell@)
--
-- This function is ideal for implementing the 'liftTyped' method of 'Lift'
-- directly, as its type changed in @template-haskell-2.17.0.0@.
genericLiftTypedCompatWithPkg :: (Quote m, Generic a, GLift (Rep a)) => String -> a -> Splice m a
# if MIN_VERSION_template_haskell(2,17,0)
genericLiftTypedCompatWithPkg = genericLiftTypedWithPkg
# else
genericLiftTypedCompatWithPkg = genericLiftTypedTExpWithPkg
# endif
#endif

#if MIN_VERSION_base (4,5,0)
-- $friendlyFunctions
--
-- The functions in this section are nice and simple, but are only available on
-- GHC 7.4.1 (@base@ 4.5) and later due to API limitations of earlier GHC
-- versions. The types of these functions depend slightly on the GHC version.
-- In particular, before GHC 8.0 (@base@ 4.9), these functions have
-- 'Data.Typeable.Typeable' constraints in addition to 'GHC.Generic.Generic'
-- ones.
--
-- These functions do all of the work for you:
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
-- &#35;if MIN&#95;VERSION&#95;template_haskell(2,9,0)
--   liftTyped = genericLiftTypedCompat
-- &#35;endif
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
-- foo = $(lift (Foo 1 \'a\' \"baz\"))
-- @

-- | Produce a generic definition of 'lift'.
--
-- === Note
--
-- A @'Data.Typeable.Typeable' a@ instance is required for GHC < 8.0 (@base@ < 4.9).
# if MIN_VERSION_base (4,9,0)
genericLift :: (Quote m, Generic a, GLift (Rep a)) => a -> m Exp
genericLift = glift "" . from
# else
genericLift :: forall m a. (Quote m, Generic a, GLift (Rep a), OuterTypeable a) => a -> m Exp
genericLift =
  glift (T.tyConPackage (T.typeRepTyCon (getConTR (Proxy :: Proxy a))))
    . from
# endif

# if MIN_VERSION_template_haskell(2,9,0)
-- | Like 'genericLift', but returns a 'Code' instead of an 'Exp'.
#  if MIN_VERSION_base (4,9,0)
genericLiftTyped :: (Quote m, Generic a, GLift (Rep a)) => a -> Code m a
#  else
genericLiftTyped :: (Quote m, Generic a, GLift (Rep a), OuterTypeable a) => a -> Code m a
#  endif
genericLiftTyped = unsafeCodeCoerce . genericLift

-- | Like 'genericLift', but returns a 'TExp' instead of an 'Exp'.
#  if MIN_VERSION_base (4,9,0)
genericLiftTypedTExp :: (Quote m, Generic a, GLift (Rep a)) => a -> m (TExp a)
#  else
genericLiftTypedTExp :: (Quote m, Generic a, GLift (Rep a), OuterTypeable a) => a -> m (TExp a)
#  endif
genericLiftTypedTExp = unsafeTExpCoerceQuote . genericLift

-- | Lift 'genericLift', but returns:
--
-- * A 'Code' (if using @template-haskell-2.17.0.0@ or later), or
-- * A 'TExp' (if using an older version of @template-haskell@)
--
-- This function is ideal for implementing the 'liftTyped' method of 'Lift'
-- directly, as its type changed in @template-haskell-2.17.0.0@.
#  if MIN_VERSION_base (4,9,0)
genericLiftTypedCompat :: (Quote m, Generic a, GLift (Rep a)) => a -> Splice m a
#  else
genericLiftTypedCompat :: (Quote m, Generic a, GLift (Rep a), OuterTypeable a) => a -> Splice m a
#  endif
#  if MIN_VERSION_template_haskell(2,17,0)
genericLiftTypedCompat = genericLiftTyped
#  else
genericLiftTypedCompat = genericLiftTypedTExp
#  endif
# endif
#endif

-- | Class of generic representation types which can be converted to Template
-- Haskell expressions.
class GLift f where
    glift :: Quote m
          => String -- ^ The package name (not used on GHC 8.0 and later)
          -> f a    -- ^ The generic value
          -> m Exp  -- ^ The resulting Template Haskell expression

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
-- Haskell expressions, given a package and module name.
class GLiftDatatype f where
    gliftWith :: Quote m
              => String -- ^ The package name
              -> String -- ^ The module name
              -> f a    -- ^ The generic value
              -> m Exp  -- ^ The resulting Template Haskell expression

instance GLiftDatatype V1 where
    -- While many instances for void types produce the laziest possible result
    -- (here, something like pure undefined), we choose to be stricter. There
    -- seems little if any benefit to delaying exceptions in this context.
    gliftWith _ _ x =
      case x of
#if __GLASGOW_HASKELL__ >= 708
        {}
#else
        -- pseq ensures that we'll get the exception/non-termination
        -- of v, rather than allowing GHC to "optimize" the function
        -- to gliftWith _ _ _ = undefined, which it would be permitted
        -- to do if we used seq or a bang pattern.
        v -> v `pseq` undefined
#endif

instance (Constructor c, GLiftArgs f) => GLiftDatatype (C1 c f) where
    gliftWith pName mName c@(M1 x) =
      gliftArgs x (ConE (mkNameG_d pName mName cName))
      where
        cName :: String
        cName = conName c

instance (GLiftDatatype f, GLiftDatatype g) => GLiftDatatype (f :+: g) where
    gliftWith pName mName (L1 l) = gliftWith pName mName l
    gliftWith pName mName (R1 r) = gliftWith pName mName r

-- | Class of generic representation types which can conceptually be converted
-- to a list of Template Haskell expressions (which represent a constructors'
-- arguments).
class GLiftArgs f where
    -- | @gliftArgs e f@ applies @f@ to the zero or more arguments represented
    -- by @e@.
    gliftArgs :: Quote m => f a -> Exp -> m Exp

instance GLiftArgs U1 where
    -- This pattern match must be strict, because
    -- lift undefined really shouldn't just happen
    -- to work for unit types.
    gliftArgs U1 = return

instance Lift c => GLiftArgs (K1 i c) where
    gliftArgs (K1 x) h = AppE h `liftM` liftQuote x

instance GLiftArgs f => GLiftArgs (S1 s f) where
    gliftArgs (M1 x) = gliftArgs x

instance (GLiftArgs f, GLiftArgs g) => GLiftArgs (f :*: g) where
    gliftArgs (f :*: g) = gliftArgs f >=> gliftArgs g

instance GLiftArgs UAddr where
    gliftArgs (UAddr a) h = return $ AppE h (LitE (StringPrimL (word8ify (unpackCString# a))))
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
    gliftArgs (UChar c) h = return $ AppE h (LitE (CharPrimL (C# c)))
#endif

instance GLiftArgs UDouble where
    gliftArgs (UDouble d) h = return $ AppE h (LitE (DoublePrimL (toRational (D# d))))

instance GLiftArgs UFloat where
    gliftArgs (UFloat f) h = return $ AppE h (LitE (floatPrimL (toRational (F# f))))

instance GLiftArgs UInt where
    gliftArgs (UInt i) h = return $ AppE h (LitE (IntPrimL (toInteger (I# i))))

instance GLiftArgs UWord where
    gliftArgs (UWord w) h = return $ AppE h (LitE (WordPrimL (toInteger (W# w))))

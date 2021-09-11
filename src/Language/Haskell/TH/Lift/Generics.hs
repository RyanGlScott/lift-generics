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

"GHC.Generics"-based approach to implementing `lift`.
-}
module Language.Haskell.TH.Lift.Generics (
      genericLiftWithPkg
#if MIN_VERSION_template_haskell(2,9,0)
    , genericLiftTypedWithPkg
    , genericLiftTypedTExpWithPkg
    , genericLiftTypedCompatWithPkg
#endif
#if MIN_VERSION_base(4,5,0)
    , genericLift
#if MIN_VERSION_template_haskell(2,9,0)
    , genericLiftTyped
    , genericLiftTypedTExp
    , genericLiftTypedCompat
#endif
#endif
    , GLift(..)
    , GLiftDatatype(..)
    , GLiftArgs(..)
    -- * 'Lift' reexport
    , Lift(..)
    ) where

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
#if MIN_VERSION_base(4,7,0) && !MIN_VERSION_base(4,9,0)
import Data.Proxy (Proxy (..))
#endif

#undef CURRENT_PACKAGE_KEY
-- | "GHC.Generics"-based 'lift' implementation.
--
-- API limitations of early versions of GHC (7.2 and earlier) require the user
-- to produce the package name themselves. This isn't always as easy to come up
-- with as it sounds, because GHC 7.10 uses a hashed package ID for a name. To
-- make things worse, if you produce the wrong package name, you might get
-- bizarre compilation errors!
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
genericLiftWithPkg :: (Quote m, Generic a, GLift (Rep a)) => String -> a -> m Exp
genericLiftWithPkg pkg = glift pkg . from

#if MIN_VERSION_template_haskell(2,9,0)
-- | Like 'genericLiftWithPkg', but returns a 'Code' instead of an 'Exp'.
genericLiftTypedWithPkg :: (Quote m, Generic a, GLift (Rep a)) => String -> a -> Code m a
genericLiftTypedWithPkg pkg = unsafeCodeCoerce . genericLiftWithPkg pkg

-- | Like 'genericLiftWithPkg', but returns a 'TExp' instead of an 'Exp'.
genericLiftTypedTExpWithPkg :: (Quote m, Generic a, GLift (Rep a)) => String -> a -> m (TExp a)
genericLiftTypedTExpWithPkg pkg = unsafeTExpCoerceQuote . genericLiftWithPkg pkg

-- | Lift 'genericLiftWithPkg', but returns:
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
-- | "GHC.Generics"-based 'lift' implementation. Only available on GHC 7.4.1 and later
-- due to API limitations of earlier GHC versions. Note: GHC versions before 8.0
-- have extra 'Data.Typeable.Typeable' constraints on these functions.
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
#if MIN_VERSION_base (4,9,0)
genericLift :: (Quote m, Generic a, GLift (Rep a)) => a -> m Exp
genericLift = glift "" . from
#elif MIN_VERSION_base (4,7,0)
genericLift :: forall m a. (Quote m, Generic a, T.Typeable a, GLift (Rep a)) => a -> m Exp
genericLift =
  glift (T.tyConPackage (T.typeRepTyCon (T.typeRep (Proxy :: Proxy a))))
    . from
#else
genericLift :: forall m a. (Quote m, Generic a, T.Typeable a, GLift (Rep a)) => a -> m Exp
genericLift =
  glift (T.tyConPackage (T.typeRepTyCon (T.typeOf (undefined :: a))))
    . from
#endif

#if MIN_VERSION_template_haskell(2,9,0)
-- | Like 'genericLift', but returns a 'Code' instead of an 'Exp'.
#if MIN_VERSION_base (4,9,0)
genericLiftTyped :: (Quote m, Generic a, GLift (Rep a)) => a -> Code m a
#else
genericLiftTyped :: (Quote m, Generic a, T.Typeable a, GLift (Rep a)) => a -> Code m a
#endif
genericLiftTyped = unsafeCodeCoerce . genericLift

-- | Like 'genericLift', but returns a 'TExp' instead of an 'Exp'.
#if MIN_VERSION_base (4,9,0)
genericLiftTypedTExp :: (Quote m, Generic a, GLift (Rep a)) => a -> m (TExp a)
#else
genericLiftTypedTExp :: (Quote m, Generic a, T.Typeable a, GLift (Rep a)) => a -> m (TExp a)
#endif
genericLiftTypedTExp = unsafeTExpCoerceQuote . genericLift

-- | Lift 'genericLift', but returns:
--
-- * A 'Code' (if using @template-haskell-2.17.0.0@ or later), or
-- * A 'TExp' (if using an older version of @template-haskell@)
--
-- This function is ideal for implementing the 'liftTyped' method of 'Lift'
-- directly, as its type changed in @template-haskell-2.17.0.0@.
#if MIN_VERSION_base (4,9,0)
genericLiftTypedCompat :: (Quote m, Generic a, GLift (Rep a)) => a -> Splice m a
#else
genericLiftTypedCompat :: (Quote m, Generic a, T.Typeable a, GLift (Rep a)) => a -> Splice m a
#endif
# if MIN_VERSION_template_haskell(2,17,0)
genericLiftTypedCompat = genericLiftTyped
# else
genericLiftTypedCompat = genericLiftTypedTExp
# endif
#endif
#endif

-- | Class of generic representation types which can be converted to Template
-- Haskell expressions. You shouldn't need to use this typeclass directly; it is
-- only exported for educational purposes.
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
-- Haskell expressions, given a package and module name. You shouldn't need to use
-- this typeclass directly; it is only exported for educational purposes.
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
-- arguments). You shouldn't need to use this typeclass directly; it is only
-- exported for educational purposes.
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

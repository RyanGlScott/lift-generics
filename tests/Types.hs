{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds                  #-}
#endif

{-|
Module:      Types
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott

Data types for testing `lift-generics`' capabilities.
-}
module Types (
    PureQ, runPureQ
  , Unit(..), Product(..), Sum(..)
  , p, s, u
  ) where

import Control.Monad.State

import Generics.Deriving.TH (deriveAll)

import GHC.Exts

import Language.Haskell.TH.Lift.Generics ( genericLiftWithPkg
#if MIN_VERSION_template_haskell(2,16,0)
                                         , genericLiftTypedCompat
#endif
                                         )
import Language.Haskell.TH.Syntax hiding (newName)
import Language.Haskell.TH.Syntax.Compat

import Prelude ()
import Prelude.Compat

newtype PureQ a = MkPureQ (State Uniq a)
  deriving (Functor, Applicative, Monad, MonadState Uniq)

runPureQ :: PureQ a -> a
runPureQ m = case m of MkPureQ m' -> evalState m' 0

instance Quote PureQ where
  newName s = state $ \i -> (mkNameU s i, i + 1)

data Unit = Unit
  deriving (Eq, Ord, Show)
$(deriveAll ''Unit)

data Product a b c d = Product a b c d
  deriving (Eq, Ord, Show)
$(deriveAll ''Product)

data Sum a b = Inl a | Inr b
  deriving (Eq, Ord, Show)
$(deriveAll ''Sum)

data Unboxed a
   = Unboxed a
#if MIN_VERSION_template_haskell(2,11,0)
             Char#
#endif
             Double#
             Float#
             Int#
             Word#
  deriving (Eq, Ord, Show)
$(deriveAll ''Unboxed)

p :: Product Char Int Bool String
p = Product 'a' 1 True "b"

s :: Sum Char Int
s = Inl 'c'

u :: Unboxed Int
u = Unboxed 1
#if MIN_VERSION_template_haskell(2,11,0)
            '1'#
#endif
            1.0##
            1.0#
            1#
            1##

pkgKey :: String
pkgKey = "main"

instance Lift Unit where
    lift = genericLiftWithPkg pkgKey
#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped = genericLiftTypedCompat
#endif

instance (Lift a, Lift b, Lift c, Lift d) => Lift (Product a b c d) where
    lift = genericLiftWithPkg pkgKey
#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped = genericLiftTypedCompat
#endif

instance (Lift a, Lift b) => Lift (Sum a b) where
    lift = genericLiftWithPkg pkgKey
#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped = genericLiftTypedCompat
#endif

instance Lift a => Lift (Unboxed a) where
    lift = genericLiftWithPkg pkgKey
#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped = genericLiftTypedCompat
#endif

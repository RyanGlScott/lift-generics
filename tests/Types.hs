{-# LANGUAGE CPP             #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds       #-}
#endif

{-|
Module:      Types
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott

Data types for testing `lift-generics`' capabilities.
-}
module Types (Unit(..), Product(..), Sum(..), p, s, u) where

import Generics.Deriving.TH (deriveAll)

import GHC.Exts

import Language.Haskell.TH.Lift.Generics (genericLiftWithPkg)
import Language.Haskell.TH.Syntax (Lift(..))

import Prelude ()
import Prelude.Compat

data Unit = Unit
  deriving (Eq, Ord, Show)
data Product a b c d = Product a b c d
  deriving (Eq, Ord, Show)
data Sum a b = Inl a | Inr b
  deriving (Eq, Ord, Show)
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

instance (Lift a, Lift b, Lift c, Lift d) => Lift (Product a b c d) where
    lift = genericLiftWithPkg pkgKey

instance (Lift a, Lift b) => Lift (Sum a b) where
    lift = genericLiftWithPkg pkgKey

instance Lift a => Lift (Unboxed a) where
    lift = genericLiftWithPkg pkgKey

$(deriveAll ''Unit)
$(deriveAll ''Product)
$(deriveAll ''Sum)
$(deriveAll ''Unboxed)

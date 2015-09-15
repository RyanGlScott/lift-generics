{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

{-|
Module:      Types
Copyright:   (C) 2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott

Data types for testing `lift-generics`' capabilities.
-}
module Types (Empty, Unit(..), Product(..), Sum(..), p, s) where

import Generics.Deriving.TH (deriveAll)

import Language.Haskell.TH.Lift.Generics (genericLiftWithPkg)
import Language.Haskell.TH.Syntax (Lift(..))

import Prelude ()
import Prelude.Compat

data Empty
data Unit = Unit
  deriving (Eq, Show)
data Product a b c d = Product a b c d
  deriving (Eq, Show)
data Sum a b = Inl a | Inr b
  deriving (Eq, Show)

p :: Product Char () Bool String
p = Product 'a' () True "b"

s :: Sum Char ()
s = Inl 'a'

pkgKey :: String
pkgKey = "main"

instance Lift Empty where
    lift = genericLiftWithPkg pkgKey

instance Lift Unit where
    lift = genericLiftWithPkg pkgKey

instance (Lift a, Lift b, Lift c, Lift d) => Lift (Product a b c d) where
    lift = genericLiftWithPkg pkgKey

instance (Lift a, Lift b) => Lift (Sum a b) where
    lift = genericLiftWithPkg pkgKey

$(deriveAll ''Empty)
$(deriveAll ''Unit)
$(deriveAll ''Product)
$(deriveAll ''Sum)

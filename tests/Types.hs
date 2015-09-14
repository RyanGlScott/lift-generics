{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Types (Empty, Unit(..), Product(..), Sum(..), p, s) where

import Generics.Deriving.TH (deriveAll)

import Language.Haskell.TH.Lift.Generics (genericLiftWithPkg)
import Language.Haskell.TH.Syntax (Lift(..))

import Prelude ()
import Prelude.Compat

import Test.QuickCheck (Arbitrary(..), oneof)

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

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
       => Arbitrary (Product a b c d) where
  arbitrary = Product <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = oneof [Inl <$> arbitrary, Inr <$> arbitrary]

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

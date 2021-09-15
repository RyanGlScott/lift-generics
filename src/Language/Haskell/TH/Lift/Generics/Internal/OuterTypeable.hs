{-# language CPP #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 708
{-# language PolyKinds #-}
#endif

#if __GLASGOW_HASKELL__ < 710
{-# language OverlappingInstances #-}
#endif

module Language.Haskell.TH.Lift.Generics.Internal.OuterTypeable
  ( OuterTypeable (..)
  ) where
import Data.Typeable

-- | A type is @OuterTypable@ if its /type constructor/ is
-- 'Typeable'. For example, @'Maybe' a@ is only 'Typeable' if
-- @a@ is 'Typeable', but it is always @OuterTypeable@.
--
-- === Caution
--
-- Before GHC 7.8, only the last seven arguments are properly
-- supported. So given a type
--
-- @
-- data Foo a b c d e f g h i = ...
--   deriving (Typeable)
-- @
--
-- @OuterTypeable (Foo a b c d e f g h i)@ requires that @a@
-- and @b@ be 'Typeable', but the rest of the arguments need
-- not be.
class OuterTypeable a where
  -- | Get the 'TypeRep' corresponding to the outermost constructor
  -- of a type.
  getConTR :: proxy a -> TypeRep

#if __GLASGOW_HASKELL__ >= 708

# if __GLASGOW_HASKELL__ >= 710
instance {-# OVERLAPPING #-} OuterTypeable f => OuterTypeable (f a) where
# else
instance                     OuterTypeable f => OuterTypeable (f a) where
# endif
  getConTR _ = getConTR (Proxy :: Proxy f)

instance Typeable a => OuterTypeable a where
  getConTR = typeRep

-- Why do we need overlapping instances above? Couldn't we use
-- the type family instance selection trick? No, we couldn't. We'd
-- like to be able to write
--
-- type family IsApp (a :: k) :: Bool where
--   IsApp (f a) = 'True
--   IsApp a = 'False
--
-- and switch on that. Unfortunately, that type family wasn't allowed
-- until TypeInType came out in GHC 8.0, which is precisely when we
-- stop needing this module at all.

#else

-- Before GHC 7.8, we didn't have polykinded Typeable, so things were
-- rather less pleasant.

instance Typeable a => OuterTypeable a where
  getConTR _ = typeOf (undefined :: a)
instance Typeable1 p => OuterTypeable (p a) where
  getConTR _ = typeOf1 (undefined :: p a)
instance Typeable2 p => OuterTypeable (p a b) where
  getConTR _ = typeOf2 (undefined :: p a b)
instance Typeable3 p => OuterTypeable (p a b c) where
  getConTR _ = typeOf3 (undefined :: p a b c)
instance Typeable4 p => OuterTypeable (p a b c d) where
  getConTR _ = typeOf4 (undefined :: p a b c d)
instance Typeable5 p => OuterTypeable (p a b c d e) where
  getConTR _ = typeOf5 (undefined :: p a b c d e)
instance Typeable6 p => OuterTypeable (p a b c d e f) where
  getConTR _ = typeOf6 (undefined :: p a b c d e f)
instance Typeable7 p => OuterTypeable (p a b c d e f g) where
  getConTR _ = typeOf7 (undefined :: p a b c d e f g)
#endif

{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Haskell.TH.Lift.Generics (
      genericLiftWithPkg
#if __GLASGOW_HASKELL__ >= 711
    , genericLift
#endif
    , GLift(..)
    , GLiftDatatype(..)
    , GLiftArgs(..)
    ) where

import Generics.Deriving

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax (Exp, Lift(..), Name, Q, mkNameG_d, mkNameG_v)

genericLiftWithPkg :: (Generic a, GLift (Rep a)) => String -> a -> Q Exp
genericLiftWithPkg pkg = glift pkg . from

#if __GLASGOW_HASKELL__ >= 711
genericLift :: (Generic a, GLift (Rep a)) => a -> Q Exp
genericLift = glift "" . from
#endif

class GLift f where
    glift :: String -> f a -> Q Exp

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

class GLiftDatatype f where
    gliftWith :: String -> String -> f a -> Q Exp

instance GLiftDatatype V1 where
    gliftWith _ _ _ = appE (varE errorValName) (litE (stringL "Void lift"))

instance (Constructor c, GLiftArgs f) => GLiftDatatype (C1 c f) where
    gliftWith pName mName c@(M1 x) =
      appsE (conE (mkNameG_d pName mName cName) : gliftArgs x)
      where
        cName :: String
        cName = conName c

instance (GLiftDatatype f, GLiftDatatype g) => GLiftDatatype (f :+: g) where
    gliftWith pName mName (L1 l) = gliftWith pName mName l
    gliftWith pName mName (R1 r) = gliftWith pName mName r

class GLiftArgs f where
    gliftArgs :: f a -> [Q Exp]

instance GLiftArgs U1 where
    gliftArgs U1 = []

instance Lift c => GLiftArgs (K1 i c) where
    gliftArgs (K1 x) = [lift x]

instance GLiftArgs f => GLiftArgs (S1 s f) where
    gliftArgs (M1 x) = gliftArgs x

instance (GLiftArgs f, GLiftArgs g) => GLiftArgs (f :*: g) where
    gliftArgs (f :*: g) = gliftArgs f ++ gliftArgs g

errorValName :: Name
errorValName = mkNameG_v "base" "GHC.Err" "error"

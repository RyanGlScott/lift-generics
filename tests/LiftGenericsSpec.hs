{-# LANGUAGE TemplateHaskell #-}

{-|
Module:      LiftGenericsSpec
Copyright:   (C) 2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott

@hspec@ tests for `lift-generics`.
-}
module LiftGenericsSpec (main, spec) where

import Language.Haskell.TH.Syntax (lift)
import Test.Hspec (Spec, hspec, describe, it, parallel, shouldBe)
import Types (Unit(..), p, s, u)

main :: IO ()
main = hspec spec

description :: String
description = "should equal its lifted counterpart"

spec :: Spec
-- spec = return ()
spec = parallel $ do
    describe "Unit" $
        it description $
            Unit `shouldBe` $(lift Unit)
    describe "Product" $
        it description $
            p `shouldBe` $(lift p)
    describe "Sum" $
        it description $
            s `shouldBe` $(lift s)
    describe "Unboxed" $
        it description $
            u `shouldBe` $(lift u)

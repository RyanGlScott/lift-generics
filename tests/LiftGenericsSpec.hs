{-# LANGUAGE TemplateHaskell #-}

{-|
Module:      LiftGenericsSpec
Copyright:   (C) 2015 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott

@hspec@ tests for `lift-generics`.
-}
module LiftGenericsSpec (main, spec) where

import Control.Exception
import Language.Haskell.TH.Syntax (lift)
import Test.Hspec (Spec, hspec, describe, it, parallel, shouldBe, shouldThrow)
import Types (Empty, Unit(..), p, s)

main :: IO ()
main = hspec spec

description :: String
description = "should equal its lifted counterpart"

spec :: Spec
-- spec = return ()
spec = parallel $ do
    describe "Empty" $
        it "should throw an error" $
            $(lift (undefined :: Empty))
              `shouldThrow` \ErrorCall{} -> True
    describe "Unit" $
        it description $
            Unit `shouldBe` $(lift Unit)
    describe "Product" $
        it description $
            p == $(lift p)
    describe "Sum" $
        it description $
             s == $(lift s)

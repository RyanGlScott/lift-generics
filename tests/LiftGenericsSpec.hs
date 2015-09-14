{-# LANGUAGE TemplateHaskell #-}

module LiftGenericsSpec (main, spec) where

import Control.Exception (ErrorCall(..))
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

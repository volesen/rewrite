module Main where

import Data.Maybe (isJust, isNothing)
import Rewrite.Match.Syntactic (const, func, match, var)
import Test.Hspec
import Prelude hiding (const)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "syntacticMatch" $ do
    it "handles symbol head" $ do
      let pattern = func "f" []
      let subject = func "g" []
      isNothing $ match pattern subject

    it "handles symbol arity" $ do
      let pattern = func "f" [var "x", const "y"]
      let subject = func "f" [const "a", const "b", const "c"]
      isNothing $ match pattern subject

    it "handles symbols in patterns" $ do
      let pattern = func "f" [var "x", const "b"]
      let subject = func "f" [const "a", const "b"]
      isJust $ match pattern subject

    it "handles repeating variables" $ do
      let pattern = func "f" [var "x", var "x"]
      let subject = func "f" [const "a", const "a"]
      isJust $ match pattern subject

    it "handles repeating variables" $ do
      let pattern = func "f" [var "x", const "x"]
      let subject = func "f" [const "a", const "b"]
      isNothing $ match pattern subject
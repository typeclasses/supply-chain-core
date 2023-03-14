module Main (main) where

import Control.Applicative (pure, (<*>))
import Data.Function (($))
import Data.Functor ((<&>), (<$>))
import Data.Functor.Identity (Identity (Identity))
import Data.List qualified as List
import Data.Maybe (Maybe (..))
import Data.Semigroup ((<>))
import Prelude (Int, succ)
import SupplyChain.Core.Connect ((>-))
import SupplyChain.Core.Job (order, perform)
import SupplyChain.Core.Job qualified as Job
import SupplyChain.Core.Referral (Referral (Referral))
import SupplyChain.Core.Vendor (Vendor (Vendor, handle))
import System.IO (IO)
import Test.Hspec

main :: IO ()
main = hspec do

    describe "pure _" do
        it "run Identity" $ Job.run (pure 'a') `shouldBe` Identity 'a'
        it "run Maybe" $ Job.run (pure 'a') `shouldBe` Just 'a'
        it "eval" $ Job.eval (pure 'a') `shouldBe` 'a'

    describe "pure _ <&> _" do
        it "run Identity" $ Job.run (pure 'a' <&> succ) `shouldBe` Identity 'b'
        it "run Maybe" $ Job.run (pure 'a' <&> succ) `shouldBe` Just 'b'
        it "eval" $ Job.eval (pure 'a' <&> succ) `shouldBe` 'b'

    describe "perform" do
        it "Single" $ Job.run (perform ['a', 'b']) `shouldBe` ['a', 'b']
        it "Functor" $ Job.run (perform ['a', 'b'] <&> succ) `shouldBe` ['b', 'c']
        it "Applicative composition" $
            Job.run ((<>) <$> perform ["a", "b"] <*> perform ["c", "d"])
            `shouldBe` ["ac", "ad", "bc", "bd"]

    it "Monadic composition" do
        let j = do
              a <- perform [1 :: Int, 3]
              b <- perform ['a', 'b', 'c']
              perform (List.replicate a b)
        Job.run j `shouldBe` "abcaaabbbccc"

    describe "order" do
        let
            -- Converts dynamic effects to static effects
            f = (go >-)
              where
                go = Vendor { handle = \x -> perform x <&> (`Referral` go) }

        it "Single" $ Job.run (f $ order ['a', 'b']) `shouldBe` ['a', 'b']
        it "Functor" $ Job.run (f $ order ['a', 'b'] <&> succ) `shouldBe` ['b', 'c']
        it "Applicative composition" do
            let j = f $ (<>) <$> order ["a", "b"] <*> order ["c", "d"]
            Job.run j `shouldBe` ["ac", "ad", "bc", "bd"]

        it "Monadic composition" do
            let j = do
                  a <- order [1 :: Int, 3]
                  b <- order ['a', 'b', 'c']
                  order (List.replicate a b)
            Job.run (f j) `shouldBe` "abcaaabbbccc"

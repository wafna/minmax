module Main (main) where

import qualified Data.Map as Map
import qualified Data.Either as Either
import Data.List (intercalate)
import Test.Hspec
import MinMax


minmaxSpec :: Spec
minmaxSpec = it "inferences" pending

main :: IO ()
main = hspec $ do
    minmaxSpec
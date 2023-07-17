module TestBenchmarks where

import Control.Monad (forM_)
import Data.Maybe (fromJust)
import Helper
import System.IO (readFile')
import System.IO.Error (catchIOError, isDoesNotExistError)
import Test.Hspec

fixtureToFilename :: Model -> Alg -> String
fixtureToFilename model alg = fromJust (serializeModel model) ++ "-" ++ show alg ++ ".txt"

models :: [Model]
models = [LR 10, HMM 10, LDA (5, 10)]

algs :: [Alg]
algs = [minBound .. maxBound]

test :: SpecWith ()
test = describe "Benchmarks" $ forM_ models $ \model -> forM_ algs $ testFixture model

testFixture :: Model -> Alg -> SpecWith ()
testFixture model alg = do
  let filename = "test/fixtures/" ++ fixtureToFilename model alg
  it ("should agree with the fixture " ++ filename) $ do
    fixture <- catchIOError (readFile' filename) $ \e ->
      if isDoesNotExistError e
        then return ""
        else ioError e
    sampled <- runAlgFixed model alg
    -- Reset in case of fixture update or creation
    writeFile filename sampled
    fixture `shouldBe` sampled

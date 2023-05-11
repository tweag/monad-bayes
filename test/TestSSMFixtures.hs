module TestSSMFixtures where

import Control.Monad.Bayes.Sampler.Strict (sampleIOfixed)
import NonlinearSSM
import NonlinearSSM.Algorithms
import System.IO (readFile')
import System.IO.Error (catchIOError, isDoesNotExistError)
import Test.Hspec

fixtureToFilename :: Alg -> FilePath
fixtureToFilename alg = "test/fixtures/SSM-" ++ show alg ++ ".txt"

testFixture :: Alg -> SpecWith ()
testFixture alg = do
  let filename = fixtureToFilename alg
  it ("should agree with the fixture " ++ filename) $ do
    ys <- sampleIOfixed $ generateData t
    fixture <- catchIOError (readFile' filename) $ \e ->
      if isDoesNotExistError e
        then return ""
        else ioError e
    sampled <- sampleIOfixed $ runAlgFixed (map fst ys) alg
    -- Reset in case of fixture update or creation
    writeFile filename sampled
    fixture `shouldBe` sampled

test :: SpecWith ()
test = describe "TestSSMFixtures" $ mapM_ testFixture algs

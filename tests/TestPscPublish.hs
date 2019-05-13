{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TestPscPublish where

import Prelude

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.Time.Clock (getCurrentTime)
import qualified Data.Aeson as A
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Version
import Data.Foldable (forM_)
import qualified Text.PrettyPrint.Boxes as Boxes
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.FilePath.Glob (glob)

import Language.PureScript.Docs
import Language.PureScript.Publish
import Language.PureScript.Publish.ErrorsWarnings as Publish

import qualified Language.PureScript.CST as CST
import Language.PureScript.Errors
import Language.PureScript.Make (runMake, make, inferForeignModules, buildMakeActions)
import Language.PureScript.Options (Options(..), defaultOptions, CodegenTarget(..))

import Test.Tasty
import Test.Tasty.Hspec (Spec, Expectation, runIO, context, it, expectationFailure, testSpec)
import TestUtils hiding (inferForeignModules, makeActions)

main :: IO TestTree
main = testSpec "publish" spec

spec :: Spec
spec = do
  context "preparePackage with json roundtrips" $ do
    it "purescript-prelude" $ do
      testPackage
        "tests/support/bower_components/purescript-prelude"
        Nothing
        "../../prelude-resolutions.json"

    it "basic example" $ do
      testPackage
        "tests/purs/publish/basic-example"
        (Just "../../../support/bower_components/")
        "resolutions.json"

    it "basic example with legacy resolutions file" $ do
      testPackage
        "tests/purs/publish/basic-example"
        (Just "../../../support/bower_components/")
        "resolutions-legacy.json"

  context "json compatibility" $ do
    let compatDir = "tests" </> "json-compat"
    versions <- runIO $ listDirectory compatDir
    forM_ versions $ \version -> do
      context ("json produced by " ++ version) $ do
        files <- runIO $ listDirectory (compatDir </> version)
        forM_ files $ \file -> do
          it file $ do
            result <- A.eitherDecodeFileStrict' (compatDir </> version </> file)
            case result of
              Right (_ :: VerifiedPackage) ->
                pure ()
              Left err ->
                expectationFailure ("JSON parsing failed: " ++ err)

data TestResult
  = ParseFailed String
  | Mismatch ByteString ByteString -- ^ encoding before, encoding after
  | Pass ByteString
  deriving (Show)

roundTrip :: UploadedPackage -> TestResult
roundTrip pkg =
  let before = A.encode pkg
  in case A.eitherDecode before of
       Left err -> ParseFailed err
       Right parsed -> do
         let after' = A.encode (parsed :: UploadedPackage)
         if before == after'
           then Pass before
           else Mismatch before after'

testRunOptions :: PublishOptions
testRunOptions = defaultPublishOptions
  { publishGetVersion = return testVersion
  , publishGetTagTime = const (liftIO getCurrentTime)
  , publishWorkingTreeDirty = return ()
  }
  where testVersion = ("v999.0.0", Version [999,0,0] [])

-- | Given a directory which contains a package, produce JSON from it, and then
-- | attempt to parse it again, and ensure that it doesn't change.
testPackage :: FilePath -> Maybe FilePath -> FilePath -> Expectation
testPackage packageDir dependenciesDir resolutionsFile = do
  res <- pushd packageDir $ do
    compileForPublish dependenciesDir >>= \case
      Left errs ->
        expectationFailure $
          prettyPrintMultipleErrors defaultPPEOptions errs
      Right _ ->
        pure ()
    preparePackage "bower.json" resolutionsFile testRunOptions
  case res of
    Left err ->
      expectationFailure $
        "Failed to produce JSON from " ++ packageDir ++ ":\n" ++
        Boxes.render (Publish.renderError err)
    Right package ->
      case roundTrip package of
        Pass _ ->
          pure ()
        ParseFailed msg ->
          expectationFailure ("Failed to re-parse: " ++ msg)
        Mismatch _ _ ->
          expectationFailure "JSON did not match"

compileForPublish :: Maybe FilePath -> IO (Either MultipleErrors ())
compileForPublish dependenciesDir = do
  inputFiles <-
    fmap concat $ traverse glob $
      [ "src/**/*.purs" ] ++
      case dependenciesDir of
        Just dir ->
          [ dir </> "*/src/**/*.purs" ]
        Nothing ->
          []
  moduleFiles <- readInput inputFiles
  fmap fst $ runMake testOptions $ do
    ms <- CST.parseModulesFromFiles id moduleFiles
    let filePathMap = Map.fromList $ map (\(fp, pm) -> (getModuleName $ CST.resPartial pm, Right fp)) ms
    foreigns <- inferForeignModules filePathMap
    let makeActions = buildMakeActions "output" filePathMap foreigns False
    void (make makeActions (map snd ms))

testOptions :: Options
testOptions = defaultOptions { optionsCodegenTargets = Set.singleton Docs }

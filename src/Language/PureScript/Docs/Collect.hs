
module Language.PureScript.Docs.Collect
  ( collectDocs
  ) where

import Protolude hiding (check)

import Control.Arrow ((&&&))
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Data.Aeson.BetterErrors as ABE
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import System.FilePath ((</>))

import Language.PureScript.Docs.ParseInPackage (parseFilesInPackages)
import Language.PureScript.Docs.Convert.ReExports (updateReExports)
import Language.PureScript.Docs.Prim (primModules)
import Language.PureScript.Docs.Types

import qualified Language.PureScript.AST as P
import qualified Language.PureScript.Crash as P
import qualified Language.PureScript.Errors as P
import qualified Language.PureScript.Externs as P
import qualified Language.PureScript.Make as P
import qualified Language.PureScript.Names as P
import qualified Language.PureScript.Options as P

import Web.Bower.PackageMeta (PackageName)

-- |
-- Given a compiler output directory, a list of input PureScript source files,
-- and a list of dependency PureScript source files, produce documentation for
-- the input files in the intermediate documentation format. Note that
-- dependency files are not included in the result.
--
-- The output directory must be up to date with respect to the provided input
-- and dependency files, and the files must have been built with the docs
-- codegen target, i.e. --codegen docs.
--
collectDocs ::
  forall m.
  (MonadError P.MultipleErrors m, MonadIO m) =>
  FilePath ->
  [FilePath] ->
  [(PackageName, FilePath)] ->
  m ([(FilePath, Module)], Map P.ModuleName PackageName)
collectDocs outputDir inputFiles depsFiles = do
  -- TODO: only need to partially parse here
  (parsedModules', modulesDeps) <- parseFilesInPackages inputFiles depsFiles
  let parsedModules = map (second P.getModuleName) parsedModules'

  externs <- getExterns outputDir parsedModules

  let (withPackage, shouldKeep) =
        packageDiscriminators modulesDeps
  let go =
        operateAndRetag identity modName $ \ms -> do
          docsModules <- traverse (liftIO . parseDocsJsonFile outputDir) ms
          addReExports withPackage docsModules externs

  docsModules <- go parsedModules

  pure ((filter (shouldKeep . modName . snd) docsModules), modulesDeps)

  where

  packageDiscriminators modulesDeps =
    let
      shouldKeep mn = isLocal mn && not (P.isBuiltinModuleName mn)

      withPackage :: P.ModuleName -> InPackage P.ModuleName
      withPackage mn =
        case Map.lookup mn modulesDeps of
          Just pkgName -> FromDep pkgName mn
          Nothing -> Local mn

      isLocal :: P.ModuleName -> Bool
      isLocal = not . flip Map.member modulesDeps
    in
      (withPackage, shouldKeep)

-- |
-- Check that the provided output directory is up-to-date with respect to the
-- provided input files, and if so, load all of their externs files from the
-- disk.
--
-- If the output directory is not up-to-date, throw an error.
--
getExterns ::
  forall m.
  (MonadError P.MultipleErrors m, MonadIO m) =>
  FilePath ->
  [(FilePath, P.ModuleName)] ->
  m [P.ExternsFile]
getExterns outputDir modules = do
  (result, _) <- liftIO $ P.runMake docsOptions $ do
    let filePathMap = Map.fromList $ map (\(fp, mn) -> (mn, Right fp)) modules
    foreigns <- P.inferForeignModules filePathMap
    let actions = P.buildMakeActions outputDir filePathMap foreigns False

    for modules $ \(_, mn) -> do
      runMaybeT (getExternsForModule actions mn) >>= \case
        Just externs -> pure externs
        Nothing -> throwError (P.errorMessage P.NeedToBuildDocs)

  either throwError pure result

  where
  -- We only need to make sure that externs.json and docs.json are up to date;
  -- we don't need to care about other files.
  docsOptions = P.defaultOptions { P.optionsCodegenTargets = Set.singleton P.Docs }

  -- A return value of Nothing indicates that we have out of date or missing
  -- information in the compiler output directory.
  getExternsForModule actions mn = do
    outTime <-
      MaybeT $ P.getOutputTimestamp actions mn

    inTime <-
      MaybeT $ P.getInputTimestamp actions mn >>= \case
        Right t ->
          pure t
        Left _ ->
          P.internalError $
            "Unexpected Left in getInputTimestamp: "
            ++ T.unpack (P.runModuleName mn)

    guard (inTime <= outTime)

    MaybeT $ fmap (P.decodeExterns . snd) (P.readExterns actions mn)

parseDocsJsonFile :: FilePath -> P.ModuleName -> IO Module
parseDocsJsonFile outputDir mn =
  let
    filePath = outputDir </> T.unpack (P.runModuleName mn) </> "docs.json"
  in do
    str <- BS.readFile filePath
    case ABE.parseStrict asModule str of
      Right m -> pure m
      Left err -> P.internalError $
        "Failed to decode: " ++ filePath ++
        intercalate "\n" (map T.unpack (ABE.displayError displayPackageError err))

addReExports ::
  (MonadError P.MultipleErrors m) =>
  (P.ModuleName -> InPackage P.ModuleName) ->
  [Module] ->
  [P.ExternsFile] ->
  m [Module]
addReExports withPackage docsModules externs = do
  -- We add the Prim docs modules here, so that docs generation is still
  -- possible if the modules we are generating docs for re-export things from
  -- Prim submodules. Note that the Prim modules do not exist as
  -- @Language.PureScript.Module@ values because they do not contain anything
  -- that exists at runtime. However, we have pre-constructed
  -- @Language.PureScript.Docs.Types.Module@ values for them, which we use
  -- here.
  let moduleMap =
        Map.fromList
          (map (modName &&& identity)
               (docsModules ++ primModules))

  let withReExports = updateReExports externs withPackage moduleMap
  pure (Map.elems withReExports)

-- |
-- Perform an operation on a list of things which are tagged, and reassociate
-- the things with their tags afterwards.
--
operateAndRetag ::
  forall m a b key tag.
  Monad m =>
  Ord key =>
  Show key =>
  (a -> key) ->
  (b -> key) ->
  ([a] -> m [b]) ->
  [(tag, a)] ->
  m [(tag, b)]
operateAndRetag keyA keyB operation input =
  fmap (map retag) $ operation (map snd input)
  where
  tags :: Map key tag
  tags = Map.fromList $ map (\(tag, a) -> (keyA a, tag)) input

  findTag :: key -> tag
  findTag key =
    case Map.lookup key tags of
      Just tag -> tag
      Nothing -> P.internalError ("Missing tag for: " ++ show key)

  retag :: b -> (tag, b)
  retag b = (findTag (keyB b), b)

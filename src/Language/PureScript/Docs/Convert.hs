-- | Functions for converting PureScript ASTs into values of the data types
-- from Language.PureScript.Docs.

module Language.PureScript.Docs.Convert
  ( collectDocs
  , convertModule
  ) where

import Protolude hiding (check)

import Control.Arrow ((&&&))
import Control.Category ((>>>))
import Control.Monad.Writer.Strict (runWriterT)
import Control.Monad.Supply (evalSupplyT)
import qualified Data.Aeson.BetterErrors as ABE
import qualified Data.ByteString as BS
import Data.Functor (($>))
import qualified Data.Map as Map
import Data.String (String)
import qualified Data.Text as T
import System.FilePath ((</>))

import Language.PureScript.Docs.ParseInPackage (parseFilesInPackages)
import Language.PureScript.Docs.Convert.ReExports (updateReExports)
import Language.PureScript.Docs.Convert.Single (convertSingleModule)
import Language.PureScript.Docs.Prim (primModules)
import Language.PureScript.Docs.Types

import qualified Language.PureScript.AST as P
import qualified Language.PureScript.Crash as P
import qualified Language.PureScript.Errors as P
import qualified Language.PureScript.Externs as P
import qualified Language.PureScript.Environment as P
import qualified Language.PureScript.ModuleDependencies as P
import qualified Language.PureScript.Names as P
import qualified Language.PureScript.Parser as P
import qualified Language.PureScript.Sugar as P
import qualified Language.PureScript.Types as P

import Web.Bower.PackageMeta (PackageName)

import Text.Parsec (eof)

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
  -- TODO Check that the outputDir is up to date relative to the input files

  (parsedModules, modulesDeps) <- parseFilesInPackages inputFiles depsFiles

  -- This is only necessary because we need to get hold of an Env for adding
  -- re-exports. Hopefully we will soon be able to use externs to achieve this
  -- instead.
  env <- getEnvFromModules (map snd parsedModules)

  let (withPackage, shouldKeep) =
        packageDiscriminators modulesDeps
  let go =
        operateAndRetag P.getModuleName modName $ \ms -> do
          docsModules <- traverse (liftIO . parseDocsJsonFile outputDir . P.getModuleName) ms
          addReExports withPackage docsModules env

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

  getEnvFromModules :: [P.Module] -> m P.Env
  getEnvFromModules =
    P.sortModules P.moduleSignature
      >>> fmap (fst >>> map P.importPrim)
      >=> partiallyDesugar []
      >>> fmap fst

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
  P.Env ->
  m [Module]
addReExports withPackage docsModules env = do
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

  -- Set up the traversal order for re-export handling so that Prim modules
  -- come first.
  let primModuleNames = Map.keys P.primEnv
  let traversalOrder = primModuleNames ++ map modName docsModules
  let withReExports = updateReExports env traversalOrder withPackage moduleMap
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

-- |
-- Convert a single module to a Docs.Module, making use of a pre-existing
-- type-checking environment in order to fill in any missing types. Note that
-- re-exports will not be included.
--
convertModule ::
  MonadError P.MultipleErrors m =>
  [P.ExternsFile] ->
  P.Environment ->
  P.Module ->
  m Module
convertModule externs checkEnv m =
  partiallyDesugar externs [m] >>= \case
    (_, [m']) -> pure (insertValueTypes checkEnv (convertSingleModule m'))
    _ -> P.internalError "partiallyDesugar did not return a singleton"

-- |
-- Updates all the types of the ValueDeclarations inside the module based on
-- their types inside the given Environment.
--
insertValueTypes ::
  P.Environment -> Module -> Module
insertValueTypes env m =
  m { modDeclarations = map go (modDeclarations m) }
  where
  go (d@Declaration { declInfo = ValueDeclaration P.TypeWildcard{} }) =
    let
      ident = parseIdent (declTitle d)
      ty = lookupName ident
    in
      d { declInfo = ValueDeclaration (ty $> ()) }
  go other =
    other

  parseIdent =
    either (err . ("failed to parse Ident: " ++)) identity . runParser P.parseIdent

  lookupName name =
    let key = P.Qualified (Just (modName m)) name
    in case Map.lookup key (P.names env) of
      Just (ty, _, _) ->
        ty
      Nothing ->
        err ("name not found: " ++ show key)

  err msg =
    P.internalError ("Docs.Convert.insertValueTypes: " ++ msg)

runParser :: P.TokenParser a -> Text -> Either String a
runParser p s = either (Left . show) Right $ do
  ts <- P.lex "" s
  P.runTokenParser "" (p <* eof) ts

-- |
-- Partially desugar modules so that they are suitable for extracting
-- documentation information from.
--
partiallyDesugar ::
  (MonadError P.MultipleErrors m) =>
  [P.ExternsFile] ->
  [P.Module] ->
  m (P.Env, [P.Module])
partiallyDesugar externs = evalSupplyT 0 . desugar'
  where
  desugar' =
    traverse P.desugarDoModule
      >=> traverse P.desugarAdoModule
      >=> map P.desugarLetPatternModule
      >>> traverse P.desugarCasesModule
      >=> traverse P.desugarTypeDeclarationsModule
      >=> ignoreWarnings . P.desugarImportsWithEnv externs
      >=> traverse (P.rebracketFiltered isInstanceDecl externs)

  ignoreWarnings = fmap fst . runWriterT

  isInstanceDecl (P.TypeInstanceDeclaration {}) = True
  isInstanceDecl _ = False

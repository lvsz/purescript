{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Command.Bundle as Bundle
import qualified Command.Compile as Compile
import qualified Command.Docs as Docs
import qualified Command.Hierarchy as Hierarchy
import qualified Command.Ide as Ide
import qualified Command.Publish as Publish
import qualified Command.REPL as REPL
import           Data.Foldable (fold)
import qualified Options.Applicative as Opts
import           System.Environment (getArgs)
import qualified System.IO as IO
import qualified Text.PrettyPrint.ANSI.Leijen as Doc
import           Version (versionString)

--import qualified Criterion.Main as Crit
import Data.Time
import System.Directory ( removeDirectoryRecursive )

main :: IO ()
main = do
    IO.hPutStrLn IO.stderr "Using modified PureScript"
    IO.hSetEncoding IO.stdout IO.utf8
    IO.hSetEncoding IO.stderr IO.utf8
    -- args <- getArgs
    -- IO.hPutStr IO.stderr "Args: "
    -- IO.hPutStrLn IO.stderr $ show args
    let compArgs = ["compile", "-o", "bench/output", "bench/bower_components/purescript-*/src/**/*.purs", "bench/src/**/*.purs"]
    --Crit.defaultMain [ Crit.bench "Halogen" $ Crit.whnfIO
    cmd <- Opts.handleParseResult . execParserPure opts $ compArgs
    start <- getCurrentTime
    putStrLn $ "Start: " ++ show start
    cmd
    end <- getCurrentTime
    removeDirectoryRecursive "bench/output"
    putStrLn $ "End: " ++ show end
    putStrLn $ "Completed in " ++ show (diffUTCTime end start)
    -- Crit.defaultMain . return $ Crit.bgroup "PureScript" [ Crit.bench "compile" $ Crit.whnfIO cmd ]
  where
    opts        = Opts.info (versionInfo <*> Opts.helper <*> commands) infoModList
    infoModList = Opts.fullDesc <> headerInfo <> footerInfo
    headerInfo  = Opts.progDesc "The PureScript compiler and tools"
    footerInfo  = Opts.footerDoc (Just footer)

    footer =
      mconcat
        [ para $
            "For help using each individual command, run `purs COMMAND --help`. " ++
            "For example, `purs compile --help` displays options specific to the `compile` command."
        , Doc.hardline
        , Doc.hardline
        , Doc.text $ "purs " ++ versionString
        ]

    para :: String -> Doc.Doc
    para = foldr (Doc.</>) Doc.empty . map Doc.text . words

    -- | Displays full command help when invoked with no arguments.
    execParserPure :: Opts.ParserInfo a -> [String] -> Opts.ParserResult a
    execParserPure pinfo [] = Opts.Failure $
      Opts.parserFailure Opts.defaultPrefs pinfo Opts.ShowHelpText mempty
    execParserPure pinfo args = Opts.execParserPure Opts.defaultPrefs pinfo args

    versionInfo :: Opts.Parser (a -> a)
    versionInfo = Opts.abortOption (Opts.InfoMsg versionString) $
      Opts.long "version" <> Opts.help "Show the version number" <> Opts.hidden

    commands :: Opts.Parser (IO ())
    commands =
      (Opts.subparser . fold)
        [ Opts.command "bundle"
            (Opts.info Bundle.command
              (Opts.progDesc "Bundle compiled PureScript modules for the browser"))
        , Opts.command "compile"
            (Opts.info Compile.command
              (Opts.progDesc "Compile PureScript source files"))
        , Opts.command "docs"
            (Opts.info Docs.command
              (Opts.progDesc "Generate documentation from PureScript source files in a variety of formats, including Markdown and HTML" <> Docs.infoModList))
        , Opts.command "hierarchy"
            (Opts.info Hierarchy.command
              (Opts.progDesc "Generate a GraphViz directed graph of PureScript type classes"))
        , Opts.command "ide"
            (Opts.info Ide.command
              (Opts.progDesc "Start or query an IDE server process"))
        , Opts.command "publish"
            (Opts.info Publish.command
              (Opts.progDesc "Generates documentation packages for upload to Pursuit"))
        , Opts.command "repl"
            (Opts.info REPL.command
              (Opts.progDesc "Enter the interactive mode (PSCi)"))
        ]


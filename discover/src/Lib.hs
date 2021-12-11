{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A prepocessor that retrives all modules (non-recursively) from `src/` (that
-- are supopsed to export a 'main' function of type 'IO ()') and  write to the
-- destination files a 'main' function that runs all of the 'main's from the
-- modules.
--
-- Given the following folder structure
--
-- > |-- src
-- > |   |-- Day1.hs
-- > |   |-- Day2.hs
-- > |   `-- Day3.hs
--
-- This will be generated
--
-- 
--
-- Source code is mainly from [sydtest-discover](https://github.com/NorfairKing/sydtest/blob/5b0eee208753e3554d9b158a6e48b1760514aed0/sydtest-discover/src/Test/Syd/Discover.hs).

module Lib (mainDiscover) where

import Control.Monad.IO.Class (MonadIO)
import Data.List (
  intercalate,
  isPrefixOf,
  sort,
 )
import Options.Applicative
import Path
import Path.IO
import System.FilePath qualified as FP
import Prelude

mainDiscover :: IO ()
mainDiscover = do
  args <- getArguments
  specSourceFile <- resolveFile' args.argSource
  let srcDir = parent (parent specSourceFile) </> [reldir|src|]
  let specSourceFileRel = filename specSourceFile
  solutionFiles <-
    fmap parseDayModule . sort
      . filter
        isHaskellFile
      -- All modules files
      <$> sourceFilesInNonHiddenDirs srcDir
  let output = makeDayModule specSourceFileRel solutionFiles
  writeFile args.argDestination output

data Arguments = Arguments
  { argSource :: FilePath
  , argIgnored :: FilePath
  , argDestination :: FilePath
  }
  deriving (Show, Eq)

getArguments :: IO Arguments
getArguments = execParser $ info argumentsParser fullDesc

argumentsParser :: Parser Arguments
argumentsParser =
  Arguments
    <$> strArgument (mconcat [help "Source file path"])
    <*> strArgument (mconcat [help "Ignored argument"])
    <*> strArgument (mconcat [help "Destination file path"])

sourceFilesInNonHiddenDirs ::
  forall m.
  MonadIO m =>
  Path Abs Dir ->
  m [Path Rel File]
sourceFilesInNonHiddenDirs path = do
  (_, files) <- listDirRel path
  pure $ filter (not . hiddenFile) files

hiddenFile :: Path Rel File -> Bool
hiddenFile = goFile
 where
  goFile :: Path Rel File -> Bool
  goFile f = isHiddenIn (parent f) f || goDir (parent f)
  goDir :: Path Rel Dir -> Bool
  goDir f
    | parent f == f = False
    | otherwise = isHiddenIn (parent f) f || goDir (parent f)

isHiddenIn :: Path b Dir -> Path b t -> Bool
isHiddenIn curdir ad =
  case stripProperPrefix curdir ad of
    Nothing -> False
    Just rp -> "." `isPrefixOf` toFilePath rp

isHaskellFile :: Path Rel File -> Bool
isHaskellFile p =
  case fileExtension p of
    Just ".hs" -> True
    Just ".lhs" -> True
    _ -> False

-- Each module represents a solution for a day
data DayModule = DayModule
  { dayModulePath :: Path Rel File
  , dayModuleModuleName :: String
  }

parseDayModule :: Path Rel File -> DayModule
parseDayModule rf =
  DayModule
    { dayModulePath = rf
    , dayModuleModuleName = makeModuleName rf
    }

makeModuleName :: Path Rel File -> String
makeModuleName fp =
  intercalate "." $ FP.splitDirectories $ FP.dropExtensions $ fromRelFile fp

makeDayModule :: Path Rel File -> [DayModule] -> String
makeDayModule destination sources =
  unlines
    [ moduleDeclaration (makeModuleName destination)
    , ""
    , importDeclarations sources
    , "import Prelude"
    , ""
    , mainDeclaration sources
    ]

moduleDeclaration :: String -> String
moduleDeclaration mn = unwords ["module", mn, "(main) where"]

mainDeclaration :: [DayModule] -> String
mainDeclaration fs =
  unlines $
    [ "main :: IO ()"
    , "main = do"
    ]
      <> map moduleMainLine fs

importDeclarations :: [DayModule] -> String
importDeclarations = unlines . map (("import qualified " <>) . dayModuleModuleName)

moduleMainLine :: DayModule -> String
moduleMainLine rf =
  unlines
    [ " putStrLn \"----------------\""
    , " putStrLn " <> "\"" <> dayModuleModuleName rf <> "\""
    , " putStrLn \"----------------\""
    , " " <> dayModuleModuleName rf <> ".main"
    ]

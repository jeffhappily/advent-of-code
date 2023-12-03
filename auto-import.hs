import Control.Monad
import Data.List
import Options.Applicative
import System.Directory
import System.FilePath

data Arguments = Arguments
  { argSource :: FilePath,
    argIgnored :: FilePath,
    argDestination :: FilePath
  }
  deriving (Show, Eq)

getArguments :: IO Arguments
getArguments = execParser $ info argumentsParser fullDesc

argumentsParser :: Parser Arguments
argumentsParser =
  Arguments
    <$> strArgument (mconcat [help "Source directory path"])
    <*> strArgument (mconcat [help "Ignored argument"])
    <*> strArgument (mconcat [help "Destination file path"])

main :: IO ()
main = do
  args <- getArguments
  let sourceDir = "src"
  -- List entries in the source directory
  entries <- listDirectory sourceDir

  -- Filter out non-directory files
  years <- filterM (doesDirectoryExist . (sourceDir </>)) entries
  let yearPaths = map (sourceDir </>) years
  entries <- mapM listYear yearPaths
  let (imports, cases) = unzip $ map (uncurry generateImportsAndCases) (zip years entries)
  let fileContent = generateFileContent (concat imports) (concat cases)
  writeFile args.argDestination fileContent

listYear :: FilePath -> IO [(String, [String])] -- (Day, Parts)
listYear yearPath = do
  days <- listDirectory yearPath
  parts <- mapM (listDirectory . (yearPath </>)) days
  return $ zip days (filter (isSuffixOf ".hs") <$> parts)

generateImportsAndCases :: String -> [(String, [String])] -> (String, String)
generateImportsAndCases year dayParts =
  (concatMap (generateImports year) dayParts, concatMap (generateCases year) dayParts)

generateImports :: String -> (String, [String]) -> String
generateImports year (day, parts) =
  concatMap (\part -> "import qualified " ++ year ++ "." ++ day ++ "." ++ takeBaseName part ++ " as " ++ moduleAlias year day (takeBaseName part) ++ "\n") parts

generateCases :: String -> (String, [String]) -> String
generateCases year (day, parts) =
  concatMap
    ( \part ->
        "    ("
          ++ drop 4 year
          ++ ", "
          ++ drop 3 day
          ++ ", "
          ++ partNumber (takeBaseName part)
          ++ ") -> "
          ++ moduleAlias year day (takeBaseName part)
          ++ ".main input\n"
    )
    parts

moduleAlias :: String -> String -> String -> String
moduleAlias year day part = "Y" ++ drop 4 year ++ "D" ++ drop 3 day ++ part

partNumber :: String -> String
partNumber "Part1" = "1"
partNumber "Part2" = "2"
partNumber _ = "0" -- Default case

generateFileContent :: String -> String -> String
generateFileContent imports cases =
  "module Main where\n\n"
    ++ imports
    ++ "\n"
    ++ "import qualified Data.Text.IO as TIO\n"
    ++ "import Options.Applicative\n\n"
    ++ "data Command = Command\n"
    ++ "  { year :: Int\n"
    ++ "  , day :: Int\n"
    ++ "  , part :: Int\n"
    ++ "  }\n\n"
    ++ "commandParser :: Parser Command\n"
    ++ "commandParser = Command\n"
    ++ "  <$> option auto (long \"year\" <> help \"Year of the event\")\n"
    ++ "  <*> option auto (long \"day\" <> help \"Day of the challenge\")\n"
    ++ "  <*> option auto (long \"part\" <> help \"Part of the challenge\")\n\n"
    ++ "runCommand :: Command -> IO ()\n"
    ++ "runCommand (Command year day part) = do\n"
    ++ "  let inputFilePath = \"input/\" ++ show year ++ \"/d\" ++ show day ++ \".txt\"\n"
    ++ "  input <- TIO.readFile inputFilePath\n"
    ++ "  case (year, day, part) of\n"
    ++ cases
    ++ "    _ -> putStrLn \"Challenge not found\"\n\n"
    ++ "main :: IO ()\n"
    ++ "main = execParser opts >>= runCommand\n"
    ++ "  where\n"
    ++ "    opts = info (commandParser <**> helper)\n"
    ++ "      ( fullDesc\n"
    ++ "     <> progDesc \"Run a solution for Advent of Code\"\n"
    ++ "     <> header \"Advent of Code - Haskell solutions\" )\n"

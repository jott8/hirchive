{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module Main where

import qualified Data.Text as T

import ExifTool            ( ExifTool, get, readMetaEither, withExifTool, Metadata, Tag(Tag) )
import Data.Text           ( Text )
import System.Directory    ( doesDirectoryExist, listDirectory, copyFileWithMetadata, createDirectoryIfMissing, renameFile, copyFile )
import System.FilePath     ( takeExtension, (</>), splitFileName )
import System.Environment  ( getArgs )
import Data.Maybe          ( catMaybes )
import Data.List           ( group, sort )
import Control.Applicative ( (<|>) )
import Control.Monad.Reader
import Control.Monad.Trans.Maybe 

type FmtDate = Text

type ConfigM = ReaderT Config

data Config = Config
    { dirPath :: FilePath
    , targetPath :: FilePath
    , rnFile :: Bool
    , movementSetting :: FilePath -> FilePath -> IO ()
    } 

data PhotoFile = MkPhotoFile
           { photoFileName :: String
           , photoFilePath :: FilePath
           , takenOn :: Text }
    deriving (Show, Eq)

fmtYm :: Text -> Text
fmtYm = T.intercalate "-" . take 2 . T.splitOn ":" . T.takeWhile (/= ' ')

fmtFullDate :: Text -> Text
fmtFullDate = T.replace " " "-" . T.replace ":" "I"

listOfDates :: [PhotoFile] -> [FmtDate]
listOfDates = map head . group . sort . map (fmtYm . takenOn)

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

dateTimeOriginal, creationDate :: Tag
dateTimeOriginal = Tag "DateTimeOriginal"
creationDate = Tag "CreationDate"

createDirs :: [FmtDate] -> ConfigM IO ()
createDirs dates = do
    Config {targetPath} <- ask
    mapM_ (liftIO . createDirectoryIfMissing True . (</>) targetPath . T.unpack) dates

sortPhotos :: [PhotoFile] -> ConfigM IO ()
sortPhotos photos = do 
    Config {targetPath, movementSetting} <- ask
    mapM_ (go targetPath movementSetting) photos
    where
        go tp f (MkPhotoFile pfn pfp to) = liftIO $ f pfp (tp </> T.unpack (fmtYm to) </> T.unpack (fmtFullDate to) ++ takeExtension pfn )

readPhoto :: ExifTool -> FilePath -> IO (Maybe PhotoFile)
readPhoto et fp = photoFromMeta fp <$> readMetaEither et [dateTimeOriginal, creationDate] fp

photoFromMeta :: FilePath -> Either a Metadata -> Maybe PhotoFile
photoFromMeta fp meta = do
    cleanMeta <- hush meta
    date      <- getDate cleanMeta
    return $ MkPhotoFile (snd . splitFileName $ fp) fp date
    where 
        getDate :: Metadata -> Maybe Text
        getDate m = get dateTimeOriginal m <|> get creationDate m

readPhotos :: FilePath -> ConfigM IO [PhotoFile]
readPhotos fp = liftIO $ withExifTool $ \ et -> catMaybes <$> (readDir fp >>= mapM (readPhoto et))

readDir :: FilePath -> IO [FilePath]
readDir topDir = do
  names <- listDirectory topDir
  paths <- mapM go names
  return $ concat paths
  where
    go name = do
      let path = topDir </> name
      isDirectory <- doesDirectoryExist path
      if isDirectory
        then readDir path
        else return [path]

archive :: ConfigM IO ()
archive = do
    Config {dirPath, targetPath} <- ask
    liftIO $ putStrLn $ "Starting hirchive: moving/copying files from '" ++ dirPath ++ "' to '" ++ targetPath ++ "' ...\n"
    photos <- readPhotos dirPath
    let lod = listOfDates photos
    createDirs lod
    sortPhotos photos
    liftIO $ putStrLn "Done"

printUsage :: IO ()
printUsage = putStrLn usage
    where
        usage = "+-----------------+\n" ++
                "| hirchive - Help |\n" ++
                "+-----------------+\n" ++
                "1) Move files (-m), copy files with metadata (-cm), copy without m.d. (-m)\n" ++
                "   - hirchive [-m,-c,-cm] dir targetDir\n\n" ++
                "2) Print this help message\n" ++
                "   - hirchive -h"


loadConfig :: MaybeT IO Config
loadConfig = do
    args <- liftIO getArgs

    case args of
        ["-m", path, tp]  -> return $ Config path tp True renameFile
        ["-c", path, tp]  -> return $ Config path tp True copyFile
        ["-cm", path, tp] -> return $ Config path tp True copyFileWithMetadata
        _                 -> mzero

main :: IO ()
main = runMaybeT loadConfig >>= maybe printUsage start
    where
        start :: Config -> IO ()
        start = runReaderT archive

-- TODO: support Posix & windows? 
-- TODO: add logging/reports (corrupted files, wrong filetype ...)
-- TODO: seperate pure & IO
-- TODO: String/Text efficiency

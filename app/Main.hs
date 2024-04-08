{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T

import ExifTool            ( ExifTool, get, readMetaEither, withExifTool, Metadata, Tag(Tag) )
import Data.Text           ( Text )
import System.Directory    ( doesDirectoryExist, listDirectory, copyFileWithMetadata, createDirectoryIfMissing, renameFile, copyFile )
import System.FilePath     ( (</>), splitFileName )
import Data.Maybe          ( catMaybes )
import Data.List           ( group, sort )
import Control.Applicative ( (<|>) )

type FmtDate = Text

data PhotoFile = MkPhotoFile
           { photoFileName :: String
           , photoFilePath :: FilePath
           , takenOn :: Text }
    deriving (Show, Eq)

fmtYm :: Text -> Text
fmtYm = T.intercalate "-" . take 2 . T.splitOn ":" . T.takeWhile (/= ' ')

listOfDates :: [PhotoFile] -> [FmtDate]
listOfDates = map head . group . sort . map (fmtYm . takenOn)

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

dateTimeOriginal, creationDate :: Tag
dateTimeOriginal = Tag "DateTimeOriginal"
creationDate = Tag "CreationDate"

createDirs :: FilePath -> [FmtDate] -> IO ()
createDirs t = mapM_ (createDirectoryIfMissing True . (++) t . T.unpack)

sortPhotos :: (FilePath -> FilePath -> IO ()) -> FilePath -> [PhotoFile] -> IO ()
sortPhotos f targetDir = mapM_ go
    where
        go (MkPhotoFile pfn pfp to) = f pfp (targetDir </> T.unpack (fmtYm to) </> pfn)

movePhotos, copyPhotos, copyPhotosM :: FilePath -> [PhotoFile] -> IO ()
movePhotos = sortPhotos renameFile
copyPhotos = sortPhotos copyFile
copyPhotosM = sortPhotos copyFileWithMetadata

readPhoto :: ExifTool -> FilePath -> IO (Maybe PhotoFile)
readPhoto et fp = do
    meta <- readMetaEither et [dateTimeOriginal, creationDate] fp

    return $ do { cleanMeta <- hush meta; date <- getDate cleanMeta; return $ MkPhotoFile (snd . splitFileName $ fp) fp date}

    where 
        getDate :: Metadata -> Maybe Text
        getDate meta = get dateTimeOriginal meta <|> get creationDate meta

readPhotos :: FilePath -> IO [PhotoFile]
readPhotos fp = withExifTool $ \ et -> catMaybes <$> (readDir fp >>= mapM (readPhoto et))

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

archive :: FilePath -> FilePath -> IO ()
archive from to = do
    putStrLn $ "Starting hirchive: moving/copying files from '" ++ from ++ "' to '" ++ to ++ "' ...\n"
    photos <- readPhotos from
    let lof = listOfDates photos
    createDirs to lof
    copyPhotosM to photos
    putStrLn "Done"

main :: IO ()
main = undefined
    
-- TODO: support Posix & windows? 
-- TODO: add logging/reports (corrputed files, wrong filetype ...)
-- TODO: String/Text efficiency

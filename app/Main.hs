module Main where

import Graphics.HsExif     ( getDateTimeOriginal, parseFileExif )
import System.Directory    
import System.FilePath     ( (</>), splitFileName )
import Data.Time.Format    ( defaultTimeLocale, formatTime )
import Data.Time.LocalTime ( LocalTime )
import Data.Maybe          ( catMaybes )
import Data.List           ( group, sort )

type FmtDate = String

data PhotoFile = MkPhotoFile
           { photoFileName :: String
           , photoFilePath :: FilePath
           , takenOn :: LocalTime }
    deriving (Show, Eq)

fmtYm :: LocalTime -> String
fmtYm = formatTime defaultTimeLocale "%Y-%m"

listOfDates :: [PhotoFile] -> [FmtDate]
listOfDates = map head . group . sort . map (fmtYm . takenOn)

createDirs :: FilePath -> [FmtDate] -> IO ()
createDirs t = mapM_ (createDirectoryIfMissing True . (++) t)

movePhotos :: FilePath -> [PhotoFile] -> IO ()
movePhotos t = mapM_ go
    where
        go (MkPhotoFile pfn pfp to) = renameFile pfp (t ++ "/" ++ fmtYm to ++ "/" ++ pfn)

readPhoto :: FilePath -> IO (Maybe PhotoFile)
readPhoto fp = do
    exif <- parseFileExif fp
    let dateTaken = either (const Nothing) Just exif >>= getDateTimeOriginal
    return $ MkPhotoFile (snd . splitFileName $ fp) fp <$> dateTaken

readPhotos :: FilePath -> IO [PhotoFile]
readPhotos fp = catMaybes <$> (readDir fp >>= mapM readPhoto)

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
    photos <- readPhotos from
    let lof = listOfDates photos
    createDirs to lof
    movePhotos to photos

main :: IO ()
main = undefined
    
-- TODO: support Posix & windows? 

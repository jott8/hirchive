module Main where

import Graphics.HsExif     ( getDateTimeOriginal, parseFileExif )
import System.Directory    
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

readPhoto :: FilePath -> String -> IO (Maybe PhotoFile)
readPhoto pathPrefix fn = do
    let fp = pathPrefix ++ fn
    exif <- parseFileExif fp
    let dateTaken = either (const Nothing) Just exif >>= getDateTimeOriginal
    return $ MkPhotoFile fn fp <$> dateTaken

readPhotos :: FilePath -> IO [PhotoFile]
readPhotos fp = catMaybes <$> (listDirectory fp >>= mapM (readPhoto fp))

readPhotoDir :: FilePath -> IO [Bool]
readPhotoDir dirPath = listDirectory dirPath >>= mapM go
    where go x = doesDirectoryExist $ dirPath ++ x

filePath, target :: FilePath
filePath = "data/sampleDir/"
target   = "data/targetDir/"

main :: IO ()
main = undefined
    
-- TODO: recursively walk sub-directories and construct flattened list of images
-- TODO: support Posix & windows? 

{-# LANGUAGE ScopedTypeVariables #-}
module Lib (someFunc) where

import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Csv (HasHeader (NoHeader), decode, encode)
import Data.Foldable (Foldable)
import Data.Monoid (Monoid, mappend, mempty)
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import System.Environment (getArgs)

someFunc :: IO ()
someFunc =  do
    startTime <- getCurrentTime
    args <- getArgs
    putStrLn "Loading file..."

    if length args < 3 then
        putStrLn "You must provide three arguments.  resourceName fileSource and fileTarget. In this order"
    else
        let
            resourceName = head args
            fileSource = args !! 1
            fileTarget = args !! 2
        in
            do
                csvData <- BL.readFile fileSource
                putStrLn "Parsing data..."
                let
                    eData = decode NoHeader csvData :: Either String (Vector (Vector BL.ByteString))
                case eData of
                    Left err ->
                        putStrLn $ "Could not parse csv data from " ++ fileSource ++ ". " ++ err

                    Right value ->
                        do
                            putStrLn "Converting data..."
                            writeFile fileTarget . unpack . encode . processData resourceName $ value
                            completionTime <- getCurrentTime

                            let timeDiff = show . floor . toRational . diffUTCTime completionTime  $ startTime

                            putStrLn $ "Time elapsed in seconds: " ++ timeDiff ++ "s"
                            putStrLn "Success"


processData :: String -> Vector (Vector BL.ByteString) -> [(BL.ByteString, String, BL.ByteString, BL.ByteString)]
processData strResourceName raw =
    Vector.toList $ indexedFoldMap processRow raw
    where
        resourceName = pack strResourceName

        processRow rowIndex row =
            Vector.imap createCell $ Vector.tail row
            where
                date =
                    Vector.head row

                createCell colIndex cellContent =
                    (date, "PATH_" ++ show rowIndex , cellContent, resourceName)


indexedFoldMap :: Monoid m => (Int -> a -> m) -> Vector a -> m
indexedFoldMap f v =
    foldl mappend mempty $ Vector.imap f v

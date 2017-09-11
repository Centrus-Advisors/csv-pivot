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
import Text.Regex.Posix ((=~))

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
                            BL.writeFile fileTarget . encode . processData resourceName $ value
                            completionTime <- getCurrentTime

                            let timeDiff = show . floor . toRational . diffUTCTime completionTime  $ startTime

                            putStrLn $ "Time elapsed in seconds: " ++ timeDiff ++ "s"
                            putStrLn "Success"


processData :: String -> Vector (Vector BL.ByteString) -> [ (BL.ByteString, BL.ByteString, BL.ByteString, BL.ByteString) ]
processData strResourceName raw =
    Vector.toList $ Vector.concatMap processRow raw
    where
        resourceName = pack strResourceName

        processRow :: Vector BL.ByteString -> Vector (BL.ByteString, BL.ByteString, BL.ByteString, BL.ByteString)
        processRow row =
            mappend (Vector.singleton firstContent)
                $ Vector.imap (createCell . (1+))
                $ Vector.tail row

            where
                date =
                    pack $ flip (=~) "(\\w|-)+" $ show $ Vector.head row

                firstValue =
                    pack $ (++) " " $ flip (=~) "\\w+\\.\\w+" $ show $ Vector.head row

                firstContent =
                    createCell 0 firstValue

                createCell colIndex cellContent =
                    (date
                    , pack $ "PATH_" ++ show colIndex
                    , cellContent
                    , resourceName
                    )

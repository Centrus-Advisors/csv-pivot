{-# LANGUAGE ScopedTypeVariables #-}
module Lib (someFunc) where

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Csv (HasHeader (NoHeader), decode, encode)
import Data.Foldable (Foldable)
import Data.Monoid (Monoid, mappend, mempty)
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import System.Environment (getArgs)

someFunc :: IO ()
someFunc =  do
    args <- getArgs
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
                let
                    eData = decode NoHeader csvData :: Either String (Vector (Vector BL.ByteString))
                case eData of
                    Left err ->
                        putStrLn $ "Could not parse csv data from " ++ fileSource ++ ". " ++ err

                    Right value ->
                        do
                            writeFile fileTarget . unpack . encode . processData resourceName $ value
                            putStrLn "Success"


processData :: String -> Vector (Vector BL.ByteString) -> [(BL.ByteString, BL.ByteString, BL.ByteString, BL.ByteString)]
processData strResourceName raw =
    let
        headers = Vector.head raw
        resourceName = pack strResourceName
        contentRows = Vector.tail raw
    in
        Vector.toList $
        indexedFoldMap
            (\rowIndex row ->
                let
                    date = Vector.head row
                in
                    Vector.imap
                        (\colIndex cellContent ->
                            let
                                cellHeader = headers ! (colIndex + 1)
                            in
                                (date, cellHeader, cellContent, resourceName)
                        )
                        (Vector.tail row)
            )
            contentRows

indexedFoldMap :: Monoid m => (Int -> a -> m) -> Vector a -> m
indexedFoldMap f v =
    foldl mappend mempty $ Vector.imap f v

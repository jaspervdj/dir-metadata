module DirMetadata.Persist
    ( persist
    , unpersist
    ) where

import System.FilePath ((</>))
import System.Directory (doesFileExist, getHomeDirectory)

import Data.Aeson (Result (..), encode, fromJSON, json)
import Data.Attoparsec (parseOnly)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import DirMetadata (DirMetadata, empty, clean)

persistFile :: IO FilePath
persistFile = do
    homeDir <- getHomeDirectory
    return $ homeDir </> ".dir-metadata.json"

persist :: DirMetadata -> IO ()
persist dm = do
    file <- persistFile
    BL.writeFile file (encode $ clean dm)

unpersist :: IO DirMetadata
unpersist = do
    file <- persistFile
    exists <- doesFileExist file
    if not exists
        then return empty
        else do
            contents <- B.readFile file
            case parseOnly json contents of
                Left  e -> error $ "Could not parse: " ++ file ++ ": " ++ e
                Right v -> case fromJSON v of
                    Error e    -> error $ "Invalid JSON: " ++ file ++ ": " ++ e
                    Success dm -> return dm

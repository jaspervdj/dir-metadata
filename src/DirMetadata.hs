{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DirMetadata
    ( DirMetadata
    , empty
    , list
    , add
    , addAll
    , remove
    , removeAll
    , listDirs
    , clean
    ) where

import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as M

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)

newtype DirMetadata = DirMetadata {unDirMetadata :: Map FilePath [Text]}
                    deriving (FromJSON, ToJSON)

empty :: DirMetadata
empty = DirMetadata M.empty

list :: FilePath -> DirMetadata -> [Text]
list fp = fromMaybe [] . M.lookup fp . unDirMetadata

add :: FilePath -> Text -> DirMetadata -> DirMetadata
add fp t = addAll fp [t]

addAll :: FilePath -> [Text] -> DirMetadata -> DirMetadata
addAll fp ts (DirMetadata dm) = DirMetadata $ M.insertWith (flip (++)) fp ts dm

remove :: FilePath -> [Int] -> DirMetadata -> DirMetadata
remove fp is (DirMetadata dm) = DirMetadata $ M.update remove' fp dm
  where
    remove' = Just . map snd . filter ((`notElem` is) . fst) . zip [1 ..]

removeAll :: FilePath -> DirMetadata -> DirMetadata
removeAll fp = DirMetadata . M.delete fp . unDirMetadata

listDirs :: DirMetadata -> [FilePath]
listDirs = M.keys . unDirMetadata

clean :: DirMetadata -> DirMetadata
clean = DirMetadata . M.filter (not . null) . unDirMetadata

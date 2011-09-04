{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DirMetadata
    ( DirMetadata
    , empty
    , list
    , add
    , addAll
    , remove
    , removeAll
    , move
    , moveAll
    , listDirs
    , clean
    ) where

import Data.List (partition)
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
remove fp is = fst . remove' fp is

remove' :: FilePath -> [Int] -> DirMetadata -> (DirMetadata, [Text])
remove' fp is (DirMetadata dm) = (DirMetadata (M.insert fp keep dm), delete)
  where
    mt f (x, y) = (f x, f y)
    (delete, keep) = mt (map snd) $ partition ((`elem` is) . fst) $
        zip [1 ..] $ fromMaybe [] $ M.lookup fp dm

removeAll :: FilePath -> DirMetadata -> DirMetadata
removeAll fp = DirMetadata . M.delete fp . unDirMetadata

move :: FilePath -> [Int] -> FilePath -> DirMetadata -> DirMetadata
move src is dst dm = let (dm', ts) = remove' src is dm in addAll dst ts dm'

moveAll :: FilePath -> FilePath -> DirMetadata -> DirMetadata
moveAll src dst dm = let ts = list src dm
                         dm' = removeAll src dm
                     in addAll dst ts dm'

listDirs :: DirMetadata -> [FilePath]
listDirs = M.keys . unDirMetadata

clean :: DirMetadata -> DirMetadata
clean = DirMetadata . M.filter (not . null) . unDirMetadata

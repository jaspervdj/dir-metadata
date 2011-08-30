module Main
    ( main
    ) where

import Control.Monad (forM_)
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory, makeRelativeToCurrentDirectory)
import System.FilePath ((</>))
import Data.Maybe (listToMaybe)

import DirMetadata.Persist (persist, unpersist)
import qualified DirMetadata as DM
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
    args <- getArgs
    case args of
        []            -> list []
        ("ls"    : a) -> list a
        ("add"   : a) -> add a
        ("help"  : _) -> usage
        ("rm"    : a) -> remove a
        ("usage" : _) -> usage
        _             -> usage

getDir :: Maybe FilePath -> IO FilePath
getDir (Just fp) = do
    dir <- makeRelativeToCurrentDirectory fp
    cd <- getCurrentDirectory
    return $ cd </> dir
getDir Nothing = getCurrentDirectory

list :: [String] -> IO ()
list args = do
    dir <- getDir $ listToMaybe args
    dm <- unpersist
    forM_ (zip [1 ..] $ DM.list dir dm) $ \(i, t) -> do
        putStr (show (i :: Int))
        putStr ". "
        T.putStrLn t

add :: [String] -> IO ()
add args = do
    t <- case args of [] -> fmap T.stripEnd T.getContents
                      _  -> return $ T.unwords $ map T.pack args
    dir <- getDir Nothing
    persist . DM.add dir t =<< unpersist

remove :: [String] -> IO ()
remove args = do
    let (d, is) = case args of
            []      -> (Nothing, [])
            (h : a) -> if isInt h
                then (Nothing, map read args)
                else (Just h, map read a)
    dir <- getDir d
    persist . DM.remove dir is =<< unpersist
  where
    isInt = not . null . (reads :: ReadS Int)

usage :: IO ()
usage = undefined

{-# LANGUAGE OverloadedStrings #-}

module IndexCode (getFiles) where

import Control.Monad
import Control.Monad.Extra
import Filesystem.Path
import Data.Sequence ((><))
import Data.Foldable (toList)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Sequence as S
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FP
import qualified Control.Concurrent.Async as CA
import qualified Data.Text as T

decodeFile :: FP.FilePath -> T.Text
decodeFile fname =
    case FP.toText fname of
        (Right fname') -> fname'
        (Left _) -> ""

recurse :: S.Seq FP.FilePath -> S.Seq FP.FilePath -> IO (S.Seq FP.FilePath)
recurse files subdirs
    | S.null subdirs = return files
    | otherwise = do
            subs <- CA.mapConcurrently lsRecursive subdirs
            return $ files >< (join subs)

notHidden :: FP.FilePath -> Bool
notHidden fname = (T.head $ decodeFile $ FP.filename fname) /= '.'

getUnhidden :: S.Seq FP.FilePath -> S.Seq FP.FilePath
getUnhidden = S.filter notHidden

isDir :: FP.FilePath -> Bool
isDir fname = unsafePerformIO $ FS.isDirectory fname

lsRecursive :: FP.FilePath -> IO (S.Seq FP.FilePath)
lsRecursive dir = do
    fs <- S.fromList <$> FS.listDirectory dir
    let (dirs, files) = S.partition isDir fs
    recurse files (getUnhidden dirs)

getFiles :: String -> IO (S.Seq FP.FilePath)
getFiles = lsRecursive . FP.decodeString

{-# LANGUAGE NoMonomorphismRestriction #-}
module System.Find (find
                   ,nonRecursiveFind
                   ,grep
                   ,anything
                   ,hasExt
                   ,withFind
                   ,fOr,fAnd) where 

import System.Directory
import System.Posix.Directory
import System.Posix.Files
import System.FilePath
import System.Posix.Files
import System.IO.Unsafe
import Control.Monad
import qualified Data.ByteString.Char8 as BS

f' f x y z = do
  a <- x z
  b <- y z
  return (a `f` b)

fAnd,fOr :: (a -> IO Bool) -> (a -> IO Bool) -> a -> IO Bool
fOr  = f' (||)
fAnd = f' (&&)

gluePaths a b = a </> b
repath path = map (gluePaths path) 

isNotSymbolicLink path = getFileStatus path >>= return . not . isSymbolicLink

anything :: (FilePath -> IO Bool)
anything _ = return True

hasExt e fp = return (ext == e)
    where 
      ext = reverse . takeWhile (/= '.') . reverse $ fp

grep test path = let test' = BS.pack test in do 
  doesFileExist path >>= \x->
    if x 
      then BS.readFile path >>= return . BS.isInfixOf test'
      else return False

noDots = filter (\x->x /= "." && x /= "..") 

------------------------------------------------------------------------

expandDir p = getDirectoryContents p >>= return . repath p . noDots 

find' :: FilePath -> (FilePath -> IO Bool) -> (FilePath -> IO a) -> IO [a]
find' path ff iodo = do
  cur   <- catch (expandDir path) (\_->return [])
  files <- filterM doesFileExist cur >>= filterM ff >>= mapM iodo
  dirs  <- filterM doesDirectoryExist cur
  dirs' <- filterM ff dirs >>= mapM iodo
  expanded <- fmap concat . mapM (\x->find' x ff iodo) $ dirs
  return (dirs' ++ files ++ expanded)


find'' :: [FilePath] -> [a] -> (FilePath -> IO Bool) -> (FilePath -> IO a) -> IO [a]
find'' [] out _ _ = return out
find'' (x:xs) out ff iodo = do
  nout  <- ff x >>= \ch -> 
           if ch 
            then iodo x >>= return . (flip (:) out) 
            else return out
  isDir <- doesDirectoryExist x
  if isDir 
     then (catch (expandDir x) (\_->return [])) >>= \y -> find'' (y ++ xs) nout ff iodo
     else find'' (xs) nout ff iodo


withFind path ff iodo = find'' [path] [] ff iodo

find path ff = find'' [path] [] ff return
nonRecursiveFind path ff = expandDir path >>= filterM ff 


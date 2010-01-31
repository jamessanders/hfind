{-# LANGUAGE NoMonomorphismRestriction #-}
module System.Find (find
                   ,nonRecursiveFind
                   ,grep
                   ,anything
                   ,hasExt
                   ,isNotSymbolicLink
                   ,fOr,fAnd) where 

import System.Directory
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

gluePaths a b = if last a == '/' then a++b else a++"/"++b
repath path list = map (gluePaths path) list 

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

find path ff = do
    a    <- ff path
    rest <- unsafeInterleaveIO getRest
    if a 
      then return (path : rest)
      else return rest
    where 
      getRest = (fmap and $ sequence [catch (doesDirectoryExist path) (\_->return False),
                                      catch (isNotSymbolicLink  path) (\_->return False)]) >>= \e -> 
               if e == True 
                 then do
                   list <- (catch (getDirectoryContents path) (\_->return [])) >>= return . repath path . noDots
                   mapM (flip find ff) list >>= return . concat 
                 else return []

nonRecursiveFind path ff = do
  getDirectoryContents path >>= filterM ff . repath path . noDots



{-# LANGUAGE NoMonomorphismRestriction #-}
module System.Find (find,nonRecursiveFind,contains,noFilter,anything,hasExt,isNotSymbolicLink) where 
import Data.List (isInfixOf)
import System.Directory
import System.FilePath
import System.Posix.Files
import System.IO.Unsafe
import Control.Monad

andM (x:xs)    = x >>= \y-> if y then andM xs else return False
andM []        = return True

filterFile filters file = (andM $ map (\f->f file) filters) 

gluePaths a b = if last a == '/' then a++b else a++"/"++b
repath path list = map (gluePaths path) list 

isNotSymbolicLink path = getFileStatus path >>= return . not . isSymbolicLink

find path filters = do
    rest <- unsafeInterleaveIO $ getRest
    a <- filterFile filters path
    case a of
          True -> return $ path : rest
          False-> return rest
    where noDots = filter (\x->x /= "." && x /= "..") 
          getRest = andM [doesDirectoryExist path,isNotSymbolicLink path] >>= \e -> 
           if e == True 
            then do
              list <- getDirectoryContents path >>= return . repath path . noDots
              mapM (\x->find x filters) list >>= return . concat 
            else
              return []


nonRecursiveFind path filters = do
  getDirectoryContents path >>= filterM dofilter . repath path . noDots
  where noDots = filter (\x->x /= "." && x /= "..") 
        dofilter b = mapM (\f -> f b) filters >>= return . and

contains test path = do 
  doesFileExist path >>= \x->
   case x of
     True -> readFile path >>= return . isInfixOf test
     False -> return False

withFind path filters action = do
    rest <- getRest
    filterFile filters path >>= \y ->
        case y of
          True -> action path
          False-> return ()
    where noDots = filter (\x->x /= "." && x /= "..") 
          getRest = doesDirectoryExist path >>= \e -> 
                    if e == True 
                     then do
                       list <- getDirectoryContents path >>= return . repath path . noDots
                       mapM (\x->withFind x filters action) list
                     else
                       return [()]

noFilter = True

anything :: (FilePath -> IO Bool)
anything = \x->return True

hasExt e fp = return (ext == e)
    where 
      ext = reverse . takeWhile (/= '.') . reverse $ fp
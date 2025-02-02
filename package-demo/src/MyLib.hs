
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}

module MyLib  where


import System.Directory 

import System.Random 

import Control.Monad

import System.IO.Error 

import Control.Exception 


randomNumber :: IO Int
randomNumber = randomRIO (1, 10)

-- ghci> randomNumber
-- 7


-- someFunc :: IO ()
-- someFunc = putStrLn "someFunc"


-- Ok, one module loaded.
-- ghci> :t doesPathExist
-- doesPathExist :: FilePath -> IO Bool

-- ghci> doesPathExist "package-demo.cabal"
-- True

-- ghci> doesDirectoryExist "src"
-- True

-- ghci> :t randomRIO (1,10)
-- randomRIO (1,10)
--   :: (Random a, Control.Monad.IO.Class.MonadIO m, Num a) => m a

-- ghci> :t randomRIO (1,10)
-- randomRIO (1,10)
--   :: (Random a, Control.Monad.IO.Class.MonadIO m, Num a) => m a

-- ghci> :t randomRIO (1,10) :: IO Int
-- randomRIO (1,10) :: IO Int :: IO Int

-- ghci> randomRIO (1,10) :: IO Int
-- 5


-- ghci> :t getDirectoryContents
-- getDirectoryContents :: FilePath -> IO [FilePath]


-- ghci> getDirectoryContents "."
-- [".","..","package-demo.cabal","app","LICENSE","test","CHANGELOG.md","dist-newstyle","hie.yaml","src"]

-- ghci> getDirectoryContents "dist-newstyle"
-- [".","..","packagedb","cache","build","tmp"]



filesInCurrentDirectory :: IO [FilePath]
filesInCurrentDirectory = do  
    contents <- getDirectoryContents "."
    filterM doesFileExist contents


-- ghci> getDirectoryContents "."
-- [".","..","package-demo.cabal","app","LICENSE","test","CHANGELOG.md","dist-newstyle","hie.yaml","src"]

-- ghci> :t getFileSize
-- getFileSize :: FilePath -> IO Integer

-- ghci> getFileSize "hie.yaml"
-- 52


fileSizesInCurrentDirectory :: IO [(FilePath , Integer)]
fileSizesInCurrentDirectory = do 
    files <- filesInCurrentDirectory
    mapM (\ fp -> (fp , ) <$> getFileSize fp) files 


    -- mapM (\ fp -> fmap (fp, ) getFileSize fp) files 
 
-- ghci> fileSizesInCurrentDirectory
-- [("package-demo.cabal",715),("LICENSE",1496),("CHANGELOG.md",115),("hie.yaml",52)]



-- ghci> setCurrentDirectory "src"
-- ghci> fileSizesInCurrentDirectory 
-- [("MyLib.hs",1845)]




-------- ReadFile ------------------


-- ghci> readFile "test.txt"
-- *** Exception: test.txt: openFile: does not exist (No such file or directory)
-- ghci> readFile "hie.yaml"
-- "cradle:\n  cabal:\n    component : \"lib:package-demo\"\n"


-- ghci> :t readFile 
-- readFile :: FilePath -> IO String


------------- Catching error -----------

-- ghci> :t catchIOError
-- catchIOError :: IO a -> (IOError -> IO a) -> IO a

-- ghci> catchIOError (readFile "test.txt") (\ _ -> return "")
-- ""

-- ghci> catchIOError (readFile "hie.yaml") (\ _ 
-- -> return "")
-- "cradle:\n  cabal:\n    component : \"lib:package-demo\"\n"


-- ghci> catchIOError (Just <$> readFile "hie.yam
-- l") (\ _ -> return Nothing)
-- Just "cradle:\n  cabal:\n    component : \"lib:package-demo\"\n"


-- ghci> catchIOError (Just <$> readFile "text.txt") (\ _ -> return Nothing)
-- Nothing



-- ghci> catchIOError (Just <$> readFile "text.txt") (\ err -> if isDoesNotExistError err then return Nothing else throwIO err)
-- Nothing


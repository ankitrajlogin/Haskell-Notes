{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_Q (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/ankit.raj/.cabal/bin"
libdir     = "/Users/ankit.raj/.cabal/lib/aarch64-osx-ghc-9.4.8/Q-0.1-inplace"
dynlibdir  = "/Users/ankit.raj/.cabal/lib/aarch64-osx-ghc-9.4.8"
datadir    = "/Users/ankit.raj/.cabal/share/aarch64-osx-ghc-9.4.8/Q-0.1"
libexecdir = "/Users/ankit.raj/.cabal/libexec/aarch64-osx-ghc-9.4.8/Q-0.1"
sysconfdir = "/Users/ankit.raj/.cabal/etc"

getBinDir     = catchIO (getEnv "Q_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "Q_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "Q_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "Q_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Q_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Q_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'

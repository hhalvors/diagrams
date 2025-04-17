{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_diagrams (
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
version = Version [0,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/hhalvors/diagrams/.stack-work/install/aarch64-osx/c1409fcc6d22d61b06e79374c3ff24f42003c9379ec03d7fa6fbba03dedfe0be/9.8.4/bin"
libdir     = "/Users/hhalvors/diagrams/.stack-work/install/aarch64-osx/c1409fcc6d22d61b06e79374c3ff24f42003c9379ec03d7fa6fbba03dedfe0be/9.8.4/lib/aarch64-osx-ghc-9.8.4/diagrams-0.0.0-K8ileCWufRH4490n3REtN-diagrams-exe"
dynlibdir  = "/Users/hhalvors/diagrams/.stack-work/install/aarch64-osx/c1409fcc6d22d61b06e79374c3ff24f42003c9379ec03d7fa6fbba03dedfe0be/9.8.4/lib/aarch64-osx-ghc-9.8.4"
datadir    = "/Users/hhalvors/diagrams/.stack-work/install/aarch64-osx/c1409fcc6d22d61b06e79374c3ff24f42003c9379ec03d7fa6fbba03dedfe0be/9.8.4/share/aarch64-osx-ghc-9.8.4/diagrams-0.0.0"
libexecdir = "/Users/hhalvors/diagrams/.stack-work/install/aarch64-osx/c1409fcc6d22d61b06e79374c3ff24f42003c9379ec03d7fa6fbba03dedfe0be/9.8.4/libexec/aarch64-osx-ghc-9.8.4/diagrams-0.0.0"
sysconfdir = "/Users/hhalvors/diagrams/.stack-work/install/aarch64-osx/c1409fcc6d22d61b06e79374c3ff24f42003c9379ec03d7fa6fbba03dedfe0be/9.8.4/etc"

getBinDir     = catchIO (getEnv "diagrams_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "diagrams_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "diagrams_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "diagrams_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "diagrams_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "diagrams_sysconfdir") (\_ -> return sysconfdir)



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

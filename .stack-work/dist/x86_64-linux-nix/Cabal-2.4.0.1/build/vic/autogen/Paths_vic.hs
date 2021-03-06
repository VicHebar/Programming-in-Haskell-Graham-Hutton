{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_vic (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/vichebar/src/Programming-in-Haskell-Graham-Hutton/.stack-work/install/x86_64-linux-nix/4fe0c6bf90e64f9875c35a9c96451b67c43ee47f6416de083a48661f9970d8f3/8.6.5/bin"
libdir     = "/home/vichebar/src/Programming-in-Haskell-Graham-Hutton/.stack-work/install/x86_64-linux-nix/4fe0c6bf90e64f9875c35a9c96451b67c43ee47f6416de083a48661f9970d8f3/8.6.5/lib/x86_64-linux-ghc-8.6.5/vic-0.1.0.0-8mkQgTWnLme4cQ3bJ8cva0-vic"
dynlibdir  = "/home/vichebar/src/Programming-in-Haskell-Graham-Hutton/.stack-work/install/x86_64-linux-nix/4fe0c6bf90e64f9875c35a9c96451b67c43ee47f6416de083a48661f9970d8f3/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/vichebar/src/Programming-in-Haskell-Graham-Hutton/.stack-work/install/x86_64-linux-nix/4fe0c6bf90e64f9875c35a9c96451b67c43ee47f6416de083a48661f9970d8f3/8.6.5/share/x86_64-linux-ghc-8.6.5/vic-0.1.0.0"
libexecdir = "/home/vichebar/src/Programming-in-Haskell-Graham-Hutton/.stack-work/install/x86_64-linux-nix/4fe0c6bf90e64f9875c35a9c96451b67c43ee47f6416de083a48661f9970d8f3/8.6.5/libexec/x86_64-linux-ghc-8.6.5/vic-0.1.0.0"
sysconfdir = "/home/vichebar/src/Programming-in-Haskell-Graham-Hutton/.stack-work/install/x86_64-linux-nix/4fe0c6bf90e64f9875c35a9c96451b67c43ee47f6416de083a48661f9970d8f3/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "vic_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "vic_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "vic_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "vic_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "vic_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "vic_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

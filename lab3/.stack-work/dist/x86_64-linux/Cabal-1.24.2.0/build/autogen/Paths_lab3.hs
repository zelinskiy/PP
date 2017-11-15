{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_lab3 (
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

bindir     = "/home/nik/projects/PP/lab3/.stack-work/install/x86_64-linux/lts-9.13/8.0.2/bin"
libdir     = "/home/nik/projects/PP/lab3/.stack-work/install/x86_64-linux/lts-9.13/8.0.2/lib/x86_64-linux-ghc-8.0.2/lab3-0.1.0.0"
dynlibdir  = "/home/nik/projects/PP/lab3/.stack-work/install/x86_64-linux/lts-9.13/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/nik/projects/PP/lab3/.stack-work/install/x86_64-linux/lts-9.13/8.0.2/share/x86_64-linux-ghc-8.0.2/lab3-0.1.0.0"
libexecdir = "/home/nik/projects/PP/lab3/.stack-work/install/x86_64-linux/lts-9.13/8.0.2/libexec"
sysconfdir = "/home/nik/projects/PP/lab3/.stack-work/install/x86_64-linux/lts-9.13/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "lab3_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "lab3_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "lab3_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "lab3_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lab3_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lab3_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

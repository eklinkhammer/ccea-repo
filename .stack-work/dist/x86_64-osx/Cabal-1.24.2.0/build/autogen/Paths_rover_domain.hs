{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_rover_domain (
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

bindir     = "/Users/klinkhae/research/haskell_ws/rover-domain/.stack-work/install/x86_64-osx/lts-7.15/8.0.1/bin"
libdir     = "/Users/klinkhae/research/haskell_ws/rover-domain/.stack-work/install/x86_64-osx/lts-7.15/8.0.1/lib/x86_64-osx-ghc-8.0.1/rover-domain-0.1.0.0-61Fsc9uUajFGfxZsLCyosq"
dynlibdir  = "/Users/klinkhae/research/haskell_ws/rover-domain/.stack-work/install/x86_64-osx/lts-7.15/8.0.1/lib/x86_64-osx-ghc-8.0.1"
datadir    = "/Users/klinkhae/research/haskell_ws/rover-domain/.stack-work/install/x86_64-osx/lts-7.15/8.0.1/share/x86_64-osx-ghc-8.0.1/rover-domain-0.1.0.0"
libexecdir = "/Users/klinkhae/research/haskell_ws/rover-domain/.stack-work/install/x86_64-osx/lts-7.15/8.0.1/libexec"
sysconfdir = "/Users/klinkhae/research/haskell_ws/rover-domain/.stack-work/install/x86_64-osx/lts-7.15/8.0.1/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "rover_domain_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "rover_domain_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "rover_domain_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "rover_domain_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "rover_domain_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "rover_domain_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

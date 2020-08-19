{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_landschaften_service (
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

bindir     = "/Users/concerto/cs/landschaften-service/.stack-work/install/x86_64-osx/6c29048965673d0512355e2826e8b6b07ab6999461cad42d385d53bd0ded7923/8.6.5/bin"
libdir     = "/Users/concerto/cs/landschaften-service/.stack-work/install/x86_64-osx/6c29048965673d0512355e2826e8b6b07ab6999461cad42d385d53bd0ded7923/8.6.5/lib/x86_64-osx-ghc-8.6.5/landschaften-service-0.1.0.0-2vKyLupMvjOK8I1vGEdIBF"
dynlibdir  = "/Users/concerto/cs/landschaften-service/.stack-work/install/x86_64-osx/6c29048965673d0512355e2826e8b6b07ab6999461cad42d385d53bd0ded7923/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/concerto/cs/landschaften-service/.stack-work/install/x86_64-osx/6c29048965673d0512355e2826e8b6b07ab6999461cad42d385d53bd0ded7923/8.6.5/share/x86_64-osx-ghc-8.6.5/landschaften-service-0.1.0.0"
libexecdir = "/Users/concerto/cs/landschaften-service/.stack-work/install/x86_64-osx/6c29048965673d0512355e2826e8b6b07ab6999461cad42d385d53bd0ded7923/8.6.5/libexec/x86_64-osx-ghc-8.6.5/landschaften-service-0.1.0.0"
sysconfdir = "/Users/concerto/cs/landschaften-service/.stack-work/install/x86_64-osx/6c29048965673d0512355e2826e8b6b07ab6999461cad42d385d53bd0ded7923/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "landschaften_service_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "landschaften_service_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "landschaften_service_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "landschaften_service_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "landschaften_service_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "landschaften_service_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

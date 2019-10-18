{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_tui (
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
version = Version [0,0,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/mnt/c/Users/natuo/OneDrive/Documents/SHOOL/FALL 2019/CPSC 449/Stack/Checkers/Haskell-Checkers-Frontend/.stack-work/install/x86_64-linux/256e1ae8609e55675f79e9ceba4a4fe28ec9cae86ad705bcb5e169ebcebd38c7/8.4.4/bin"
libdir     = "/mnt/c/Users/natuo/OneDrive/Documents/SHOOL/FALL 2019/CPSC 449/Stack/Checkers/Haskell-Checkers-Frontend/.stack-work/install/x86_64-linux/256e1ae8609e55675f79e9ceba4a4fe28ec9cae86ad705bcb5e169ebcebd38c7/8.4.4/lib/x86_64-linux-ghc-8.4.4/tui-0.0.0.1-9lLxITXitoEJdWL7uGNCp3-tui"
dynlibdir  = "/mnt/c/Users/natuo/OneDrive/Documents/SHOOL/FALL 2019/CPSC 449/Stack/Checkers/Haskell-Checkers-Frontend/.stack-work/install/x86_64-linux/256e1ae8609e55675f79e9ceba4a4fe28ec9cae86ad705bcb5e169ebcebd38c7/8.4.4/lib/x86_64-linux-ghc-8.4.4"
datadir    = "/mnt/c/Users/natuo/OneDrive/Documents/SHOOL/FALL 2019/CPSC 449/Stack/Checkers/Haskell-Checkers-Frontend/.stack-work/install/x86_64-linux/256e1ae8609e55675f79e9ceba4a4fe28ec9cae86ad705bcb5e169ebcebd38c7/8.4.4/share/x86_64-linux-ghc-8.4.4/tui-0.0.0.1"
libexecdir = "/mnt/c/Users/natuo/OneDrive/Documents/SHOOL/FALL 2019/CPSC 449/Stack/Checkers/Haskell-Checkers-Frontend/.stack-work/install/x86_64-linux/256e1ae8609e55675f79e9ceba4a4fe28ec9cae86ad705bcb5e169ebcebd38c7/8.4.4/libexec/x86_64-linux-ghc-8.4.4/tui-0.0.0.1"
sysconfdir = "/mnt/c/Users/natuo/OneDrive/Documents/SHOOL/FALL 2019/CPSC 449/Stack/Checkers/Haskell-Checkers-Frontend/.stack-work/install/x86_64-linux/256e1ae8609e55675f79e9ceba4a4fe28ec9cae86ad705bcb5e169ebcebd38c7/8.4.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tui_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tui_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "tui_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "tui_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tui_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tui_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

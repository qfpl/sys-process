{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Sys.CmdSpec(
  CmdSpec(..)
, AsCmdSpec(..)
, AsExecutableName(..)
, AsExecutableArguments(..)
, AsShellCommand(..)
, AsRawCommand(..)
) where

import Control.Applicative(Applicative)
import Control.Category(Category(id, (.)))
import Control.Lens(Optic', Choice, Profunctor, prism', iso, _1, _2)
import Data.Eq(Eq)
import Data.Functor(Functor)
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ord)
import Data.String(IsString(fromString), String)
import Data.Tuple(uncurry)
import Prelude(Show)
import System.FilePath(FilePath)
import qualified System.Process as Process

data CmdSpec =
  ShellCommand String
      -- ^ A command line to execute using the shell
  | RawCommand FilePath [String]
      -- ^ The name of an executable with a list of arguments
      --
      -- The 'FilePath' argument names the executable, and is interpreted
      -- according to the platform's standard policy for searching for
      -- executables. Specifically:
      --
      -- * on Unix systems the
      --   <http://pubs.opengroup.org/onlinepubs/9699919799/functions/execvp.html execvp(3)>
      --   semantics is used, where if the executable filename does not
      --   contain a slash (@/@) then the @PATH@ environment variable is
      --   searched for the executable.
      --
      -- * on Windows systems the Win32 @CreateProcess@ semantics is used.
      --   Briefly: if the filename does not contain a path, then the
      --   directory containing the parent executable is searched, followed
      --   by the current directory, then some standard locations, and
      --   finally the current @PATH@.  An @.exe@ extension is added if the
      --   filename does not already have an extension.  For full details
      --   see the
      --   <http://msdn.microsoft.com/en-us/library/windows/desktop/aa365527%28v=vs.85%29.aspx documentation>
      --   for the Windows @SearchPath@ API.
  deriving (Eq, Ord, Show)

instance IsString CmdSpec where
  fromString =
    ShellCommand

class AsCmdSpec p f s where
  _CmdSpec ::
    Optic' p f s CmdSpec

instance AsCmdSpec p f CmdSpec where
  _CmdSpec =
    id

instance (Profunctor p, Functor f) => AsCmdSpec p f Process.CmdSpec where
  _CmdSpec =
    iso
      (\c -> case c of
          Process.ShellCommand s -> 
            ShellCommand s
          Process.RawCommand p s ->
            RawCommand p s)
      (\c -> case c of
          ShellCommand s -> 
            Process.ShellCommand s
          RawCommand p s ->
            Process.RawCommand p s)

class AsExecutableName p f s where
  _ExecutableName ::
    Optic' p f s FilePath

instance AsExecutableName p f FilePath where
  _ExecutableName =
    id

instance Applicative f => AsExecutableName (->) f CmdSpec where
  _ExecutableName =
    _RawCommand . _1

class AsExecutableArguments p f s where
  _ExecutableArguments ::
    Optic' p f s [String]

instance AsExecutableArguments p f [String] where
  _ExecutableArguments =
    id

instance Applicative f => AsExecutableArguments (->) f CmdSpec where
  _ExecutableArguments =
    _RawCommand . _2

class AsShellCommand p f s where
  _ShellCommand ::
    Optic' p f s String

instance AsShellCommand p f String where
  _ShellCommand =
    id

instance (Choice p, Applicative f) => AsShellCommand p f CmdSpec where
  _ShellCommand =
    prism'
      ShellCommand
      (\c -> case c of
                ShellCommand s -> 
                  Just s
                RawCommand _ _ ->
                  Nothing)

class AsRawCommand p f s where
  _RawCommand ::
    Optic' p f s (FilePath, [String])

instance AsRawCommand p f (FilePath, [String]) where
  _RawCommand =
    id
    
instance (Choice p, Applicative f) => AsRawCommand p f CmdSpec where
  _RawCommand =
    prism'
      (uncurry RawCommand)
      (\c -> case c of
                ShellCommand _ -> 
                  Nothing
                RawCommand p s ->
                  Just (p, s))

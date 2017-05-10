{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Sys.CreateProcess(
  CreateProcess(..)
, AsCreateProcess(..)
, AsWorkingDirectory(..)
, AsEnvironment(..)
, AsStdin(..)
, AsStdout(..)
, AsStderr(..)
, AsCloseDescriptors(..)
, AsCreateGroup(..)
, AsDelegateCtrlC(..)
) where

import Control.Applicative(Applicative)
import Control.Category(Category(id, (.)))
import Control.Lens(Optic', Profunctor, lens, from, iso, (#))
import Data.Bool(Bool)
import Data.Eq(Eq)
import Data.Functor(Functor)
import Data.Maybe(Maybe)
import Data.String(String)
import Prelude(Show)
import Sys.CmdSpec(CmdSpec, AsCmdSpec(_CmdSpec), AsExecutableName(_ExecutableName), AsExecutableArguments(_ExecutableArguments), AsShellCommand(_ShellCommand), AsRawCommand(_RawCommand))
import Sys.StdStream(StdStream, AsStdStream(_StdStream))
import System.FilePath(FilePath)
import System.Posix.Types(GroupID, UserID)
import qualified System.Process as Process

-- | Data type representing a process.
--
-- /see 'System.Process.CreateProcess'/.
data CreateProcess =
  CreateProcess
    CmdSpec
    (Maybe FilePath)          
    (Maybe [(String,String)]) 
    StdStream
    StdStream
    StdStream
    Bool     
    Bool 
    Bool 
    Bool 
    Bool 
    Bool
    (Maybe GroupID) 
    (Maybe UserID) 
  deriving (Eq, Show)

-- | Types that related to @CreateProcess@.
class AsCreateProcess p f s where
  _CreateProcess ::
    Optic' p f s CreateProcess

instance AsCreateProcess p f CreateProcess where
  _CreateProcess =
    id

instance (Profunctor p, Functor f) => AsCreateProcess p f Process.CreateProcess where
  _CreateProcess =
    iso
      (\(Process.CreateProcess s p e i o r d g c x1 x2 x3 x4 x5) ->
        CreateProcess (from _CmdSpec # s) p e (from _StdStream # i) (from _StdStream # o) (from _StdStream # r) d g c x1 x2 x3 x4 x5)
      (\(CreateProcess s p e i o r d g c x1 x2 x3 x4 x5) ->
        Process.CreateProcess (_CmdSpec # s) p e (_StdStream # i) (_StdStream # o) (_StdStream # r) d g c x1 x2 x3 x4 x5)

instance Functor f => AsCmdSpec (->) f CreateProcess where
  _CmdSpec =
    lens
      (\(CreateProcess s _ _ _ _ _ _ _ _ _ _ _ _ _) -> s)
      (\(CreateProcess _ p e i o r d g c x1 x2 x3 x4 x5) s -> CreateProcess s p e i o r d g c x1 x2 x3 x4 x5)

instance Applicative f => AsExecutableName (->) f CreateProcess where
  _ExecutableName =
    _CmdSpec . _ExecutableName

instance Applicative f => AsExecutableArguments (->) f CreateProcess where
  _ExecutableArguments =
    _CmdSpec . _ExecutableArguments

instance Applicative f => AsShellCommand (->) f CreateProcess where
  _ShellCommand =
    _CmdSpec . _ShellCommand

instance Applicative f => AsRawCommand (->) f CreateProcess where
  _RawCommand =
    _CmdSpec . _RawCommand

-- | Types that relate to a (maybe) working directory.
class AsWorkingDirectory p f s where
  _WorkingDirectory ::
    Optic' p f s (Maybe FilePath)

instance AsWorkingDirectory p f (Maybe FilePath) where
  _WorkingDirectory =
    id

instance Functor f => AsWorkingDirectory (->) f CreateProcess where
  _WorkingDirectory =
    lens
      (\(CreateProcess _ p _ _ _ _ _ _ _ _ _ _ _ _) -> p)
      (\(CreateProcess s _ e i o r d g c x1 x2 x3 x4 x5) p -> CreateProcess s p e i o r d g c x1 x2 x3 x4 x5)

-- | Types that relate to an environment.
class AsEnvironment p f s where
  _Environment ::
    Optic' p f s (Maybe [(String, String)])

instance AsEnvironment p f (Maybe [(String, String)]) where
  _Environment =
    id

instance Functor f => AsEnvironment (->) f CreateProcess where
  _Environment =
    lens
      (\(CreateProcess _ _ e _ _ _ _ _ _ _ _ _ _ _) -> e)
      (\(CreateProcess s p _ i o r d g c x1 x2 x3 x4 x5) e -> CreateProcess s p e i o r d g c x1 x2 x3 x4 x5)

-- | Types that relate to a standard input stream.
class AsStdin p f s where
  _Stdin ::
    Optic' p f s StdStream

instance AsStdin p f StdStream where
  _Stdin =
    id

instance Functor f => AsStdin (->) f CreateProcess where
  _Stdin =
    lens
      (\(CreateProcess _ _ _ i _ _ _ _ _ _ _ _ _ _) -> i)
      (\(CreateProcess s p e _ o r d g c x1 x2 x3 x4 x5) i -> CreateProcess s p e i o r d g c x1 x2 x3 x4 x5)

-- | Types that relate to a standard output stream.
class AsStdout p f s where
  _Stdout ::
    Optic' p f s StdStream

instance AsStdout p f StdStream where
  _Stdout =
    id

instance Functor f => AsStdout (->) f CreateProcess where
  _Stdout =
    lens
      (\(CreateProcess _ _ _ _ o _ _ _ _ _ _ _ _ _) -> o)
      (\(CreateProcess s p e i _ r d g c x1 x2 x3 x4 x5) o -> CreateProcess s p e i o r d g c x1 x2 x3 x4 x5)

-- | Types that relate to a standard error stream.
class AsStderr p f s where
  _Stderr ::
    Optic' p f s StdStream

instance AsStderr p f StdStream where
  _Stderr =
    id

instance Functor f => AsStderr (->) f CreateProcess where
  _Stderr =
    lens
      (\(CreateProcess _ _ _ _ _ r _ _ _ _ _ _ _ _) -> r)
      (\(CreateProcess s p e i o _ d g c x1 x2 x3 x4 x5) r -> CreateProcess s p e i o r d g c x1 x2 x3 x4 x5)

-- | Types that relate to closing descriptors.
class AsCloseDescriptors p f s where
  _CloseDescriptors ::
    Optic' p f s Bool

instance AsCloseDescriptors p f Bool where
  _CloseDescriptors =
    id

instance Functor f => AsCloseDescriptors (->) f CreateProcess where
  _CloseDescriptors =
    lens
      (\(CreateProcess _ _ _ _ _ _ d _ _ _ _ _ _ _) -> d)
      (\(CreateProcess s p e i o r _ g c x1 x2 x3 x4 x5) d -> CreateProcess s p e i o r d g c x1 x2 x3 x4 x5)

-- | Types that relate to creating groups.
class AsCreateGroup p f s where
  _CreateGroup ::
    Optic' p f s Bool

instance AsCreateGroup p f Bool where
  _CreateGroup =
    id

instance Functor f => AsCreateGroup (->) f CreateProcess where
  _CreateGroup =
    lens
      (\(CreateProcess _ _ _ _ _ _ _ g _ _ _ _ _ _) -> g)
      (\(CreateProcess s p e i o r d _ c x1 x2 x3 x4 x5) g-> CreateProcess s p e i o r d g c x1 x2 x3 x4 x5)

-- | Types that relate to delegating CTRL-C.
class AsDelegateCtrlC p f s where
  _DelegateCtrlC ::
    Optic' p f s Bool

instance AsDelegateCtrlC p f Bool where
  _DelegateCtrlC =
    id
    
instance Functor f => AsDelegateCtrlC (->) f CreateProcess where
  _DelegateCtrlC =
    lens
      (\(CreateProcess _ _ _ _ _ _ _ _ c _ _ _ _ _) -> c)
      (\(CreateProcess s p e i o r d g _ x1 x2 x3 x4 x5) c -> CreateProcess s p e i o r d g c x1 x2 x3 x4 x5)

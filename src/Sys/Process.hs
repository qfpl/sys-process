module Sys.Process(
  module CmdSpec
, module CreateProcess
, module StdStream
-- * Running sub-processes
, createProcess
, createProcess_
, shell
, proc
-- ** Simpler functions for common tasks
, Process.callProcess
, Process.callCommand
, Process.spawnProcess
, Process.spawnCommand
, readCreateProcess
, Process.readProcess
, readCreateProcessWithExitCode
, readProcessWithExitCode
-- ** Related utilities
, Process.showCommandForUser
-- ** Control-C handling on Unix
-- $ctlc-handling
-- * Process completion
, waitForProcess
, getProcessExitCode
, Process.terminateProcess
, Process.interruptProcessGroupOf
-- * Interprocess communication
, Process.createPipe
) where

{-
    
    -- ** Related utilities
    showCommandForUser,

    -- ** Control-C handling on Unix
    -- $ctlc-handling

    -- * Process completion
    waitForProcess,
    getProcessExitCode,
    terminateProcess,
    interruptProcessGroupOf,

    -- Interprocess communication
-}

import Control.Category(Category((.)))
import Control.Lens((#), (%~), (^.), _1)
import Data.Functor(Functor(fmap))
import Data.Maybe(Maybe)
import Data.String(String)
import Sys.CmdSpec as CmdSpec
import Sys.CreateProcess as CreateProcess
import Sys.StdStream as StdStream
import Sys.ExitCode(ExitCode, unExitCode)
import System.FilePath(FilePath)
import System.IO(Handle, IO)
import qualified System.Process as Process
import Prelude()

createProcess ::
  CreateProcess
  -> IO (Maybe Handle, Maybe Handle, Maybe Handle, Process.ProcessHandle) 
createProcess =
  Process.createProcess . (_CreateProcess #)

createProcess_ ::
  String
  -> CreateProcess
  -> IO (Maybe Handle, Maybe Handle, Maybe Handle, Process.ProcessHandle) 
createProcess_ s =
  Process.createProcess_ s . (_CreateProcess #)

shell ::
  String
  -> CreateProcess
shell s =
  Process.shell s ^. _CreateProcess

proc ::
  FilePath
  -> [String]
  -> CreateProcess
proc p s =
  Process.proc p s ^. _CreateProcess

readCreateProcess ::
  CreateProcess   
  -> String 
  -> IO String  
readCreateProcess =
  Process.readCreateProcess . (_CreateProcess #)

readCreateProcessWithExitCode ::
  CreateProcess   
  -> String 
  -> IO (ExitCode, String, String)
readCreateProcessWithExitCode p =
  fmap (_1 %~ unExitCode) . Process.readCreateProcessWithExitCode (_CreateProcess # p)

readProcessWithExitCode ::
  FilePath
  -> [String] 
  -> String 
  -> IO (ExitCode, String, String)  
readProcessWithExitCode f a =
  fmap (_1 %~ unExitCode) . Process.readProcessWithExitCode f a

waitForProcess ::
  Process.ProcessHandle
  -> IO ExitCode
waitForProcess =
  fmap unExitCode . Process.waitForProcess

getProcessExitCode ::
  Process.ProcessHandle
  -> IO (Maybe ExitCode) 
getProcessExitCode =
  fmap (fmap unExitCode) . Process.getProcessExitCode

module Sys.Process(
  module CmdSpec
, module CreateProcess
, module StdStream
, Process.ProcessHandle
-- * Running sub-processes
, createProcess
, createProcess_
, shell
, proc
-- ** Simpler functions for common tasks
, callProcess
, callCommand
, spawnProcess
, spawnCommand
, readCreateProcess
, readProcess
, readCreateProcessWithExitCode
, readProcessWithExitCode
-- ** Related utilities
, showCommandForUser
-- ** Control-C handling on Unix
-- $ctlc-handling
-- * Process completion
, waitForProcess
, getProcessExitCode
, terminateProcess
, interruptProcessGroupOf
-- * Interprocess communication
, createPipe
) where

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

{- |
This is the most general way to spawn an external process.  The
process can be a command line to be executed by a shell or a raw command
with a list of arguments.  The stdin, stdout, and stderr streams of
the new process may individually be attached to new pipes, to existing
'Handle's, or just inherited from the parent (the default.)

The details of how to create the process are passed in the
'CreateProcess' record.  To make it easier to construct a
'CreateProcess', the functions 'proc' and 'shell' are supplied that
fill in the fields with default values which can be overriden as
needed.

'createProcess' returns @(/mb_stdin_hdl/, /mb_stdout_hdl/, /mb_stderr_hdl/, /ph/)@,
where

 * if @'std_in' == 'CreatePipe'@, then @/mb_stdin_hdl/@ will be @Just /h/@,
   where @/h/@ is the write end of the pipe connected to the child
   process's @stdin@.

 * otherwise, @/mb_stdin_hdl/ == Nothing@

Similarly for @/mb_stdout_hdl/@ and @/mb_stderr_hdl/@.

For example, to execute a simple @ls@ command:

>   r <- createProcess (proc "ls" [])

To create a pipe from which to read the output of @ls@:

>   (_, Just hout, _, _) <-
>       createProcess (proc "ls" []){ std_out = CreatePipe }

To also set the directory in which to run @ls@:

>   (_, Just hout, _, _) <-
>       createProcess (proc "ls" []){ cwd = Just "\home\bob",
>                                     std_out = CreatePipe }

Note that @Handle@s provided for @std_in@, @std_out@, or @std_err@ via the
@UseHandle@ constructor will be closed by calling this function. This is not
always the desired behavior. In cases where you would like to leave the
@Handle@ open after spawning the child process, please use 'createProcess_'
instead.

/see 'System.Process.createProcess'/.
-}
createProcess ::
  CreateProcess
  -> IO (Maybe Handle, Maybe Handle, Maybe Handle, Process.ProcessHandle) 
createProcess =
  Process.createProcess . (_CreateProcess #)

-- | This function is almost identical to
-- 'System.Process.createProcess'. The only differences are:
--
-- * 'Handle's provided via 'UseHandle' are not closed automatically.
--
-- * This function takes an extra @String@ argument to be used in creating
--   error messages.
--
-- /see 'System.Process.createProcess_'/.
createProcess_ ::
  String -- ^ function name (for error messages)
  -> CreateProcess
  -> IO (Maybe Handle, Maybe Handle, Maybe Handle, Process.ProcessHandle) 
createProcess_ s =
  Process.createProcess_ s . (_CreateProcess #)

-- | Construct a 'CreateProcess' record for passing to 'createProcess',
-- representing a command to be passed to the shell.
--
-- /see 'System.Process.shell'/.
shell ::
  String
  -> CreateProcess
shell s =
  Process.shell s ^. _CreateProcess

-- | Construct a 'CreateProcess' record for passing to 'createProcess',
-- representing a raw command with arguments.
--
-- See 'RawCommand' for precise semantics of the specified @FilePath@.
--
-- /see 'System.Process.proc'/.
proc ::
  FilePath
  -> [String]
  -> CreateProcess
proc p s =
  Process.proc p s ^. _CreateProcess

-- | Creates a new process to run the specified command with the given
-- arguments, and wait for it to finish.  If the command returns a non-zero
-- exit code, an exception is raised.
--
-- If an asynchronous exception is thrown to the thread executing
-- @callProcess@. The forked process will be terminated and
-- @callProcess@ will wait (block) until the process has been
-- terminated.
--
-- /see 'System.Process.callProcess/.
callProcess ::
  FilePath
  -> [String]
  -> IO ()
callProcess =
  Process.callProcess

-- | Creates a new process to run the specified shell command.  If the
-- command returns a non-zero exit code, an exception is raised.
--
-- If an asynchronous exception is thrown to the thread executing
-- @callCommand@. The forked process will be terminated and
-- @callCommand@ will wait (block) until the process has been
-- terminated.
--
-- /see 'System.Process.callCommand/.
callCommand ::
  String
  -> IO ()
callCommand =
  Process.callCommand

-- | Creates a new process to run the specified raw command with the given
-- arguments. It does not wait for the program to finish, but returns the
-- 'ProcessHandle'.
--
-- /see 'System.Process.spawnProcess/.
spawnProcess ::
  FilePath
  -> [String]
  -> IO Process.ProcessHandle
spawnProcess =
  Process.spawnProcess

-- | Creates a new process to run the specified shell command.
-- It does not wait for the program to finish, but returns the 'ProcessHandle'.
--
-- /see 'System.Process.spawnCommand/.
spawnCommand ::
  String
  -> IO Process.ProcessHandle
spawnCommand =
  Process.spawnCommand

-- | @readCreateProcess@ works exactly like 'readProcess' except that it
-- lets you pass 'CreateProcess' giving better flexibility.
--
-- >  > readCreateProcess (shell "pwd" { cwd = "/etc/" }) ""
-- >  "/etc\n"
--
-- Note that @Handle@s provided for @std_in@ or @std_out@ via the CreateProcess
-- record will be ignored.
--
-- /see 'System.Process.readCreateProcess'/.
readCreateProcess ::
  CreateProcess   
  -> String -- ^ standard input
  -> IO String  -- ^ stdout
readCreateProcess =
  Process.readCreateProcess . (_CreateProcess #)

-- strictly, blocking until the process terminates, and returns the output
-- string. The external process inherits the standard error.
--
-- If an asynchronous exception is thrown to the thread executing
-- @readProcess@, the forked process will be terminated and @readProcess@ will
-- wait (block) until the process has been terminated.
--
-- Output is returned strictly, so this is not suitable for
-- interactive applications.
--
-- This function throws an 'IOError' if the process 'ExitCode' is
-- anything other than 'ExitSuccess'. If instead you want to get the
-- 'ExitCode' then use 'readProcessWithExitCode'.
--
-- Users of this function should compile with @-threaded@ if they
-- want other Haskell threads to keep running while waiting on
-- the result of readProcess.
--
-- >  > readProcess "date" [] []
-- >  "Thu Feb  7 10:03:39 PST 2008\n"
--
-- The arguments are:
--
-- * The command to run, which must be in the $PATH, or an absolute or relative path
--
-- * A list of separate command line arguments to the program
--
-- * A string to pass on standard input to the forked process.
--
-- /see 'System.Process.readProcess/.
readProcess ::
  FilePath -- ^ Filename of the executable (see 'RawCommand' for details)
  -> [String] -- ^ any arguments
  -> String -- ^ standard input
  -> IO String -- ^ stdout
readProcess =
  Process.readProcess

-- | @readCreateProcessWithExitCode@ works exactly like 'readProcessWithExitCode' except that it
-- lets you pass 'CreateProcess' giving better flexibility.
--
-- Note that @Handle@s provided for @std_in@, @std_out@, or @std_err@ via the CreateProcess
-- record will be ignored.
--
-- /see 'System.Process.readCreateProcessWithExitCode'/.
readCreateProcessWithExitCode ::
  CreateProcess   
  -> String -- ^ standard input
  -> IO (ExitCode, String, String) -- ^ exitcode, stdout, stderr
readCreateProcessWithExitCode p =
  fmap (_1 %~ unExitCode) . Process.readCreateProcessWithExitCode (_CreateProcess # p)

-- | @readProcessWithExitCode@ is like @readProcess@ but with two differences:
--
--  * it returns the 'ExitCode' of the process, and does not throw any
--    exception if the code is not 'ExitSuccess'.
--
--  * it reads and returns the output from process' standard error handle,
--    rather than the process inheriting the standard error handle.
--
-- On Unix systems, see 'waitForProcess' for the meaning of exit codes
-- when the process died as the result of a signal.
--
-- /see 'System.Process.readProcessWithExitCode'/.
readProcessWithExitCode ::
  FilePath -- ^ Filename of the executable (see 'RawCommand' for details)
  -> [String] -- ^ any arguments
  -> String -- ^ standard input
  -> IO (ExitCode, String, String) -- ^ exitcode, stdout, stderr
readProcessWithExitCode f a =
  fmap (_1 %~ unExitCode) . Process.readProcessWithExitCode f a

-- | Given a program @/p/@ and arguments @/args/@,
--   @showCommandForUser /p/ /args/@ returns a string suitable for pasting
--   into @\/bin\/sh@ (on Unix systems) or @CMD.EXE@ (on Windows).
--
-- /see 'System.Process.showCommandForUser'/.
showCommandForUser ::
  FilePath
  -> [String]
  -> String
showCommandForUser =
  Process.showCommandForUser

{- | Waits for the specified process to terminate, and returns its exit code.

GHC Note: in order to call @waitForProcess@ without blocking all the
other threads in the system, you must compile the program with
@-threaded@.

On Unix systems, a negative value @'ExitFailure' -/signum/@
indicates that the child was terminated by signal @/signum/@.
The signal numbers are platform-specific, so to test for a specific signal use
the constants provided by "System.Posix.Signals" in the @unix@ package.
Note: core dumps are not reported, use "System.Posix.Process" if you need this
detail.

/see 'System.Process.waitForProcess'/.
-}
waitForProcess ::
  Process.ProcessHandle
  -> IO ExitCode
waitForProcess =
  fmap unExitCode . Process.waitForProcess

{- |
This is a non-blocking version of 'waitForProcess'.  If the process is
still running, 'Nothing' is returned.  If the process has exited, then
@'Just' e@ is returned where @e@ is the exit code of the process.

On Unix systems, see 'waitForProcess' for the meaning of exit codes
when the process died as the result of a signal.

/see 'System.Process.getProcessExitCode'/.
-}
getProcessExitCode ::
  Process.ProcessHandle
  -> IO (Maybe ExitCode) 
getProcessExitCode =
  fmap (fmap unExitCode) . Process.getProcessExitCode

-- | Attempts to terminate the specified process.  This function should
-- not be used under normal circumstances - no guarantees are given regarding
-- how cleanly the process is terminated.  To check whether the process
-- has indeed terminated, use 'getProcessExitCode'.
--
-- On Unix systems, 'terminateProcess' sends the process the SIGTERM signal.
-- On Windows systems, the Win32 @TerminateProcess@ function is called, passing
-- an exit code of 1.
--
-- Note: on Windows, if the process was a shell command created by
-- 'createProcess' with 'shell', or created by 'runCommand' or
-- 'runInteractiveCommand', then 'terminateProcess' will only
-- terminate the shell, not the command itself.  On Unix systems, both
-- processes are in a process group and will be terminated together.
--
-- /see 'System.Process.terminateProcess/.
terminateProcess ::
  Process.ProcessHandle
  -> IO ()
terminateProcess =
  Process.terminateProcess

-- | Sends an interrupt signal to the process group of the given process.
--
-- On Unix systems, it sends the group the SIGINT signal.
--
-- On Windows systems, it generates a CTRL_BREAK_EVENT and will only work for
-- processes created using 'createProcess' and setting the 'create_group' flag
--
-- /see 'System.Process.interruptProcessGroupOf/.
interruptProcessGroupOf ::
  Process.ProcessHandle -- ^ A process in the process group
  -> IO ()
interruptProcessGroupOf =
  Process.interruptProcessGroupOf

-- | Create a pipe for interprocess communication and return a
-- @(readEnd, writeEnd)@ `Handle` pair.
--
-- /see 'System.Process.createPipe'/.
createPipe ::
  IO (Handle, Handle)
createPipe =
  Process.createPipe

module Sys.Exit(
  module ExitCode
, module Process
, Process.ProcessHandle
-- * Exit
, exitWith
, exitWithFailure
, exitWithFailure1
, exitWithSuccess
) where

import Data.NotZero
import qualified System.Exit as Exit
import Sys.ExitCode
import Sys.Process as Process
import Sys.ExitCode as ExitCode
import System.IO
import qualified System.Process as Process
import Prelude

-- ---------------------------------------------------------------------------
-- exitWith

-- | Computation 'exitWith' @code@ throws 'ExitCode' @code@.
-- Normally this terminates the program, returning @code@ to the
-- program's caller.
--
-- On program termination, the standard 'Handle's 'stdout' and
-- 'stderr' are flushed automatically; any other buffered 'Handle's
-- need to be flushed manually, otherwise the buffered data will be
-- discarded.
--
-- A program that fails in any other way is treated as if it had
-- called 'exitFailure'.
-- A program that terminates successfully without calling 'exitWith'
-- explicitly is treated as it it had called 'exitWith' 'ExitSuccess'.
--
-- As an 'ExitCode' is not an 'IOError', 'exitWith' bypasses
-- the error handling in the 'IO' monad and cannot be intercepted by
-- 'catch' from the "Prelude".  However it is a 'SomeException', and can
-- be caught using the functions of "Control.Exception".  This means
-- that cleanup computations added with 'Control.Exception.bracket'
-- (from "Control.Exception") are also executed properly on 'exitWith'.
--
-- Note: in GHC, 'exitWith' should be called from the main program
-- thread in order to exit the process.  When called from another
-- thread, 'exitWith' will throw an 'ExitException' as normal, but the
-- exception will not cause the process itself to exit.
--
exitWith ::
  ExitCode
  -> IO a
exitWith =
  Exit.exitWith . exitCode

exitWithFailure ::
  NotZero Int
  -> IO a
exitWithFailure =
  exitWith . exitFailure

-- | The computation 'exitWithFailure1' is equivalent to
-- 'exitWith' @(@'ExitFailure' /exitfail/@)@,
-- where /exitfail/ is implementation-dependent.
exitWithFailure1 ::
  IO a
exitWithFailure1 =
  exitWith (exitFailure notZero1)

-- | The computation 'exitWithSuccess' is equivalent to
-- 'exitWith' 'ExitSuccess', It terminates the program
-- successfully.
exitWithSuccess ::
  IO a
exitWithSuccess =
  exitWith exitSuccess

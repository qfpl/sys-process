{-# LANGUAGE NoImplicitPrelude #-}

module Sys.ExitCode(
  ExitCode
, _ExitFailure
, _ExitSuccess
, exitFailure
, exitSuccess
, exitFailureP
, exitSuccessP
, exitCode
, unExitCode
) where

import Control.Lens(Prism', prism', (#))
import Data.Int(Int)
import Data.Maybe(Maybe(Nothing, Just))
import Data.NotZero(NotZero, notZero, notZeroElse, notZero1)
import Data.NotZeroOr(Number, NotZeroOr(IsNotZero, OrNotZero), _IsNotZero, _OrNotZero)
import qualified System.Exit as Exit

type ExitCode =
  Number Int

_ExitFailure ::
  Prism' ExitCode (NotZero Int)
_ExitFailure =
  _IsNotZero

_ExitSuccess ::
  Prism' ExitCode ()
_ExitSuccess =
  _OrNotZero

exitFailure ::
  NotZero Int
  -> ExitCode
exitFailure =
  (_ExitFailure #)

exitSuccess ::
  ExitCode
exitSuccess =
  _ExitSuccess # ()

exitFailureP ::
  Prism' Exit.ExitCode Int
exitFailureP =
  prism'
    Exit.ExitFailure
    (\x -> case x of
             Exit.ExitFailure y ->
               Just y
             Exit.ExitSuccess ->
               Nothing)

exitSuccessP ::
  Prism' Exit.ExitCode ()
exitSuccessP =
  prism'
    (\() -> Exit.ExitSuccess)
    (\x -> case x of
         Exit.ExitFailure _ ->
           Nothing
         Exit.ExitSuccess ->
           Just ())

exitCode ::
  ExitCode
  -> Exit.ExitCode
exitCode (IsNotZero a) =
  Exit.ExitFailure (notZero # a)
exitCode (OrNotZero ()) =
  Exit.ExitSuccess

unExitCode ::
  Exit.ExitCode
  -> ExitCode
unExitCode (Exit.ExitSuccess) =
  exitSuccess
unExitCode (Exit.ExitFailure 0) =
  exitSuccess
unExitCode (Exit.ExitFailure n) =
  exitFailure (notZeroElse notZero1 n)

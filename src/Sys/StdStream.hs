{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Sys.StdStream(
  StdStream(..)
, AsStdStream(..)
, AsInherit(..)
, AsUseHandle(..)
, AsCreatePipe(..)
, AsNoStream(..)
) where

import Control.Applicative(Applicative)
import Control.Category(Category(id))
import Control.Lens(Optic', Choice, Profunctor, prism', iso)
import Data.Maybe
import Data.Eq(Eq)
import Data.Functor(Functor)
import Prelude(Show)
import System.IO(Handle)
import qualified System.Process as Process

data StdStream =
  Inherit                    -- ^ Inherit Handle from parent
  | UseHandle Handle         -- ^ Use the supplied Handle
  | CreatePipe               -- ^ Create a new pipe.  The returned
                             -- @Handle@ will use the default encoding
                             -- and newline translation mode (just
                             -- like @Handle@s created by @openFile@).
  | NoStream
  deriving (Eq, Show)

class AsStdStream p f s where
  _StdStream ::
    Optic' p f s StdStream

instance AsStdStream p f StdStream where
  _StdStream =
    id

instance (Profunctor p, Functor f) => AsStdStream p f Process.StdStream where
  _StdStream =
    iso
      (\s -> case s of 
               Process.Inherit ->
                 Inherit
               Process.UseHandle h ->
                 UseHandle h
               Process.CreatePipe ->
                 CreatePipe
               Process.NoStream ->
                 NoStream)
      (\s -> case s of 
               Inherit ->
                 Process.Inherit
               UseHandle h ->
                 Process.UseHandle h
               CreatePipe ->
                 Process.CreatePipe
               NoStream ->
                 Process.NoStream)

class AsInherit p f s where
  _Inherit ::
    Optic' p f s ()

instance AsInherit p f () where
  _Inherit =
    id

instance (Choice p, Applicative f) => AsInherit p f StdStream where
  _Inherit =
    prism'
      (\() -> Inherit)
      (\s -> case s of
               Inherit -> 
                 Just ()
               UseHandle _ ->
                 Nothing
               CreatePipe ->
                 Nothing
               NoStream ->
                 Nothing)

class AsUseHandle p f s where
  _UseHandle ::
    Optic' p f s Handle

instance AsUseHandle p f Handle where
  _UseHandle =
    id

instance (Choice p, Applicative f) => AsUseHandle p f StdStream where
  _UseHandle =
    prism'
      UseHandle
      (\s -> case s of
               Inherit -> 
                 Nothing
               UseHandle h ->
                 Just h
               CreatePipe ->
                 Nothing
               NoStream ->
                 Nothing)

class AsCreatePipe p f s where
  _CreatePipe ::
    Optic' p f s ()

instance AsCreatePipe p f () where
  _CreatePipe =
    id

instance (Choice p, Applicative f) => AsCreatePipe p f StdStream where
  _CreatePipe =
    prism'
      (\() -> CreatePipe)
      (\s -> case s of
               Inherit -> 
                 Nothing
               UseHandle _ ->
                 Nothing
               CreatePipe ->
                 Just ()
               NoStream ->
                 Nothing)

class AsNoStream p f s where
  _NoStream ::
    Optic' p f s ()

instance AsNoStream p f () where
  _NoStream =
    id

instance (Choice p, Applicative f) => AsNoStream p f StdStream where
  _NoStream =
    prism'
      (\() -> NoStream)
      (\s -> case s of
               Inherit -> 
                 Nothing
               UseHandle _ ->
                 Nothing
               CreatePipe ->
                 Nothing
               NoStream ->
                 Just ())

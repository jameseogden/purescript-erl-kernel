module Erl.Kernel.Os
  ( cmd
  , osType
  , OsType(..)
  , OsFamily(..)
  , OsName(..)
  ) where

import Effect (Effect)
import Erl.Atom (Atom)

cmd :: String -> Effect String
cmd = cmdImpl

foreign import cmdImpl :: String -> Effect String

data OsFamily
  = Unix
  | Windows

data OsName
  = Linux
  | Darwin
  | WindowsNt
  | Other Atom

data OsType
  = OsType OsFamily OsName

foreign import osType :: Effect OsType

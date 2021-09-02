module Erl.Kernel.Application
  ( ensureAllStarted
  , stop
  ) where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Erl.Atom (Atom)
import Erl.Data.List (List)
import Foreign (Foreign)

ensureAllStarted :: Atom -> Effect (Either Foreign (List Atom))
ensureAllStarted =
  ensureAllStartedImpl Left Right

stop :: Atom -> Effect (Either Foreign Unit)
stop =
  stopImpl Left Right

foreign import ensureAllStartedImpl :: (Foreign -> Either Foreign (List Atom)) -> (List Atom -> Either Foreign (List Atom)) -> Atom -> Effect (Either Foreign (List Atom))
foreign import stopImpl :: (Foreign -> Either Foreign Unit) -> (Unit -> Either Foreign Unit) -> Atom -> Effect (Either Foreign Unit)

module Erl.Kernel.Time where

import Data.Time.Duration (Milliseconds, Seconds)
import Effect (Effect)

foreign import seconds :: Effect Seconds

foreign import milliseconds :: Effect Milliseconds

module Erl.Kernel.Erlang
  ( makeRef
  , utcNowMs
  , utcNowUs
  , vmNowMs
  , vmNowUs
  , sleep
  , termToString
  , eqFfi
  ) where

import Prelude
import Effect (Effect)
import Erl.Types (Ref, FfiMilliseconds, Microsecond)
import Foreign (Foreign)

foreign import makeRef :: Effect Ref

foreign import utcNowMs :: Effect FfiMilliseconds

foreign import utcNowUs :: Effect Microsecond

foreign import vmNowMs :: Effect Microsecond

foreign import vmNowUs :: Effect Microsecond

foreign import sleep :: FfiMilliseconds -> Effect Unit

foreign import termToString :: Foreign -> String

foreign import eqFfi :: forall a. a -> a -> Boolean

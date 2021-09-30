module Erl.Kernel.Erlang
  ( makeRef
  , utcNowMs
  , utcNowUs
  , vmNowMs
  , vmNowUs
  , sleep
  , termToString
  , eqFfi
  , monitor
  , monotonicTime
  , strictlyMonotonicInt
  , currentTimeOffset
  , monotonicTimeToInstant
  ) where

import Prelude
import Data.DateTime.Instant (Instant, instant)
import Data.Int (toNumber)
import Data.Maybe (Maybe)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Erl.Atom (Atom)
import Erl.Process.Raw (Pid)
import Erl.Types (FfiMilliseconds, Microsecond, MonotonicTime(..), Ref, StrictlyMonotonicInt(..), TimeOffset(..))
import Foreign (Foreign)

foreign import makeRef :: Effect Ref

foreign import utcNowMs :: Effect FfiMilliseconds

foreign import utcNowUs :: Effect Microsecond

foreign import vmNowMs :: Effect Microsecond

foreign import vmNowUs :: Effect Microsecond

foreign import sleep :: FfiMilliseconds -> Effect Unit

foreign import termToString :: Foreign -> String

foreign import eqFfi :: forall a. a -> a -> Boolean

foreign import monitor :: Atom -> Pid -> Effect Unit

monotonicTime :: Effect MonotonicTime
monotonicTime = monotonicTime_ MonotonicTime

foreign import monotonicTime_ :: (Int -> MonotonicTime) -> Effect MonotonicTime

currentTimeOffset :: Effect TimeOffset
currentTimeOffset = currentTimeOffset_ TimeOffset

foreign import currentTimeOffset_ :: (Int -> TimeOffset) -> Effect TimeOffset

strictlyMonotonicInt :: Effect StrictlyMonotonicInt
strictlyMonotonicInt = strictlyMonotonicInt_ StrictlyMonotonicInt

foreign import strictlyMonotonicInt_ :: (Int -> StrictlyMonotonicInt) -> Effect StrictlyMonotonicInt

monotonicTimeToInstant :: MonotonicTime -> TimeOffset -> Maybe Instant
monotonicTimeToInstant (MonotonicTime t) (TimeOffset o) =
  instant $ Milliseconds $ toNumber $ nativeTimeToMilliseconds_ $ t + o

foreign import nativeTimeToMilliseconds_ :: Int -> Int

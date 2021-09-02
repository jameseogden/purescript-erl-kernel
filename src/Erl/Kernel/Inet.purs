module Erl.Kernel.Inet
  ( Port
  , Octet
  , ConnectAddress(..)
  , ConnectError(..)
  , Hextet
  , Hostname
  , PosixError(..)
  , Ip4Address(..)
  , Ip6Address(..)
  , IpAddress(..)
  , IpAddressUnion
  , SocketAddress(..)
  , HostAddress(..)
  , LocalAddress
  , AddressFamily(..)
  , Raw(..)
  , SocketActive(..)
  , SocketMode(..)
  , SocketDeliver(..)
  , CommonOptions
  , SocketType
  , ListenSocket
  , ActiveSocket
  , TcpSocket
  , SendError(..)
  , ActiveError(..)
  , defaultCommonOptions
  , sendErrorToPurs
  , connectErrorToPurs
  , activeErrorToPurs
  , posixErrorToPurs
  , class OptionsToErl
  , optionsToErl
  , makeTerms
  , class OptionToErl
  , optionToErl
  ) where

import Prelude
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol)
import Erl.Atom (atom)
import Erl.Atom.Symbol (toAtom)
import Erl.Atom.Symbol as AtomSymbol
import Erl.Data.Binary (Binary)
import Erl.Data.List (List, nil, (:))
import Erl.Data.Tuple (Tuple4, Tuple8, tuple2, tuple4)
import Erl.Kernel.File as File
import Erl.Types (class ToErl, NonNegInt, toErl)
import Erl.Untagged.Union (type (|+|), Nil, Union)
import Foreign (Foreign, unsafeToForeign)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Prelude (Proxy(..))

type Hostname
  = String

type Port
  = Int

type Octet
  = Int

type Hextet
  = Int

data SocketType

foreign import data ListenSocket :: SocketType

foreign import data ActiveSocket :: SocketType

foreign import data TcpSocket :: SocketType -> Type

instance eq_TcpActiveSocket :: Eq (TcpSocket ActiveSocket) where
  eq = eqSocketImpl

instance eq_TcpListenSocket :: Eq (TcpSocket ListenSocket) where
  eq = eqSocketImpl

foreign import eqSocketImpl :: forall socketType. TcpSocket socketType -> TcpSocket socketType -> Boolean

instance show_TcpActiveSocket :: Show (TcpSocket ActiveSocket) where
  show = showSocketImpl

instance show_TcpListenSocket :: Show (TcpSocket ListenSocket) where
  show = showSocketImpl

foreign import showSocketImpl :: forall socketType. TcpSocket socketType -> String

data PosixError
  = EAddrinuse
  | EAddrnotavail
  | EAfnosupport
  | EAlready
  | EConnaborted
  | EConnrefused
  | EConnreset
  | EDestaddrreq
  | EHostdown
  | EHostunreach
  | EInprogress
  | EIsconn
  | EMsgsize
  | ENetdown
  | ENetunreach
  | ENopkg
  | ENoprotoopt
  | ENotconn
  | ENotty
  | ENotsock
  | EProto
  | EProtonosupport
  | EPrototype
  | ESocktnosupport
  | ETimedout
  | EWouldblock
  | EXbadport
  | EXbadseq
  | File File.PosixError

derive instance eq_PosixError :: Eq PosixError

--derive instance generic_PosixError :: Generic PosixError _
instance show_PosixError :: Show PosixError where
  show _ = "inet posix"

posixErrorToPurs :: Foreign -> Maybe PosixError
posixErrorToPurs f = case posixErrorToPursImpl f of
  Nothing -> File <$> File.posixErrorToPurs f
  posix@(Just _) -> posix

foreign import posixErrorToPursImpl :: Foreign -> Maybe PosixError

type Ip4Address
  = Tuple4 Octet Octet Octet Octet

type Ip6Address
  = Tuple8 Hextet Hextet Hextet Hextet Hextet Hextet Hextet Hextet

data IpAddress
  = Ip4 Ip4Address
  | Ip6 Ip6Address

type IpAddressUnion
  = Union (Ip4Address |+| Ip6Address |+| Nil)

derive instance eqIpAddress :: Eq IpAddress

instance showIpAddress :: Show IpAddress where
  show (Ip4 ip4Address) = "ip4: " <> show ip4Address
  show (Ip6 ip6Address) = "ip6: " <> show ip6Address

type LocalAddress
  = String

data SocketAddress
  = IpAddress IpAddress
  | Any
  | Loopback
  | LocalAddress LocalAddress

derive instance eqSocketAddress :: Eq SocketAddress

data HostAddress
  = Host Hostname
  | Ip IpAddress

derive instance eqHostAddress :: Eq HostAddress

instance showHostAddress :: Show HostAddress where
  show (Host hostName) = "Host " <> show hostName
  show (Ip ipAddress) = "Ip" <> show ipAddress

data ConnectAddress
  = SocketAddr SocketAddress
  | HostAddr HostAddress

instance toErl_ConnectAddress :: ToErl ConnectAddress where
  toErl (SocketAddr socketAddress) = toErl socketAddress
  toErl (HostAddr hostAddress) = toErl hostAddress

data ConnectError
  = ConnectTimeout
  | ConnectPosix PosixError

derive instance eq_ConnectError :: Eq ConnectError

derive instance generic_ConnectError :: Generic ConnectError _

instance show_ConnectError :: Show ConnectError where
  show = genericShow

connectErrorToPurs :: Foreign -> Maybe ConnectError
connectErrorToPurs f = connectErrorToPursImpl ((map ConnectPosix) <<< posixErrorToPurs) f

data AddressFamily
  = Inet4
  | Inet6
  | Local

derive instance eqAddressFamily :: Eq AddressFamily

data Raw
  = Raw NonNegInt NonNegInt Binary

derive instance eqRaw :: Eq Raw

data SocketActive
  = Active
  | Passive
  | Once
  | N Int

derive instance eqSocketActive :: Eq SocketActive

data SocketMode
  = ListData
  | BinaryData

derive instance eqSocketMode :: Eq SocketMode

data SocketDeliver
  = Port
  | Term

derive instance eqSocketDeliver :: Eq SocketDeliver

type CommonOptions r
  = ( active :: Maybe SocketActive
    , buffer :: Maybe NonNegInt
    , deliver :: Maybe SocketDeliver
    , dontroute :: Maybe Boolean
    , header :: Maybe NonNegInt
    , high_msgq_watermark :: Maybe NonNegInt
    , low_msgq_watermark :: Maybe NonNegInt
    , priority :: Maybe Int
    , raw :: Maybe (List Raw)
    , recbuf :: Maybe NonNegInt
    , reuseaddr :: Maybe Boolean
    , sndbuf :: Maybe NonNegInt
    , tos :: Maybe Int
    , tclass :: Maybe NonNegInt
    , ttl :: Maybe NonNegInt
    , recvtos :: Maybe Boolean
    , recvtclass :: Maybe Boolean
    , recvttl :: Maybe Boolean
    , ipv6_v6only :: Maybe Boolean
    | r
    )

defaultCommonOptions ::
  forall r.
  Row.Union r (CommonOptions ()) (CommonOptions r) =>
  Record r -> Record (CommonOptions r)
defaultCommonOptions r =
  Record.union r
    { active: Nothing
    , buffer: Nothing
    , deliver: Nothing
    , dontroute: Nothing
    , header: Nothing
    , high_msgq_watermark: Nothing
    , low_msgq_watermark: Nothing
    , priority: Nothing
    , raw: Nothing
    , recbuf: Nothing
    , reuseaddr: Nothing
    , sndbuf: Nothing
    , tos: Nothing
    , tclass: Nothing
    , ttl: Nothing
    , recvtos: Nothing
    , recvtclass: Nothing
    , recvttl: Nothing
    , ipv6_v6only: Nothing
    }

data SendError
  = SendClosed
  | SendPosix PosixError

derive instance eq_SendError :: Eq SendError

derive instance generic_SendError :: Generic SendError _

instance show_SendError :: Show SendError where
  show = genericShow

sendErrorToPurs :: Foreign -> Maybe SendError
sendErrorToPurs f = sendErrorToPursImpl ((map SendPosix) <<< posixErrorToPurs) f

data ActiveError
  = ActiveClosed
  | ActiveTimeout
  | ActivePosix PosixError

derive instance eq_ActiveError :: Eq ActiveError

derive instance generic_ActiveError :: Generic ActiveError _

instance show_ActiveError :: Show ActiveError where
  show = genericShow

activeErrorToPurs :: Foreign -> Maybe ActiveError
activeErrorToPurs f = activeErrorToPursImpl ((map ActivePosix) <<< posixErrorToPurs) f

optionsToErl ::
  forall r rl.
  RL.RowToList r rl =>
  OptionsToErl r rl =>
  Record r -> List Foreign
optionsToErl = makeTerms (Proxy :: _ rl)

instance toErl_Raw :: ToErl Raw where
  toErl (Raw protocol optionNum val) = unsafeToForeign $ tuple4 (atom "raw") protocol optionNum val

instance toErl_HostAddress :: ToErl HostAddress where
  toErl (Host hostname) = unsafeToForeign $ hostname
  toErl (Ip ipAddress) = toErl ipAddress

instance toErl_AddressFamily :: ToErl AddressFamily where
  toErl Inet4 = unsafeToForeign $ atom "inet4"
  toErl Inet6 = unsafeToForeign $ atom "inet6"
  toErl Local = unsafeToForeign $ atom "local"

instance toErl_SocketMode :: ToErl SocketMode where
  toErl ListData = unsafeToForeign $ atom "list"
  toErl BinaryData = unsafeToForeign $ atom "binary"

instance toErl_IpAddress :: ToErl IpAddress where
  toErl (Ip4 ipAddress) = toErl ipAddress
  toErl (Ip6 ipAddress) = toErl ipAddress

instance toErl_SocketAddress :: ToErl SocketAddress where
  toErl (IpAddress ipAddress) = toErl ipAddress
  toErl (LocalAddress localAddress) = toErl localAddress
  toErl Any = unsafeToForeign $ atom "any"
  toErl Loopback = unsafeToForeign $ atom "loopback"

instance toErl_SocketDeliver :: ToErl SocketDeliver where
  toErl Port = unsafeToForeign $ atom "port"
  toErl Term = unsafeToForeign $ atom "term"

instance toErl_SocketActive :: ToErl SocketActive where
  toErl Active = unsafeToForeign $ atom "true"
  toErl Passive = unsafeToForeign $ atom "false"
  toErl Once = unsafeToForeign $ atom "once"
  toErl (N n) = unsafeToForeign $ n

class OptionToErl :: Symbol -> Type -> Constraint
class OptionToErl sym option where
  optionToErl :: List Foreign -> AtomSymbol.Atom sym -> option -> List Foreign

instance optionToErl_Raw :: OptionToErl "raw" (List Raw) where
  optionToErl acc _name raws = foldl (\acc raw -> toErl raw : acc) acc raws
else instance optionToErl_List :: (IsSymbol name, ToErl a) => OptionToErl name (List a) where
  optionToErl acc name val = (unsafeToForeign $ tuple2 (toAtom name) (toErl <$> val)) : acc
else instance optionToErl_Other :: (IsSymbol name, ToErl a) => OptionToErl name a where
  optionToErl acc name val = (unsafeToForeign $ tuple2 (toAtom name) (toErl val)) : acc

class OptionsToErl :: Row Type -> RL.RowList Type -> Constraint
class OptionsToErl r rl where
  makeTerms :: Proxy rl -> Record r -> List Foreign

instance optionsToErl_nil :: OptionsToErl r RL.Nil where
  makeTerms _ _r = nil

instance optionsToErl_consMaybe ::
  ( IsSymbol sym
  , Row.Cons sym (Maybe a) t1 r
  , OptionsToErl r tail
  , OptionToErl sym a
  ) =>
  OptionsToErl r (RL.Cons sym (Maybe a) tail) where
  makeTerms _ r = do
    let
      tail = makeTerms (Proxy :: _ tail) r
    maybe tail (optionToErl tail (AtomSymbol.atom :: AtomSymbol.Atom sym)) $ Record.get (Proxy :: _ sym) r
else instance optionsToErl_cons ::
  ( IsSymbol sym
  , Row.Cons sym a t1 r
  , OptionsToErl r tail
  , OptionToErl sym a
  ) =>
  OptionsToErl r (RL.Cons sym a tail) where
  makeTerms _ r = do
    let
      tail = makeTerms (Proxy :: _ tail) r
    optionToErl tail (AtomSymbol.atom :: AtomSymbol.Atom sym) $ Record.get (Proxy :: _ sym) r

------------------------------------------------------------------------------
-- FFI
foreign import sendErrorToPursImpl :: (Foreign -> Maybe SendError) -> Foreign -> Maybe SendError

foreign import activeErrorToPursImpl :: (Foreign -> Maybe ActiveError) -> Foreign -> Maybe ActiveError

foreign import connectErrorToPursImpl :: (Foreign -> Maybe ConnectError) -> Foreign -> Maybe ConnectError

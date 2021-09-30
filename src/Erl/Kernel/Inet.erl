-module(erl_kernel_inet@foreign).

-export([ posixErrorToPursImpl/1
        , sendErrorToPursImpl/2
        , activeErrorToPursImpl/2
        , connectErrorToPursImpl/2
        , parseIpAddress/1
        , ntoa4/1
        , ntoa6/1
        ]).

%% FFI Helpers
-define(just(A), {just, A}).
-define(nothing, {nothing}).
-define(ip4(A), {ip4, A}).
-define(ip6(A), {ip6, A}).


posixErrorToPursImpl(eaddrinuse) -> ?just({eaddrinuse});
posixErrorToPursImpl(eaddrnotavail) -> ?just({eaddrnotavail});
posixErrorToPursImpl(eafnosupport) -> ?just({eafnosupport});
posixErrorToPursImpl(ealready) -> ?just({ealready});
posixErrorToPursImpl(econnaborted) -> ?just({econnaborted});
posixErrorToPursImpl(econnrefused) -> ?just({econnrefused});
posixErrorToPursImpl(econnreset) -> ?just({econnreset});
posixErrorToPursImpl(edestaddrreq) -> ?just({edestaddrreq});
posixErrorToPursImpl(ehostdown) -> ?just({ehostdown});
posixErrorToPursImpl(ehostunreach) -> ?just({ehostunreach});
posixErrorToPursImpl(einprogress) -> ?just({einprogress});
posixErrorToPursImpl(eisconn) -> ?just({eisconn});
posixErrorToPursImpl(emsgsize) -> ?just({emsgsize});
posixErrorToPursImpl(enetdown) -> ?just({enetdown});
posixErrorToPursImpl(enetunreach) -> ?just({enetunreach});
posixErrorToPursImpl(enopkg) -> ?just({enopkg});
posixErrorToPursImpl(enoprotoopt) -> ?just({enoprotoopt});
posixErrorToPursImpl(enotconn) -> ?just({enotconn});
posixErrorToPursImpl(enotty) -> ?just({enotty});
posixErrorToPursImpl(enotsock) -> ?just({enotsock});
posixErrorToPursImpl(eproto) -> ?just({eproto});
posixErrorToPursImpl(eprotonosupport) -> ?just({eprotonosupport});
posixErrorToPursImpl(eprototype) -> ?just({eprototype});
posixErrorToPursImpl(esocktnosupport) -> ?just({esocktnosupport});
posixErrorToPursImpl(etimedout) -> ?just({etimedout});
posixErrorToPursImpl(ewouldblock) -> ?just({ewouldblock});
posixErrorToPursImpl(exbadport) -> ?just({exbadport});
posixErrorToPursImpl(exbadseq) -> ?just({exbadseq});
posixErrorToPursImpl(_) -> ?nothing.

sendErrorToPursImpl(_PosixErr, closed) -> ?just({sendClosed});
sendErrorToPursImpl(PosixErr, Other) -> PosixErr(Other).

activeErrorToPursImpl(_PosixErr, closed) -> ?just({activeClosed});
activeErrorToPursImpl(_PosixErr, timeout) -> ?just({activeTimeout});
activeErrorToPursImpl(PosixErr, Other) -> PosixErr(Other).

connectErrorToPursImpl(_PosixErr, timeout) -> ?just({connectTimeout});
connectErrorToPursImpl(PosixErr, Other) -> PosixErr(Other).

parseIpAddress(Str) ->
  case inet:parse_address(binary_to_list(Str)) of
      {ok, Ip} when tuple_size(Ip) == 4 ->
          ?just(?ip4(Ip));
      {ok, Ip} when tuple_size(Ip) == 8 ->
          ?just(?ip6(Ip));
      _ ->
          ?nothing
  end.

ntoa4(Addr) ->
  case inet:ntoa(Addr) of
    Str when is_list(Str) -> ?just(list_to_binary(Str));
    _ -> ?nothing
  end.

ntoa6(Addr) ->
  ntoa4(Addr).

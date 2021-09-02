-module(erl_kernel_inet@foreign).

-export([ posixErrorToPursImpl/1
        , sendErrorToPursImpl/2
        , activeErrorToPursImpl/2
        , connectErrorToPursImpl/2
        , eqSocketImpl/2
        , showSocketImpl/1
        ]).

eqSocketImpl(Socket, Socket) -> true;
eqSocketImpl(_, _) -> false.

showSocketImpl(Socket) -> list_to_binary(lists:flatten(io_lib:format("~p", [Socket]))).

posixErrorToPursImpl(eaddrinuse) -> {just, {eaddrinuse}};
posixErrorToPursImpl(eaddrnotavail) -> {just, {eaddrnotavail}};
posixErrorToPursImpl(eafnosupport) -> {just, {eafnosupport}};
posixErrorToPursImpl(ealready) -> {just, {ealready}};
posixErrorToPursImpl(econnaborted) -> {just, {econnaborted}};
posixErrorToPursImpl(econnrefused) -> {just, {econnrefused}};
posixErrorToPursImpl(econnreset) -> {just, {econnreset}};
posixErrorToPursImpl(edestaddrreq) -> {just, {edestaddrreq}};
posixErrorToPursImpl(ehostdown) -> {just, {ehostdown}};
posixErrorToPursImpl(ehostunreach) -> {just, {ehostunreach}};
posixErrorToPursImpl(einprogress) -> {just, {einprogress}};
posixErrorToPursImpl(eisconn) -> {just, {eisconn}};
posixErrorToPursImpl(emsgsize) -> {just, {emsgsize}};
posixErrorToPursImpl(enetdown) -> {just, {enetdown}};
posixErrorToPursImpl(enetunreach) -> {just, {enetunreach}};
posixErrorToPursImpl(enopkg) -> {just, {enopkg}};
posixErrorToPursImpl(enoprotoopt) -> {just, {enoprotoopt}};
posixErrorToPursImpl(enotconn) -> {just, {enotconn}};
posixErrorToPursImpl(enotty) -> {just, {enotty}};
posixErrorToPursImpl(enotsock) -> {just, {enotsock}};
posixErrorToPursImpl(eproto) -> {just, {eproto}};
posixErrorToPursImpl(eprotonosupport) -> {just, {eprotonosupport}};
posixErrorToPursImpl(eprototype) -> {just, {eprototype}};
posixErrorToPursImpl(esocktnosupport) -> {just, {esocktnosupport}};
posixErrorToPursImpl(etimedout) -> {just, {etimedout}};
posixErrorToPursImpl(ewouldblock) -> {just, {ewouldblock}};
posixErrorToPursImpl(exbadport) -> {just, {exbadport}};
posixErrorToPursImpl(exbadseq) -> {just, {exbadseq}};
posixErrorToPursImpl(_) -> {nothing}.

sendErrorToPursImpl(_PosixErr, closed) -> {just, {sendClosed}};
sendErrorToPursImpl(PosixErr, Other) -> PosixErr(Other).

activeErrorToPursImpl(_PosixErr, closed) -> {just, {activeClosed}};
activeErrorToPursImpl(_PosixErr, timeout) -> {just, {activeTimeout}};
activeErrorToPursImpl(PosixErr, Other) -> PosixErr(Other).

connectErrorToPursImpl(_PosixErr, timeout) -> {just, {connectTimeout}};
connectErrorToPursImpl(PosixErr, Other) -> PosixErr(Other).

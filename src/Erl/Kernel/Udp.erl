-module(erl_kernel_udp@foreign).

-export([ openImpl/4
        , sendImpl/6
        , recvImpl/5
        , portImpl/1
        , setoptsImpl/4
        , eqSocketImpl/2
        , showSocketImpl/1
        , openErrorToPursImpl/2
        , sendErrorToPursImpl/2
        , receiveErrorToPursImpl/2
        ]).

openImpl(Left, Right, Port, Options) ->
    fun() ->
            case gen_udp:open(Port, Options) of
                {ok, Socket} ->
                    Right(Socket);
                {error, Reason} ->
                    Left(Reason)
            end
    end.

sendImpl(Left, Right, Socket, Host, Port, Packet) ->
    fun() ->
            Host2 = if is_binary(Host) -> binary_to_list(Host);
                       true -> Host
                    end,
            case gen_udp:send(Socket, Host2, Port, Packet) of
                ok ->
                    Right(unit);
                {error, Reason} ->
                    Left(Reason)
            end
    end.

recvImpl(Left, Data, DataAnc, Socket, Timeout) ->
    fun() ->
            case gen_udp:recv(Socket, 0, Timeout) of
                {ok, {Address, Port, Packet}} ->
                    Data(Address, Port, Packet);
                {ok, {Address, Port, AncData, Packet}} ->
                    DataAnc(Address, Port, AncData, Packet);
                {error, Reason} ->
                    Left(Reason)
            end
    end.

portImpl(Socket) ->
    fun() ->
            case inet:port(Socket) of
                {ok, Port} ->
                    {just, Port};
                _ ->
                    {nothing}
            end
    end.

setoptsImpl(Left, Right, Socket, Options) ->
    fun() ->
            case inet:setopts(Socket, Options) of
                ok ->
                    Right(unit);
                {error, Reason} ->
                    Left(Reason)
            end
    end.

eqSocketImpl(Socket, Socket) -> true;
eqSocketImpl(_, _) -> false.

showSocketImpl(Socket) -> list_to_binary(lists:flatten(io_lib:format("~p", [Socket]))).

openErrorToPursImpl(_PosixErr, system_limit) -> {just, {openSystemLimit}};
openErrorToPursImpl(PosixErr, Other) -> PosixErr(Other).

sendErrorToPursImpl(_PosixErr, not_owner) -> {just, {sendNotOwner}};
sendErrorToPursImpl(PosixErr, Other) -> PosixErr(Other).

receiveErrorToPursImpl(_PosixErr, not_owner) -> {just, {receiveNotOwner}};
receiveErrorToPursImpl(_PosixErr, timeout) -> {just, {receiveTimeout}};
receiveErrorToPursImpl(PosixErr, Other) -> PosixErr(Other).

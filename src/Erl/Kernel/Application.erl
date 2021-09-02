-module(erl_kernel_application@foreign).

-export([ ensureAllStartedImpl/3
        , stopImpl/3
        ]).

ensureAllStartedImpl(Left, Right, App) ->
    fun() ->
            case application:ensure_all_started(App) of
                {ok, Apps} ->
                    Right(Apps);
                {error, Reason} ->
                    Left(Reason)
            end
    end.

stopImpl(Left, Right, App) ->
    fun() ->
            case application:stop(App) of
                ok ->
                    Right(unit);
                {error, Reason} ->
                    Left(Reason)
            end
    end.

-module(erl_kernel_time@foreign).

-export([seconds/0
        ,milliseconds/0
        ]).


seconds() -> fun() ->
                   erlang:system_time(second)
               end.


milliseconds() -> fun() ->
                   erlang:system_time(millisecond)
               end.


-module(erl_kernel_os@foreign).

-export([cmdImpl/1]).

cmdImpl(Command) -> fun() ->
                      os:cmd(binary_to_list(Command), #{ max_size => infinity })
                    end.

-module(erl_kernel_os@foreign).

-export([ cmdImpl/1
        , osType/0
        ]).

cmdImpl(Command) -> fun() ->
                      os:cmd(binary_to_list(Command), #{ max_size => infinity })
                    end.

osType() ->
    fun() ->
            {osFamily, osName} = os:type(),
            { osType
            , case osFamily of
                  unix -> {unix};
                 win32 -> {windows}
              end
            , case osName of
                  darwin -> {darwin};
                  linux -> {linux};
                  nt -> {windowNt};
                  Other -> {other, Other}
              end
            }
    end.

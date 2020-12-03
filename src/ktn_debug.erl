%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ktn_debug: functions useful for debugging
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(ktn_debug).

-export(
  [ ppst/1
  ]).

-spec ppst([any()]) ->
  [any()].
ppst(StackTrace) ->
  F =
    fun({_Module, Function, Arity, Props}) ->
      File = proplists:get_value(file, Props),
      Line = proplists:get_value(line, Props),
      io_lib:format("\t~s:~p:~p/~p~n", [File, Line, Function, Arity])
    end,
  lists:flatten(["\n", lists:map(F, StackTrace), "\n"]).

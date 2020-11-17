%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ktn_debug: functions useful for debugging
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(ktn_debug).

-ifdef(OTP_RELEASE).
-define(GET_STACKTRACE, []).
-else.
-define(GET_STACKTRACE, erlang:get_stacktrace()).
-endif.

-export(
  [ ppst/0
  , ppst/1
  ]).

-spec ppst() ->
  [any()].
ppst() ->
  ppst(?GET_STACKTRACE).

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

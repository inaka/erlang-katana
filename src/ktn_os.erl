%% @doc Utility functions to run commands in the underlying OS.
-module(ktn_os).

-export([command/1, command/2]).

-type opts() :: #{ log_fun => fun((iodata()) -> any())
                 , timeout => integer()
                 , monitor => reference()
                 }.
-type exit_status() :: integer().

-spec command(iodata()) -> {exit_status(), string()}.
command(Cmd) ->
  Opts = #{log_fun => fun error_logger:info_msg/1},
  command(Cmd, Opts).

-spec command(iodata(), opts()) -> {exit_status(), string()}.
command(Cmd, Opts) ->
  PortOpts = [hide, stream, exit_status, eof, stderr_to_stdout],
  Port = open_port({spawn, shell_cmd()}, PortOpts),
  MonRef = erlang:monitor(port, Port),
  true = erlang:port_command(Port, make_cmd(Cmd)),
  Result = get_data(Port, Opts#{monitor => MonRef}, []),
  _ = demonitor(MonRef, [flush]),
  Result.

-spec get_data(port(), opts(), [string()]) -> {exit_status(), string()}.
get_data(Port, Opts, Data) ->
  %% Get timeout option or an hour if undefined.
  Timeout = maps:get(timeout, Opts, 600000),
  MonRef = maps:get(monitor, Opts),
  receive
    {Port, {data, NewData}} ->
      case maps:get(log_fun, Opts, undefined) of
        Fun when is_function(Fun) -> Fun(NewData);
        undefined -> ok
      end,
      get_data(Port, Opts, [NewData | Data]);
    {Port, eof} ->
      catch port_close(Port),
      flush_until_down(Port, MonRef),
      receive
        {Port, {exit_status, ExitStatus}} ->
          {ExitStatus, lists:flatten(lists:reverse(Data))}
      end;
    {'DOWN', MonRef, _, _, Reason} ->
      flush_exit(Port),
      exit({error, Reason, lists:flatten(lists:reverse(Data))})
  after
    Timeout -> exit(timeout)
  end.

-spec make_cmd(string()) -> iodata().
make_cmd(Cmd) ->
  %% We insert a new line after the command, in case the command
  %% contains a comment character.
  [$(, unicode:characters_to_binary(Cmd), "\n) </dev/null; exit\n"].

-spec shell_cmd() -> string().
shell_cmd() -> "/bin/sh -s unix:cmd".

%% When port_close returns we know that all the
%% messages sent have been sent and that the
%% DOWN message is after them all.
flush_until_down(Port, MonRef) ->
  receive
    {Port, {data, _Bytes}} ->
      flush_until_down(Port, MonRef);
    {'DOWN', MonRef, _, _, _} ->
      flush_exit(Port)
  end.

%% The exit signal is always delivered before
%% the down signal, so we can be sure that if there
%% was an exit message sent, it will be in the
%% mailbox now.
flush_exit(Port) ->
  receive
    {'EXIT',  Port,  _} ->
      ok
  after 0 ->
    ok
  end.

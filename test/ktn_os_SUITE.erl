-module(ktn_os_SUITE).

-export([all/0]).

-export([command/1]).

-define(EXCLUDED_FUNS,
        [
         module_info,
         all,
         test,
         init_per_suite,
         end_per_suite
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
    Exports = ?MODULE:module_info(exports),
    [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec command(config()) -> ok.
command(_Config) ->
  Opts = #{log_fun => fun(_) -> ok end},

  {0, "/\n"} = ktn_os:command("cd /; pwd", Opts),

  {ok, Cwd} = file:get_cwd(),
  Result = Cwd ++ "\n",
  {0, Result} = ktn_os:command("pwd", Opts),

  {2, _} = ktn_os:command("pwd; ls w4th3v3r", Opts),

  Result2 = Result ++ "Hi\n",
  {0, Result2} = ktn_os:command("pwd; echo Hi", #{}),

  {0, "/\n"} = ktn_os:command("cd /; pwd"),

  ok = try ktn_os:command("sleep 5", #{timeout => 1000})
       catch _:timeout -> ok end,

  FilterFun =
    fun(Line) ->
        case re:run(Line, "=INFO REPORT==== .* ===") of
          nomatch -> false;
          {match, _}-> true
        end
    end,

  ct:capture_start(),
  {0, "/\n"} = ktn_os:command("cd /; pwd"),
  ct:capture_stop(),
  Lines = ct:capture_get([]),
  ListFun = fun(Line) -> FilterFun(Line) end,
  [_ | _] = lists:filter(ListFun, Lines),

  ct:comment("Check result when process is killed"),
  Self = self(),
  YesFun = fun() ->
               case ktn_os:command("yes > /dev/null") of
                 {ExitStatus, _} when ExitStatus =/= 0 -> Self ! ok;
                 UnexpectedResult -> Self ! {error, UnexpectedResult}
               end
           end,
  erlang:spawn_link(YesFun),
  [] = os:cmd("pkill yes"),
  ok  = receive X -> X after 1000 -> timeout end,

  ct:comment("Check result when port is closed"),
  Yes2Fun =
    fun() ->
      process_flag(trap_exit, true),
      try ktn_os:command("yes > /dev/null") of
        UnexpectedResult -> Self ! {error, UnexpectedResult}
      catch
        exit:{error, _, _} -> Self ! ok
      end
    end,
  FindPort =
    fun(Proc) ->
      fun() ->
        {links, Links} = erlang:process_info(Proc, links),
        [_] = [P || P <- Links, is_port(P)]
      end
    end,
  Pid = erlang:spawn(Yes2Fun),
  try
    [Port] = ktn_task:wait_for_success(FindPort(Pid)),
    port_close(Port),
    ok  = receive X2 -> X2 after 1000 -> timeout end
  after
    exit(Pid, kill)
  end,

  ct:comment("Check result when port is killed"),
  Pid2 = erlang:spawn(Yes2Fun),
  try
    [Port2] = ktn_task:wait_for_success(FindPort(Pid2)),
    exit(Port2, kill),
    ok  = receive X3 -> X3 after 1000 -> timeout end
  after
    exit(Pid2, kill)
  end.

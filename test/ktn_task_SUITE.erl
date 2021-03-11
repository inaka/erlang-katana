-module(ktn_task_SUITE).

-export([
         all/0,
         init_per_testcase/2,
         end_per_testcase/2
        ]).

-export([
         wait_for/1,
         wait_for_error/1
        ]).

-define(EXCLUDED_FUNS,
        [
          module_info,
          all,
          init_per_case,
          end_per_case
        ]).

-type config() :: [{atom(), term()}].
-type fail() :: {fail, {integer(), integer()}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom(), ...].
all() ->
    Exports = ?MODULE:module_info(exports),
    [F || {F, 1} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

-spec init_per_testcase(term(), config()) -> config().
init_per_testcase(_Case, Config) ->
    ets:new(?MODULE, [named_table, public]),
    ets:insert(?MODULE, {counter, counters:new(1, [write_concurrency])}),
    Config.

-spec end_per_testcase(term(), config()) -> config().
end_per_testcase(_Case, Config) ->
    ets:delete(?MODULE),
    Config.

% Took this idea from
% https://gist.github.com/garazdawi/17cdb5914b950f0acae21d9fcf7e8d41
-spec cnt_incr() -> ok.
cnt_incr() ->
    counters:add(
        ets:lookup_element(?MODULE, counter, 2), 1, 1).

-spec cnt_read() -> integer().
cnt_read() ->
    counters:get(
        ets:lookup_element(?MODULE, counter, 2), 1).

-spec fails_until(integer()) -> fail() | ok.
fails_until(X) ->
    fails_until(X, cnt_read()).

-spec fails_until(integer(), integer()) -> fail() | ok.
fails_until(X, Curr) when X > Curr ->
    cnt_incr(),
    throw({fail, {Curr, X}});
fails_until(_X, _Curr) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec wait_for(config()) -> ok.
wait_for(_Config) ->
    ok = ktn_task:wait_for(fun() -> ok end, ok, 1, 1),
    ok = ktn_task:wait_for(fun() -> fails_until(10) end, ok, 0, 11),
    ok.

-spec wait_for_error(config()) -> ok.
wait_for_error(_Config) ->
    {error, {timeout, {fail}}} =
              ktn_task:wait_for(fun() -> throw({fail}) end, ok, 0, 3),
    {error, {timeout, {fail, {10, 15}}}} =
              ktn_task:wait_for(fun() -> fails_until(15) end, ok, 0, 11),
    ok.

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
-type task(T) :: fun(() -> T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom(), ...].
all() ->
    Exports = ?MODULE:module_info(exports),
    [F || {F, 1} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

-spec init_per_testcase(term(), config()) -> config().
init_per_testcase(_Case, Config) ->
    _Tid = ets:new(?MODULE, [named_table, public]),
    Config.

-spec end_per_testcase(term(), config()) -> config().
end_per_testcase(_Case, Config) ->
    ets:delete(?MODULE),
    Config.

% Took this idea from
% https://gist.github.com/garazdawi/17cdb5914b950f0acae21d9fcf7e8d41
-spec cnt_incr(reference()) -> ok.
cnt_incr(Ref) ->
    counters:add(
        ets:lookup_element(?MODULE, Ref, 2), 1, 1).

-spec cnt_read(reference()) -> integer().
cnt_read(Ref) ->
    counters:get(
        ets:lookup_element(?MODULE, Ref, 2), 1).

-spec fail_until(integer()) -> task(_).
fail_until(X) ->
    Ref = make_ref(),
    ets:insert(?MODULE, {Ref, counters:new(1, [write_concurrency])}),
    fun () ->
        Curr = cnt_read(Ref),
        fail_until(X, Curr, Ref)
    end.

-spec fail_until(integer(), integer(), reference()) -> no_return() | ok.
fail_until(X, Curr, Ref) when X > Curr ->
    cnt_incr(Ref),
    throw({fail, {X, Curr, Ref}});
fail_until(_X, _Curr, _Ref) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec wait_for(config()) -> ok.
wait_for(_Config) ->
    ok = ktn_task:wait_for(fun() -> ok end, ok, 1, 1),
    ok = ktn_task:wait_for(fail_until(10), ok, 0, 11).

-spec wait_for_error(config()) -> ok.
wait_for_error(_Config) ->
    {error, {timeout, {fail}}} =
              ktn_task:wait_for(fun() -> throw({fail}) end, ok, 0, 3),
    {error, {timeout, {fail, {15, 10, _Ref}}}} =
              ktn_task:wait_for(fail_until(15), ok, 0, 11),
    ok.

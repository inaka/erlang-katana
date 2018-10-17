-module(ktn_date_SUITE).

-export([all/0]).

-export([
         shift_days/1,
         shift_months/1
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
    [
     shift_days,
     shift_months
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec shift_days(config()) -> ok.
shift_days(_Config) ->
    Datetime = {{2018, 8, 15}, {12, 12, 12}},
    {{2018, 8, 16}, {12, 12, 12}} = ktn_date:shift_days(Datetime, 1),
    {{2018, 8, 14}, {12, 12, 12}} = ktn_date:shift_days(Datetime, -1),
    {{2018, 9, 4},  {12, 12, 12}} = ktn_date:shift_days(Datetime, 20),
    ok.

-spec shift_months(config()) -> ok.
shift_months(_Config) ->
    {2018, 10, 15} = ktn_date:shift_months({2018, 8, 15}, 2),
    {2018, 6,  15} = ktn_date:shift_months({2018, 8, 15}, -2),
    {2019, 2,  15} = ktn_date:shift_months({2018, 8, 15}, 6),

    {2018, 10, 31} = ktn_date:shift_months({2018, 8, 31}, 2),
    {2018, 6,  30} = ktn_date:shift_months({2018, 8, 31}, -2),
    {2019, 2,  28} = ktn_date:shift_months({2018, 8, 31}, 6),

    ok = try
        _ = ktn_date:shift_months({0, 8, 31}, -9),
        out_of_bounds_expected
    catch
        error:out_of_bounds -> ok
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ktn_date: functions useful for handling dates and time values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(ktn_date).

-define(SECONDS_IN_A_DAY, 86400).

-export([
         now_human_readable/0,
         shift_days/2,
         shift_months/2
        ]).

-type date()     :: {date, {non_neg_integer(), 1..12, 1..31}}.
-type datetime() :: { datetime
                    , {{integer(), 1..12, 1..31}
                    , {1..24, 1..60, 1..60}}
                    }.

-export_type(
  [ date/0
  , datetime/0
  ]).

%% @doc Returns the current date in a human readable format binary.
-spec now_human_readable() -> binary().
now_human_readable() ->
    TimeStamp = {_, _, Micro} = os:timestamp(),
    {{Year, Month, Day},
     {Hour, Minute, Second}} = calendar:now_to_universal_time(TimeStamp),
    DateList = io_lib:format("~p-~2..0B-~2..0BT~p:~p:~p.~6..0wZ",
                             [Year, Month, Day, Hour, Minute, Second, Micro]),
    list_to_binary(DateList).

%% @doc Moves the received datetime `N' days to the future (or to the past)
-spec shift_days(calendar:datetime(), integer()) -> calendar:datetime().
shift_days(Datetime, N) ->
    Shift = ?SECONDS_IN_A_DAY * N,
    Secs = calendar:datetime_to_gregorian_seconds(Datetime),
    calendar:gregorian_seconds_to_datetime(Secs + Shift).

%% @doc Moves the received date `N' months to the future (or to the past)
-spec shift_months(calendar:date(), integer()) -> calendar:date().
shift_months({Y, M, D}, N) ->
    %% in order for the modular arithmetic to work, months in this function
    %% range from 0 to 11 (January to December)
    TotalMonths = 12*Y + M-1 + N,

    case TotalMonths >= 0 of
        true ->
            Month = TotalMonths rem 12,
            Year = (TotalMonths - Month) div 12,

            %% add one back to the month to fix our tricky mod 12
            find_valid_date({Year, Month+1, D});
        false ->
            error(out_of_bounds)
    end.

%% @doc Returns `Date' if valid. Otherwise, returns `Date' replacing `Day`
%% with the last day of the month.
find_valid_date(Date) ->
    case calendar:valid_date(Date) of
        true ->
            Date;
        false ->
            {Y, M, _} = Date,
            {Y, M, calendar:last_day_of_the_month(Y, M)}
    end.
-module(ktn_meta_suite_SUITE).

-include_lib("mixer/include/mixer.hrl").
-compile([{parse_transform, mixer}]).
-mixin([{ ktn_meta_SUITE
        , [ all/0
          , xref/1
          , dialyzer/1
          , elvis/1
          ]
        }]).

-export([init_per_suite/1]).

-type config() :: [{atom(), term()}].

-spec init_per_suite(config()) -> config().
-ifdef(rebar).
init_per_suite(Config) ->
    LibDir = code:lib_dir(katana),
    BuildRoot = filename:dirname(filename:dirname(LibDir)),
    {ok, Dirlist} = file:list_dir(BuildRoot),
    [Plt|_] = lists:filter(fun (File) ->
                                   case re:run(File, "plt") of
                                       {match, _} -> true;
                                       nomatch -> false
                                   end
                           end,
                           Dirlist),
    Pltfile = filename:join(BuildRoot, Plt),
  [ {base_dir, LibDir}
  , {elvis_config, filename:join([LibDir, "test", "elvis.config"])}
  , {plts, [Pltfile]}
  | Config
  ].
-else.
init_per_suite(Config) ->
  [ {base_dir, "../.."}
  , {elvis_config, "../../test/elvis.config"}
  , {plts, ["../../.katana.plt"]}
  | Config
  ].
-endif.

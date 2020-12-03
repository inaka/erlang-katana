-module(ktn_meta_SUITE).

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([xref/1, dialyzer/1, elvis/1]).

-type config() :: [{atom(), term()}].

-spec all() -> [dialyzer | elvis | xref].
all() -> [dialyzer, elvis, xref].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) -> [{application, katana} | Config].

-spec end_per_suite(config()) -> config().
end_per_suite(Config) -> Config.

%% @doc xref's your code using xref_runner.
%%      Available Options:
%%      - xref_config: Configuration for xref_runner
%%      - xref_checks: List of xref checks to perform
-spec xref(config()) -> {comment, []}.
xref(Config) ->
  XrefConfig =
    case test_server:lookup_config(xref_config, Config) of
      undefined ->
        #{ dirs => dirs(Config)
         , xref_defaults => [ {verbose, true}
                            , {recurse, true}
                            , {builtins, true}
                            ]
         };
      XC -> XC
    end,
  Checks =
    case test_server:lookup_config(xref_checks, Config) of
      undefined ->
        [ undefined_function_calls
        , locals_not_used
        , deprecated_function_calls
        ];
      Cs -> Cs
    end,

  ct:comment("There are no Warnings"),
  [] =
    [ Warning
    || Check <- Checks, Warning <- xref_runner:check(Check, XrefConfig)],

  {comment, ""}.

%% @doc dialyzes your code.
%%      By default it uses all the plts in the project root folder.
%%      You can change that by providing a 'plts' parameter in Config.
%%      You can also change the warnings using the 'dialyzer_warnings' parameter
-spec dialyzer(config()) -> {comment, []}.
dialyzer(Config) ->
  Plts = plts(Config),
  Dirs = dirs(Config),
  Warnings =
    case test_server:lookup_config(dialyzer_warnings, Config) of
      undefined -> [error_handling, race_conditions, unmatched_returns];
      Ws -> Ws
    end,

  ct:comment("Dialyzer must emit no warnings"),
  Opts =
    [ {analysis_type, succ_typings}
    , {plts,          Plts}
    , {files_rec,     Dirs}
    , {check_plt,     true}
    , {warnings,      Warnings}
    , {get_warnings,  true}
    ],
  [] = [dialyzer:format_warning(W, basename) || W <- dialyzer:run(Opts)],
  {comment, ""}.

%% @doc Checks your code with elvis
%%      Available Options:
%%      - elvis_config: Location of elvis.config
-spec elvis(config()) -> {comment, []}.
elvis(Config) ->
  ElvisConfig =
    case test_server:lookup_config(elvis_config, Config) of
      undefined ->
        BaseDir = base_dir(Config),
        ConfigFile =
          case is_rebar3_project(Config) of
            true  -> filename:join(BaseDir, "../../../../elvis.config");
            false -> filename:join(BaseDir, "elvis.config")
          end,
        [ fix_dirs(Group, Config)
        || Group <- elvis_config:from_file(ConfigFile)];
      ConfigFile -> elvis_config:from_file(ConfigFile)
    end,

  ct:comment("Elvis rocks!"),
  ok = elvis_core:rock(ElvisConfig),

  {comment, ""}.

base_dir(Config) ->
  case test_server:lookup_config(base_dir, Config) of
    undefined ->
      case test_server:lookup_config(application, Config) of
        undefined ->
          ct:fail("Missing base_dir and application in Config: ~p", [Config]);
        App -> code:lib_dir(App)
      end;
    BaseDir -> BaseDir
  end.

plts(Config) ->
  case test_server:lookup_config(plts, Config) of
    undefined ->
      BaseDir = base_dir(Config),
      Wildcard =
        case is_rebar3_project(Config) of
          true ->
            DefaultRebar3PltLoc = filename:join(BaseDir, "../../../default"),
            filename:join(DefaultRebar3PltLoc, "*_plt");
          false ->
            filename:join(BaseDir, "*.plt")
        end,
      case filelib:wildcard(Wildcard) of
        [] ->
          ct:fail("No plts at ~s - you need to at least have one", [Wildcard]);
        Plts -> Plts
      end;
    Plts -> Plts
  end.

fix_dirs(#{dirs := Dirs} = Group, Config) ->
  NewDirs =
    [filename:join(base_dir(Config), Dir) || Dir <- Dirs],
  Group#{dirs := NewDirs}.

dirs(Config) ->
  BaseDir = base_dir(Config),
  Dirs =
    case test_server:lookup_config(dirs, Config) of
      undefined -> ["ebin", "test"];
      Directories -> Directories
    end,
  [filename:join(BaseDir, Dir) || Dir <- Dirs].

is_rebar3_project(Config) ->
  BaseDir = base_dir(Config),
  BaseDirBin = list_to_binary(BaseDir),
  % rebar3 projects has a `_build' folder for its profiles.
  case re:split(BaseDir, "_build") of
    % If there is just one result, it means `build' wasn't found in the path
    [BaseDirBin] -> false;
    % If there is at least two results, it means `build' was found in the
    % path, so we are dealing with a rebar3 project.
    [_Build, _Found | _] -> true
  end.

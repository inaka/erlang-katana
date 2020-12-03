%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ktn_recipe: a tool to structure code that consists of sequential steps
%%%             in which decisions are made. See README.md for documentation.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(ktn_recipe).
-author('igarai@gmail.com').

-export(
  [ run/2
  , run/4
  , pretty_print/1
  , verify/1
  , normalize/1
  ]).

-type output()                 :: ok | error | halt | term().
-type invalid_result()         :: term().
-type step_fun() :: fun ((term()) -> {output(), term()} | invalid_result()).
-type transition()             :: {step_fun(), output(), output() | step_fun()}.
-type step()                   :: atom() | transition() | step_fun().
-type transitions()            :: [step()].
-type normalized_transitions() :: [transition()].

-export_type([transitions/0]).
-export_type([normalized_transitions/0]).

-callback transitions() -> transitions().
-callback process_result(term()) -> term().
-callback process_error(term()) -> term().

-spec run(atom(), term()) -> term().
run(Mod, InitialState) when is_atom(Mod) ->
  NormalizedTransitions = normalize(Mod),
  InitialFun            = initial_fun(NormalizedTransitions),
  ResultFun             = fun Mod:process_result/1,
  ErrorFun              = fun Mod:process_error/1,
  run(NormalizedTransitions, InitialFun, ResultFun, ErrorFun, InitialState).

-spec run(transitions(), step_fun(), step_fun(), term()) -> term().
run(Transitions, ResultFun, ErrorFun, InitialState) ->
  NormalizedTransitions = normalize(Transitions),
  InitialFun            = initial_fun(NormalizedTransitions),
  run(NormalizedTransitions, InitialFun, ResultFun, ErrorFun, InitialState).

-spec run(normalized_transitions(), output(), step_fun(), step_fun(), term()) ->
  term().
run(_Transitions, error, _ResultFun, ErrorFun, State) ->
  ErrorFun(State);
run(_Transitions, halt, ResultFun, _ErrorFun, State) ->
  ResultFun(State);
run(Transitions, StepFun, ResultFun, ErrorFun, State) ->
  case StepFun(State) of
    {halt, NewState} ->
      ResultFun(NewState);
    {error, NewState} ->
      ErrorFun(NewState);
    {Output, NewState} ->
      NextStep = next_step(StepFun, Output, Transitions),
      run(Transitions, NextStep, ResultFun, ErrorFun, NewState);
    BadReturnValue ->
      Throw =
        [ {value,       BadReturnValue}
        , {step,        StepFun}
        , {transitions, Transitions}
        , {state,       State}
        ],
      throw({bad_step_return_value, Throw})
  end.

-spec initial_fun(normalized_transitions()) -> step_fun().
initial_fun([{InitialFun, _, _} | _]) -> InitialFun.

-spec normalize(module() | transitions()) -> normalized_transitions().
normalize(Mod) when is_atom(Mod) ->
  Transitions = Mod:transitions(),
  [ case Transition of
      Step when is_atom(Step) ->
        {fun Mod:Step/1, ok, next(Mod, Step, Transitions)};
      StepFun when is_function(StepFun, 1) ->
        {StepFun, ok, next(Mod, StepFun, Transitions)};
      {StepFun1, Input, StepFun2} ->
        {normalize_step(Mod, StepFun1), Input, normalize_step(Mod, StepFun2)};
      Step ->
        throw({normalization_error, bad_step, Step})
    end
  || Transition <- Transitions
  ];
normalize(Transitions) when is_list(Transitions) ->
  [ case Transition of
      StepFun when
        is_function(StepFun, 1) ->
          {StepFun, ok, next(StepFun, Transitions)};
      {StepFun1, Input, StepFun2} when
        is_function(StepFun1, 1),
        is_function(StepFun2, 1) ->
          {StepFun1, Input, StepFun2};
      {StepFun1, Input, Action} when
        Action == error; Action == halt ->
          {StepFun1, Input, Action};
      Step ->
        throw({normalization_error, bad_step, Step})
    end
  || Transition <- Transitions
  ].

-spec normalize_step(atom(), halt | error | step()) ->
  halt | error | step_fun().
normalize_step(_Mod, halt) ->
  halt;
normalize_step(_Mod, error) ->
  error;
normalize_step(Mod, Step) when is_atom(Mod), is_atom(Step) ->
  fun Mod:Step/1;
normalize_step(_Mod, StepFun) when is_function(StepFun, 1) ->
  StepFun;
normalize_step(_Mod, Step) ->
  throw({normalization_error, bad_step, Step}).

%% next_step/3 computes the next step from a given step and an input.
-spec next_step(step(), term(), normalized_transitions()) -> error | step().
next_step(_Step, _Input, [])                      -> error;
next_step(Step, Input, [{Step, Input, Next} | _]) -> Next;
next_step(Step, Input, [_ | Ts])                  -> next_step(Step, Input, Ts).

%% next/2-3 computes the implied next state in a transition table for states
%% which do not explicitly name their next state.
-spec next(atom(), step(), transitions()) -> error | halt | step_fun().
next(Mod, Step, Transitions) ->
  case next(Step, Transitions) of
    error                    -> error;
    halt                     -> halt;
    S when is_atom(S)        -> fun Mod:S/1;
    S when is_function(S, 1) -> S
  end.

-spec next(step(), normalized_transitions()) -> error | halt | step().
next(_, [])                          -> error;
next(X, [X])                         -> halt;
next(X, [{Y, _, _} | T]) when X /= Y -> next(X, T);
next(X, [Y | T])         when X /= Y -> next(X, T);
next(X, [{X, _, _} | T])             -> next2(X, T);
next(X, [X | T])                     -> next2(X, T).

next2(_, [])                          -> error;
next2(X, [{X, _, _}])                 -> halt;
next2(X, [X])                         -> halt;
next2(X, [{X, _, _} | T])             -> next2(X, T);
next2(X, [X | T])                     -> next2(X, T);
next2(X, [{Y, _, _} | _]) when X /= Y -> Y;
next2(X, [Y | _])         when X /= Y -> Y.

%% Pretty prints a procedure module's transition table.
-spec pretty_print(atom() | transitions()) -> ok.
pretty_print(Mod) when is_atom(Mod) ->
  pretty_print(normalize(Mod));
pretty_print(Transitions) when is_list(Transitions) ->
  pretty_print_normalized(normalize(Transitions)).

-spec pretty_print_normalized(normalized_transitions()) -> ok.
pretty_print_normalized(NormalizedTransitions) ->
  PrintFun =
    fun
      ({SF1, I, Action}) when Action == error; Action == halt ->
        {module, M1} = erlang:fun_info(SF1, module),
        {name, F1}   = erlang:fun_info(SF1, name),
        io:format("~p:~p(~p) -> ~p~n", [M1, F1, I, Action]);
      ({SF1, I, SF2}) ->
        {module, M1} = erlang:fun_info(SF1, module),
        {module, M2} = erlang:fun_info(SF2, module),
        {name, F1}   = erlang:fun_info(SF1, name),
        {name, F2}   = erlang:fun_info(SF2, name),
        io:format("~p:~p(~p) -> ~p:~p~n", [M1, F1, I, M2, F2])
    end,
  lists:foreach(PrintFun, NormalizedTransitions).

%% Verifies that a procedure module's transition table meets certain minimal
%% criteria. Returns ok if the transition table is a list of either atoms
%% or ternary tuples, whose first and third elements are atoms, and that
%% all states in the transition table are exported from the module with the
%% correct arity.
%% It does not verify structural properties of the FSM defined by the transition
%% table (e.g. connected, acyclic, arboreal).
-spec verify(atom() | transitions()) -> term().
verify(Mod) when is_atom(Mod) ->
  InitialState = #{recipe_type => implicit, recipe => Mod},
  run(ktn_recipe_verify, InitialState);
verify(Transitions) when is_list(Transitions) ->
  InitialState = #{recipe_type => explicit, recipe => Transitions},
  run(ktn_recipe_verify, InitialState).

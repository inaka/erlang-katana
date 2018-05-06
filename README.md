erlang katana
======

![samurai](https://raw.githubusercontent.com/unbalancedparentheses/katana/master/images/samurai.jpg)

Even if you love Erlang as we do, from time to time you might ask yourself why
some functions you normally find in other languages are not part of the erlang's
standard library. When you ask yourself that type of question you should
remember that an estimated 2 million people are currently working in COBOL and
1.5 million new lines of COBOL code are written every day. After feeling bad for
those developers, you should send a pull request to erlang katana with the
functions you use on a daily basis.

To sum up this is a grab bag of useful functions (ideally).
![grabbag](https://raw.githubusercontent.com/unbalancedparentheses/erlang-katana/master/images/bagofcat.jpg)

# Contact Us

If you find any **bugs** or have a **problem** while using this library, please
[open an issue](https://github.com/inaka/erlang-katana/issues/new) in this repo
(or a pull request :)).

And you can check all of our open-source projects at
[inaka.github.io](http://inaka.github.io)

#Objective
- [20 cool Clojure functions](https://daveyarwood.github.io/2014/07/30/20-cool-clojure-functions/)
- [Prismatic's Clojure utility belt](https://github.com/Prismatic/plumbing)

## Related Projects
Check out as well [katana-code](https://github.com/inaka/katana-code) and [katana-test](https://github.com/inaka/katana-test)

## Included goodies:

* `ktn_date`: functions useful for handling dates and time values.
* `ktn_debug`: functions useful for debugging.
* `ktn_fsm`: a _nice_ wrapper on top of `gen_fsm`
* `ktn_json`: functions useful for processing & creating JSON.
* `ktn_maps`: functions useful for handling maps.
* `ktn_numbers`: functions useful for processing numeric values.
* `ktn_recipe`: A tool to structure code that consists of sequential steps in which decisions are made.
* `ktn_rpc`: functions useful for RPC mechanisms.
* `ktn_task`: functions useful for managing asyncronous tasks.
* `ktn_user_default`: useful functions for your erlang shell.

### `ktn_user_default`

This module contains functions that are nice to have in your user default
module, and thereby added to your shell. To use them, copy the ones you want
into your `~/user_default.erl` module.

### `ktn_test_utils`

The Katana Test Utilities includes two functions useful for testing REST APIs:
`test_response/4` and `assert_response/4`.

##### assert_response(Test, MatchType, Params, Response)

`assert_response/4` uses `test_response/4` to check that a given assertion
holds. It's usage is identical to `test_response/4`, except that it fails when
the assertion fails.

##### test_response(Test, MatchType, Params, Response)

`test_response/3` provides some useful checks regarding request responses. The
call to test itself does not fail, `test_response` will return ok if the test
passed and {error, Reason} when it does not.

To have it fail on the same test use `assert_response/4`. `test_response/4` is
intended to be called as:

```erlang
ok = test_response(status, Response, "201"),
{error, {nomatch, "204", _}} = test_response(status, partial, Response, "40?"),
```

The first argument is what part of the response must be tested, and may be one
of: `status`, `headers` or `body`.

The second argument must be a map with at least three keys:
- `status`, the reponse status
- `headers`, the list of headers in the response
- `body`, the response body

The status test matches the response status agains some regex pattern. It has a
short form for specifying return codes: the third argument must be a string,
either representing the exact return code, a string representing a partial
return code (using the ? character as a wildcard, e.g. "20?", "4??"), or a
regular expression as processed by the re module. You cannot specify patterns
with a ? in the second digit position, e.g. "2?1".

If the status test fails, it returns {error, {nomatch, Pattern, Status}} where
Pattern is the provided pattern to match agains and Status is the response
status.

The headers test compares a list of headers agains those present in a response.
The comparison can check whether the set of headers provided matches exactly
(with exact) or if it is a subset of the response headers (with partial). The
headers list of is a list of {Header, Value} tuples. Note that the elements in
both headers lists do not have to be in the same order, nor have the same case.

If the test should fail, the possible error values returned are:
- {missing_headers, Headers, ResHeaders} where Headers is the list
 of headers missing from the set of headers in the response ResHeaders.
- {nomatch, Headers, ResHeaders} if the test type was exact and the two sets
 of headers do not contain the same elements.

The body test's third argument may specify whether the body must match a
provided regex ({partial, Pattern}) or match a given body exactly ({match,
Text}).

Possible error values for the body assert are:
- {regex_compile_fail, Error}} if the regular expression fails to compile.
- {error, {nomatch, Pattern, Body}} if the body does not match the regex.
- {nomatch, ResBody, Body}} if the body does not match the text.


### `ktn_recipe`

#### What is a recipe?

*Recipe* (noun): A set of conditions and parameters of an industrial process
to obtain a given result.

A recipe is a series of steps to obtain a result. This word was chosen
because 'procedure' is overloaded in computer sciences, and it is not
exactly a finite state machine.

ktn_recipe arose from the need to restructure code implementing large
application business logic decision trees.
Long functions, deeply nested case expressions, code with several
responsibilities, all make for unreadable and unmaintainable code.
Every time one needs to make a change or add a feature, one must think about
the non-obvious data flow, complex logic, possible side effects.
Exceptions pile upon exceptions, until the code is a house of cards.
If one is lucky (or responsible), one has comprehensive test suites covering
all cases.

A better way to structure the code would be preferable, one that makes the
flow obvious and separates responsibilities into appropriate code segments.
One way is a finite state machine. OTP even has a behaviour for exactly this
purpose. However, gen_fsm does not exactly fit our needs:
To begin with, gen_fsms run in their own process, which may not be what is
needed. Second, logic flow is not immediately obvious because the fsm's
state depends on the result of each state function. Finally, a gen_fsm
transitions only on receiving input, whereas we are looking for something
that runs like normal code, "on its own". So, our fsm will be defined by
something like a transition table, a single place you can look at and know
how it executes. This specification will "drive" the recipe.

The most common case envisioned is a sequential series of steps, each of
which makes a decision affecting later outcomes, so our design should be
optimized for a single, linear execution flow, allowing for writing the
minimum amount of code for this particular case. Keep the common case fast.

#### How do I use it?

The simplest use case: create a module using the ktn_recipe behaviour.
It must export `transitions/0`, `process_result/1`, `process_error/1` and a
series of unary functions called step functions.
`transitions/0` must return a list of atoms naming the step functions, in the
order in which they must be called. Each step function takes a State
variable as input and if the step was succesful emits `{ok, NewState}` with
the updated state, or `{error, NewState}` if it was unsuccessful.
After running all the steps, process_result will take the resulting state
as input and should emit whatever you need.
If one of the state functions returns `{error, NewState}`, then
`process_error/1` will be called taking the resulting state as input and
should handle all expected error conditions.

To run the recipe, call `ktn_recipe:run(CALLBACK_MODULE, INITIAL_STATE)`.

For more advanced uses, continue reading.

#### How does it work?

The main functions are `ktn_recipe:run/2-4`. These will run a recipe and
return the result.

Recipes may be specified in two ways, which we call "implicit" and
"explicit", for lack of better words. In implicit mode, we pass `run/2` a
callback module implementing the ktn_recipe behavior, which implements all
necessary functions. In explicit mode, we explicitly give `run/4` the
transition table, a function to process the state resulting from running all
recipe steps to completion, a function to process erroneous states, and the
initial state of the recipe.

#### Step functions

Step functions have the following type:

```erlang
-type state()  :: term().
-type output() :: term().
-spec Step(state()) -> {ok, state()}
                    | {output(), state()}
                    | {error, state()}
                    | {halt, state()}.
```

The recipe state may be any value you need, a list, a proplist, a record, a
map, etc. ktn_recipe does not use maps itself, although the test suite does.

The initial state passed to `run/2-4` will be passed to the first step as-is,
and the subsequent resulting states will be fed to each step.

The step functions should, if possible, have no side effects. If so, and the
recipe ends in an error state, it can be aborted without worrying about
rolling back the side effects.

For example, if the recipe involves making changes in a database, these
should be made in the `process_result/1` function, once it is certain that
all steps completed successfully.

#### Result and Error processing

In implicit mode, the module must export `process_error/1` and
`process_result/1`; in explicit mode you may pass whichever function you
desire.
The purpose of the 'result processing function' is to transform the
resulting state into a usable value.
The purpose of the 'error processing function' is to extract the error value
from the state and do something according. If you are unfortunate enough
to have unavoidable side effects in your step functions, you may undo them
here.

#### Specifying transitions

Transition tables are lists.
As stated, the simplest transition table is a list of atoms naming the step
functions, but this is not the only way recipe states may be specified.
A transition table is a list of transitions.
What is permissible as a transition depends on whether we are running in
implicit or explicit mode.

In explicit mode, a transition may be either be an external function, i.e.
`fun module:function/arity`, or a ternary tuple `{F1, I, F2}` in which the _F1_
and _F2_ elements are explicit functions and the _I_ is the transition input.
This form reprensents a transition from step _F1_ to step _F2_, if _F1_ outputs
`{Input, State}` instead of `{ok, State}`.

In implicit mode, in addition to the se two forms of specifying step
functions, an atom may be used. The module in which the function resides is
assumed to be the provided callback module, hence it is 'implicit'.

Also as stated, in any mode, the return value of step functions must be one
of:
- `{ok, State}`
- `{Output, State}`
- `{halt, State}`
- `{error, State}`

If `{error, State}` is returned, `run/2-4` will call the error processing
function. If {halt, State} is called, `run/2-4` will call the result
processing function.
If `{ok, State}` is returned, run will call the next function in the
transition table. That is, it will search the transition table until it
finds the current's functions position, if necessary skip over entries
corresponding to the current function, and call the first function with a
different name it encounters.
If `{Output, State}` is returned, run will search the transition table for an
entry matching `{Step, Output, NextStep}`, and select NextStep as the function
to call. In this way, non-linear recipes that jump ahead or loop back may
be specified. It is recommended that the transition table describe a DAG,
unless the program logic really requires loops.

Before running a recipe, `run/2-4` will 'normalize' the transition table to
remove implicit assumptions and so that it is composed only of ternary
tuples explicitly listing every transition. As a debugging aid, this
function `normalize/1` is exported by the `ktn_recipe` module.

#### Example

Suppose you have a web server with an endpoint `/conversations`, accepting the
`DELETE` method to delete conversations between users. To delete a
conversation, one must fetch the converation entity from the database, fetch
the contact (the other user in the conversation) specified by the user id
in the conversation's contact_id attribute, check the contact's status (if
it is blocked, etc) and also check whether the client is using version 1 of
our REST API or version 2, since one really deletes the conversation from
the database and the other merely clears messages from the conversation.

We could implement this as a recipe with four steps:

```erlang
transitions() ->
 [ get_conversation
 , get_contact
 , get_status
 , check_api_version
 ].
```

get_conversation fetches it from the database. If it has already been
deleted, it could return `{error, NewState}` and store the error in the state.
Likewise for `get_contact`, and `get_status`.
`get_api_version` would extract the api version from the request header or
url, and store the result in the recipe state. If all steps complete
successfully, `process_result` will effectively make the call to delete the
conversation from the database.

Using ktn_recipes, you could structure your application as:

```
+----------+   +-------------+   +---------+   +-----------+
|          |   |             |   |         |   |           |
| Endpoint +---> Recipes for +---> Entity  +---> Databases |
| handlers |   | endpoint    |   | modules |   |           |
|          |   | actions     |   |         |   |           |
|          |   |             |   |         |   |           |
+----------+   +-------------+   +---------+   +-----------+
```

The handlers would have the sole responsibility of handling the request,
i. e. parsing URL, header and body parameters, verfying them and invoking
the correct recipe.
Recipes would implement the business logic.
Entity modules would abstract management of your systems entities, including
issues such as caching, and eventually persisting the changes to the
storage medium or obtaining the required information.

#### What if something goes wrong?

The function `ktn_recipe:verify/1` takes either a recipe module or an explicit
transition table and will run several checks to verify that it will run
correctly. You may use this function to test your transition tables.
Note that verify/1 is implemented as a `ktn_recipe`, so you can use it as an
example.

If a step throws an exception, it will not be caught. That means you may see
some of ktn_recipe's internal functions in the stacktrace. In general, the
most common errors will be:
1) badly formed transition tables
2) not exporting step functions
3) bad return values from step functions
4) bad state values set by functions

1) and 2) should be detected by running verify on your recipe. 3) and 4)
will be detected at run time and an appropriate error will be returned by
`run/2-4`. Of course, your tests should detect these cases. You do have
tests, right?

Finally, there is one more function, `ktn_recipe:pretty_print/1`, which takes
either a callback module or a transition table, and prints the normalized
transition table.

%% @doc Helper module to implement waiting conditions.
%%
%% It periodically queries and validates values from a callback
%% until the condition is met or a timeout passes.
%%
%% Common method when testing for eventual consistency.
-module(wait_helper).

-export([wait_until/2, wait_until/3]).

-export_type([name/0, sleep_time/0, callback/1, validator/1, opts/1, return/1]).

-type name() :: atom().
%% Condition name, defaults to `timeout'.

-type wait_error(Expected) :: {name(), Expected, history(), undefined | term()}.
%% An error type that can be returned or thrown when the condition eventually fails.
%%
%% The last element of the tuple is the return value of the `on_error' callback given to `t:opts/1',
%% if given, otherwise `undefined'.

-type return(Expected) ::
    {ok, Expected} | {error, wait_error(Expected)} | no_return().

-type sleep_time() :: non_neg_integer().
%% How long to wait between queries for new results.

-type history() :: [term()].
%% History of results.
%%
%% Note that this history will be simplified, that is, if elements repeat consecutively,
%% they will be agregated to the history only once together with a count.

-type callback(Expected) :: fun(() -> Expected | term()).
%% A function that returns a value to verify.
%%
%% By default, the return value will be checked for equality against the expected value,
%% but a custom `t:validator/1' function can be specified.
%%
%% The return value of this callback is what will be returned by the whole `wait_until' operation.

-type validator(Expected) :: fun((Expected | term()) -> boolean()).
%% Validation to do against the callbacks return value.
%%
%% This is useful when an intermediate value of the callback is desired in the return value,
%% but the condition to be checked requires further computation that is not needed outside
%% of the scope of the condition.
%%
%% For example:
%% ```
%% Validator = fun(ReturnValue) -> contains(ReturnValue, ...) end,
%% {ok, Result} = mongoose_helper:wait_until(
%%             fun() -> do_get_value(...) end,
%%             expected,
%%             #{validator => Validator}),
%% '''
%% where `Result' is the return value of the original callback,
%% after we know it is accepted by the validator

-type opts(Expected) :: #{
    validator => validator(Expected),
    expected_value => Expected,
    time_left => timeout(),
    sleep_time => sleep_time(),
    on_error => fun(() -> term()),
    no_throw => boolean(),
    name => name()
}.

%% From mongoose_helper

%% @see wait_until/3
%% @doc Waits for `Fun' to return `ExpectedValue'.
-spec wait_until(callback(Expected), Expected) ->
    return(Expected).
wait_until(Fun, ExpectedValue) ->
    wait_until(Fun, ExpectedValue, #{}).

%% @doc Waits for `Fun' to return a value `validator' accepts.
%%
%% <ul>
%%   <li>If the result of `Fun' equals `ExpectedValue', returns `{ok, ExpectedValue}'</li>
%%   <li>If no value is returned or the result doesn't equal `ExpectedValue':</li>
%%      <ul>
%%          <li>If `no_throw => true' is given, it returns `{error, Error}',
%%              where `Error' is of type `t:wait_error/1'.</li>
%%          <li>If throws the following error: `t:wait_error/1'.</li>
%%      </ul>
%% </ul>
%%
%% Example:
%% ```
%% Opts = #{no_throw => true, time_left => timer:seconds(2)},
%% {error, _} = wait_until(fun() -> ... end, SomeVal, Opts)
%% '''
-spec wait_until(callback(Expected), Expected, opts(Expected)) ->
    return(Expected).
wait_until(Fun, ExpectedValue, Opts) ->
    Defaults = #{
        callback => Fun,
        validator => fun(NewValue) -> ExpectedValue =:= NewValue end,
        on_error => fun() -> undefined end,
        no_throw => false,
        expected_value => ExpectedValue,
        time_left => timer:seconds(5),
        sleep_time => 100,
        history => [],
        name => timeout
    },
    do_wait_until(maps:merge(Defaults, Opts)).

do_wait_until(
    #{
        expected_value := ExpectedValue,
        time_left := TimeLeft,
        history := History,
        on_error := OnError,
        no_throw := NoThrow,
        name := Name
    }
) when TimeLeft =< 0 ->
    Return = {Name, ExpectedValue, lists:reverse(History), OnError()},
    case NoThrow of
        true ->
            {error, Return};
        false ->
            error(Return)
    end;
do_wait_until(#{callback := Fun, validator := Validator} = Opts) ->
    try Fun() of
        FunResult ->
            case Validator(FunResult) of
                true -> {ok, FunResult};
                _ -> wait_and_continue(Opts, FunResult)
            end
    catch
        Error:Reason:Stacktrace ->
            wait_and_continue(Opts, {Error, Reason, Stacktrace})
    end.

wait_and_continue(
    #{
        time_left := TimeLeft,
        sleep_time := SleepTime,
        history := History
    } = Opts, FunResult
) ->
    NewHistory = case History of
        [{times, Times, FunResult} | T] ->
            [{times, Times + 1, FunResult} | T];
        _ ->
            [{times, 1, FunResult} | History]
    end,
    timer:sleep(SleepTime),
    do_wait_until(Opts#{
        time_left => TimeLeft - SleepTime,
        history => NewHistory
    }).

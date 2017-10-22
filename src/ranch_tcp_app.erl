-module(ranch_tcp_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1, profile_output/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    start(compiler),
    start(syntax_tools),
    start(goldrush),
    start(lager),
    erlang:apply(lager, start, []),
    start(ranch_tcp).

start(App) ->
    start_ok(App, application:start(App, permanent)).
start_ok(_App, ok) ->
    ok;
start_ok(_App, {error, {already_started, _App}}) ->
    ok;
start_ok(App, {error, {not_started, Dep}}) ->
    ok = start(Dep),
    start(App);
start_ok(App, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).

start(_StartType, _StartArgs) ->
    _ = consider_profiling(),
    ranch_tcp_sup:start_link().

stop(_State) ->
    ok.


consider_profiling() ->
    case config:get_ranch_tcp_env(profile) of
        true ->
            {ok, _Pid} = eprof:start(),
            eprof:start_profiling([self()]);
        _ ->
            not_profiling
    end.

-spec profile_output() -> ok.
profile_output() ->
    eprof:stop_profiling(),
    eprof:log("procs.profile"),
    eprof:analyze(procs),
    eprof:log("total.profile"),
    eprof:analyze(total).

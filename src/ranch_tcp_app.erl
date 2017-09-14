-module(ranch_tcp_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1, profile_output/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(lager),
    lager:start(),
    application:start(ranch_tcp).

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

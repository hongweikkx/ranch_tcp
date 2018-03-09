%%%-------------------------------------------------------------------
%%% @author hongweigaokkx@163.com
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 三月 2018 15:55
%%%-------------------------------------------------------------------
-module(ranch_tcp).
-define(CHILD(I, Type, Args), {I, {I, start_link, [Args]}, permanent, brutal_kill, Type, [I]}).

%% API
-export([listen/1]).

listen(Opts) when is_list(Opts) ->
    {ok, _Pid1} = supervisor:start_child(ranch_tcp_sup, ?CHILD(tcp_client_sup, supervisor, Opts)),
    {ok, _Pid2} = supervisor:start_child(tcp_listener_sup, ?CHILD(tcp_listener, worker, Opts)),
    ok.






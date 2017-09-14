%%%-------------------------------------------------------------------
%%% @author hongweigaokkx@163.com
%%% @doc
%%%    配置 相关函数
%%% @end
%%% Created : 12. 九月 2017 15:15
%%%-------------------------------------------------------------------
-module(config).

%% API
-export([
    get_ranch_tcp_env/1,
    get_ranch_tcp_env/2
]).

get_ranch_tcp_env(Key) ->
    case application:get_env(ranch_tcp, Key) of
        {ok, Val} ->
            Val;
        _E ->
            _E
    end.

get_ranch_tcp_env(Key, Default) ->
    case application:get_env(ranch_tcp, Key) of
        {ok, Val} ->
            Val;
        _ ->
            Default
    end.



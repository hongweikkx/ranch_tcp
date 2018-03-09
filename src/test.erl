%%%-------------------------------------------------------------------
%%% @author gaohongwei
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 九月 2017 15:55
%%%-------------------------------------------------------------------
-module(test).

%% API
-export([test_echo/1, test_echo_util/1, test_run_1/0, test_run_2/0]).

test_run_1() ->
    ranch_tcp:listen([]).
test_run_2() ->
    ranch_tcp:listen([{host, "127.0.0.1"}, {port, 1234}, {acceptor_num, 10},
        {max_connections, 10000}, {client_shutdown_timeout, 10}]).

test_echo(Num) ->
    [spawn(test, test_echo_util, [X]) || X <- lists:seq(1, Num)].
test_echo_util(_X) ->
    HostInNet = "localhost",
    case gen_tcp:connect(HostInNet, 1234, [binary, {packet, 2}]) of
        {ok, Sock} ->
            ok = gen_tcp:send(Sock, "hello, world!"),
            timer:sleep(5000),
            ok = gen_tcp:close(Sock);
        _ ->
            skip
    end.

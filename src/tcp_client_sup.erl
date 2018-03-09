%%%-------------------------------------------------------------------
%%% @author hongweigaokkx@163.com
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%      client sup
%%%      select some of code in supervisor.erl otp-19, the code implements init/1 as
%%%      "{simple_one_for_one, 0, 1},
%%%       [
%%%          {tcp_client, {tcp_client, start_link, []}, temporary, TimeOut, worker, [tcp_client]}
%%%       ]",
%%%       其中 TimeOut 可以通过 tcpOpts - client_shutdown_timeout 参数配置
%%% @end
%%% Created : 20. 二月 2017 下午10:40
%%%-------------------------------------------------------------------
-module(tcp_client_sup).
-behavior(gen_server).
%% API
-export([
    start_link/1,
    start_client/1,
    set_client_num_limit/1
]).
%% Supervisor callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").

-define(SERVER, ?MODULE).
-define(SETS, sets).
-define(DICT, dict).

-record(child, {
    mfargs         :: {M::atom(), F::atom(), A::list()},
    shutdown       :: brutal_kill | infinity | integer()
}).

-record(state, {
    child                      :: #child{},
    child_num          = 0     :: integer(),
    child_num_limit    = 0     :: integer(),
    dynamics                   %% undefined | ?SETS(pid())
}).


%%%===================================================================
%%% API functions
%%%===================================================================

start_link(TcpOpts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, TcpOpts, []).

start_client(Sock) ->
    gen_server:call(?SERVER, {start_child, Sock}).

set_client_num_limit(NLimit) ->
    gen_server:cast(?SERVER, {set_client_num_limit, NLimit}).

handle_call(Info, _From, State) ->
    try
        do_call(Info, _From, State)
    catch
        _:Reason ->
            lager:error("module:~p do_call info:~p wrong, the reason is:~p~n", [?MODULE, Info, Reason]),
            {reply, error, State}
    end.

handle_cast(CastMsg, State) ->
    try
        do_cast(CastMsg, State)
    catch
        _:Reason  ->
            lager:error("module:~p, do_cast info:~p wrong, the reason is:~p~n", [?MODULE, CastMsg, Reason]),
            {reply, error, State}
    end.

handle_info(Info, State) ->
    try
        do_info(Info, State)
    catch
        _:Reason  ->
            lager:error("module:~p, do_info info:~p wrong, the reason is:~p~n", [?MODULE, Info, Reason]),
            {reply, error, State}
    end.

terminate(_Reason, #state{child =Child } = State)  ->
    terminate_dynamic_children(Child, dynamics_db(State#state.dynamics)).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
init(TcpOpts) ->
    process_flag(trap_exit, true),
    ClientShutTimeout = proplists:get_value(client_shutdown_timeout, TcpOpts, ?DEFAULT_CLIENT_SHUTDOWN_TIMEOUT),
    ChildNumLimit = proplists:get_value(max_connections, TcpOpts, ?DEFAULT_MAX_CONNECTION),
    Child = #child{mfargs = {tcp_client, start_link, []}, shutdown = ClientShutTimeout},
    {ok, #state{child = Child, child_num = 0, child_num_limit = ChildNumLimit}}.


do_call({start_child, Socket}, _From, State)  ->
    case is_max_connections_limit(State) of
        true ->
            {reply, {error, max_limit}, State};
        false ->
            Child = State#state.child,
            #child{mfargs = {M, F, Args}} = Child,
            case do_start_child_i(M, F, [Socket |Args]) of
                {ok, Pid} ->
                    NState = save_dynamic_child(Pid, State),
                    {reply, {ok, Pid}, NState};
                {ok, Pid, Extra} ->
                    NState = save_dynamic_child(Pid, State),
                    {reply, {ok, Pid, Extra}, NState};
                {error, Error} ->
                    {reply, {error, Error}, State}
            end
    end;
do_call(Info, _From, State) ->
    lager:error("module:~p, do_call info:~p undefined~n", [?MODULE, Info]),
    {reply, ok, State}.


do_cast({set_client_num_limit, NLimit}, State) ->
    {noreply, State#state{child_num_limit = max(0, NLimit)}};

do_cast(CastMsg, State) ->
    lager:error("module:~p, do_cast cast_msg:~p is undefined~n", [?MODULE, CastMsg]),
    {noreply, State}.

do_info({'EXIT', Pid, Reason}, State) ->
    NState = child_exit(Pid, Reason, State),
    {noreply, NState};
do_info(Info, State) ->
    lager:error("module:~p, do_info info:~p is undefined~n", [?MODULE, Info]),
    {noreply, State}.

%% return: {ok, Pid} | {ok, Pid, Extra} | {error, Error}
do_start_child_i(M, F, A) ->
    case catch apply(M, F, A) of
        {ok, Pid} when is_pid(Pid) ->
            {ok, Pid};
        {ok, Pid, Extra} when is_pid(Pid) ->
            {ok, Pid, Extra};
        ignore ->
            {error, ignore};
        {error, Error} ->
            {error, Error};
        What ->
            {error, What}
    end.

save_dynamic_child(Pid, #state{dynamics = Dynamics, child_num = OldNum} = State) ->
    DynamicsDb = dynamics_db(Dynamics),
    State#state{dynamics = ?SETS:add_element(Pid, DynamicsDb), child_num = OldNum + 1}.

dynamics_db(undefined) ->
    ?SETS:new();
dynamics_db(DynamicsDb) ->
    DynamicsDb.


child_exit(Pid, Reason, State) ->
    child_exit_error_report(Pid, Reason),
    #state{dynamics = OldDynamics, child_num = OldNum} = State,
    State#state{dynamics = ?SETS:del_element(Pid, OldDynamics), child_num = max(0, OldNum - 1)}.

%% Shutdown all dynamic children. This happens when the supervisor is
%% stopped. Because the supervisor can have millions of dynamic children, we
%% can have an significative overhead here.
terminate_dynamic_children(Child, Dynamics) ->
    {Pids, EStack0} = monitor_dynamic_children(Dynamics),
    Sz = ?SETS:size(Pids),
    EStack =
    case Child#child.shutdown of
        brutal_kill ->
            ?SETS:fold(fun(P, _) -> exit(P, kill) end, ok, Pids),
            wait_dynamic_children(Child, Pids, Sz, undefined, EStack0);
        infinity ->
            ?SETS:fold(fun(P, _) -> exit(P, shutdown) end, ok, Pids),
            wait_dynamic_children(Child, Pids, Sz, undefined, EStack0);
        Time ->
            ?SETS:fold(fun(P, _) -> exit(P, shutdown) end, ok, Pids),
            TRef = erlang:start_timer(Time, self(), kill),
            wait_dynamic_children(Child, Pids, Sz, TRef, EStack0)
    end,
    FError =
        fun(Reason, Pid, _) ->
            report_error(shutdown_error, Reason, Pid)
        end,
    ?DICT:fold(FError, ok, EStack).

monitor_dynamic_children(Dynamics) ->
    F =
        fun(P, {Pids, EStack}) ->
            case monitor_child(P) of
                ok ->
                    {?SETS:add_element(P, Pids), EStack};
                {error, normal} ->
                    {Pids, EStack};
                {error, Reason} ->
                    {Pids, ?DICT:append(Reason, P, EStack)}
            end
        end,
    ?SETS:fold(F, {?SETS:new(), ?DICT:new()}, Dynamics).

%% Help function to shutdown/2 switches from link to monitor approach
monitor_child(Pid) ->
    %% Do the monitor operation first so that if the child dies
    %% before the monitoring is done causing a 'DOWN'-message with
    %% reason noproc, we will get the real reason in the 'EXIT'-message
    %% unless a naughty child has already done unlink...
    erlang:monitor(process, Pid),
    unlink(Pid),

    receive
    %% If the child dies before the unlik we must empty
    %% the mail-box of the 'EXIT'-message and the 'DOWN'-message.
        {'EXIT', Pid, Reason} ->
            receive
                {'DOWN', _, process, Pid, _} ->
                    {error, Reason}
            end
    after 0 ->
        %% If a naughty child did unlink and the child dies before

        %% monitor the result will be that shutdown/2 receives a
        %% 'DOWN'-message with reason noproc.
        %% If the child should die after the unlink there
        %% will be a 'DOWN'-message with a correct reason
        %% that will be handled in shutdown/2.
        ok
    end.


wait_dynamic_children(_Child, _Pids, 0, undefined, EStack) ->
    EStack;
wait_dynamic_children(_Child, _Pids, 0, TRef, EStack) ->
    %% If the timer has expired before its cancellation, we must empty the
    %% mail-box of the 'timeout'-message.
    _ = erlang:cancel_timer(TRef),
    receive
        {timeout, TRef, kill} ->
            EStack
    after 0 ->
        EStack
    end;
wait_dynamic_children(#child{shutdown=brutal_kill} = Child, Pids, Sz, TRef, EStack) ->
    receive
        {'DOWN', _MRef, process, Pid, killed} ->
            wait_dynamic_children(Child, ?SETS:del_element(Pid, Pids), Sz-1,
                TRef, EStack);

        {'DOWN', _MRef, process, Pid, Reason} ->
            wait_dynamic_children(Child, ?SETS:del_element(Pid, Pids), Sz-1,
                TRef, ?DICT:append(Reason, Pid, EStack))
    end;
wait_dynamic_children(Child, Pids, Sz, TRef, EStack) ->
    receive
        {'DOWN', _MRef, process, Pid, shutdown} ->
            wait_dynamic_children(Child, ?SETS:del_element(Pid, Pids), Sz-1,
                TRef, EStack);

        {'DOWN', _MRef, process, Pid, normal} ->
            wait_dynamic_children(Child, ?SETS:del_element(Pid, Pids), Sz-1,
                TRef, EStack);

        {'DOWN', _MRef, process, Pid, Reason} ->
            wait_dynamic_children(Child, ?SETS:del_element(Pid, Pids), Sz-1,
                TRef, ?DICT:append(Reason, Pid, EStack));

        {timeout, TRef, kill} ->
            ?SETS:fold(fun(P, _) -> exit(P, kill) end, ok, Pids),
            wait_dynamic_children(Child, Pids, Sz, undefined, EStack)
    end.


is_max_connections_limit(State)  ->
    State#state.child_num + 1 > State#state.child_num_limit.


report_error(Error, Reason, Child) ->
    ErrorMsg = [{supervisor, ?MODULE}, {errorContext, Error}, {reason, Reason}, {child, Child}],
    lager:error("module:~p report_error:~p~n", [ErrorMsg]).


child_exit_error_report(_Pid, normal) ->
    ok;
child_exit_error_report(_Pid, shutdown) ->
    ok;
child_exit_error_report(Pid, Reason) ->
    report_error(client_exit, Reason, Pid).

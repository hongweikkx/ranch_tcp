%%%-------------------------------------------------------------------
%%% @author hongweigaokkx@163.com
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%      接收进程
%%% @end
%%% Created : 11. 三月 2017 下午8:24
%%%-------------------------------------------------------------------
-module(tcp_client).
-behaviour(gen_server).


%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================

-record(state, {recv_sock, ref}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Sock]) ->
    process_flag(trap_exit, true),
    inet_db:register_socket(Sock, inet_tcp),
    State = #state{recv_sock = Sock},
    {ok, State, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Info, _From, State) ->
    try
        do_call(_Info, _From, State)
    catch
        _:Reason  ->
            lager:error("module:~p do_call info:~p wrong, the reason is:~p~n", [?MODULE, _Info, Reason]),
            {reply, error, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(Info, State) ->
    try
        do_cast(Info, State)
    catch
        _:Reason  ->
            lager:error("module:~p do_cast info:~p wrong, the reason is:~p~n", [?MODULE, Info, Reason]),
            {noreply, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    try
        do_info(_Info, State)
    catch
        _:Reason  ->
            lager:error("moudle:~p do_info info:~p wrong, the reason is:~p~n", [?MODULE, _Info, Reason]),
            {noreply, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, State) ->
    gen_tcp:close(State#state.recv_sock),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_call(_Info, _From, State) ->
    lager:error("#### module:~p do_call msg:~p is undefined.~n", [?MODULE, _Info]),
    {reply, error, State}.

do_cast(_Info, State) ->
    lager:error("#### module:~p do_cast msg:~p is undefined.~n", [?MODULE, _Info]),
    {noreply, State}.

do_info(timeout, State) ->
    recv_packet(State);

do_info({inet_async, S, Ref, {ok, Packet}}, State)  when State#state.ref =:= Ref ->
    case gen_tcp:send(S, Packet) of
        ok ->
            recv_packet(State);
        {error, Reason} ->
            {stop, Reason, State}
    end;

do_info({inet_async, _S, _Ref, {error, closed}}, State) ->
    {stop, normal, State};


do_info({'EXIT', _S, Reason}, State) ->
    {stop, Reason, State};

do_info(_Info, State) ->
    lager:error("####module:~p do_info msg:~p is undefined.~n", [?MODULE, _Info]),
    {noreply, State}.

recv_packet(State) ->
    Sock = State#state.recv_sock,
    case prim_inet:async_recv(Sock, 0, -1) of
        {ok, Ref} ->
            NState = State#state{ref = Ref},
            {noreply, NState};
        _Error ->
            {stop, normal, State}
    end.



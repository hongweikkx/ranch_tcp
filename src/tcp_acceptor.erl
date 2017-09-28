%%%-------------------------------------------------------------------
%%% @author hongweigaokkx@163.com
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 二月 2017 下午10:58
%%%-------------------------------------------------------------------
-module(tcp_acceptor).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {acceptor_name, lsock, ref}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(AcceptorNum::integer(), LSock::term()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(AcceptorNum, LSock) ->
    gen_server:start_link(?MODULE, [AcceptorNum, LSock], []).

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
init([AcceptorNum, LSock]) ->
    AccepterName = create_acceptor_worker_name(AcceptorNum),
    register(AccepterName, self()),
    process_flag(trap_exit, true),
    case prim_inet:async_accept(LSock, -1) of
        {ok, _Ref} ->
            {ok, #state{lsock = LSock, acceptor_name = AccepterName}};
        Error ->
            lager:info("create acceptor worker:~p error, the error is :~p~n", [AccepterName, Error]),
            {stop, error}
    end.

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
handle_call(Info, _From, State) ->
    try
        do_call(Info, _From, State)
    catch
        _:Reason  ->
            lager:error("module:~p do_call info:~p wrong, the reason is:~p~n", [?MODULE, Info, Reason]),
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
            lager:error("module:~p, do_cast info:~p wrong, the reason is:~p~n", [?MODULE, Info, Reason]),
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
            lager:error("module:~p do_info info:~p wrong, the reason is:~p~n", [?MODULE, _Info, Reason]),
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
terminate(_Reason, _State) ->
    lager:info("#####################Reason:~p, State:~p~n", [_Reason, _State]),
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
create_acceptor_worker_name(AcceptorNum) ->
    list_to_atom(lists:concat(["acceptor_worker_", AcceptorNum])).

do_call(Info, _From, State) ->
    lager:error("module:~p do_call msg:~p is undefined.~n", [?MODULE, Info]),
    {reply, error, State}.

do_cast(Info, State) ->
    lager:error("module:~p do_cast msg:~p is undefined.~n", [?MODULE, Info]),
    {noreply, State}.

do_info({inet_async, L, _Ref, {ok, S}}, State) when L =:= State#state.lsock  ->
    case start_client(S) of
        ok ->
            case prim_inet:async_accept(L, -1) of
                {ok, _NRef} ->
                    {noreply, State};
                Error ->
                    lager:error("create acceptor worker:~p error, the error is :~p~n", [State#state.acceptor_name, Error]),
                    {stop, error, State}
            end;
        {error, max_limit} ->
            {noreply, State};
        error ->
            {stop, error, State}
    end;

do_info({inet_async, _L, _Ref, Error}, State) ->
    lager:error("#### do_info accept error:~p~n", [{Error, State#state.acceptor_name}]),
    {stop, error, State};
do_info(_Info, State) ->
    lager:error("#### do_info msg:~p is undefined.~n", [_Info]),
    {noreply, State}.

start_client(S) ->
    case tcp_client_sup:start_client(S) of
        {ok, Pid} when is_pid(Pid) ->
            case gen_tcp:controlling_process(S, Pid) of
                {error, Reason} ->
                    lager:error("#########gen_tcp controllint process is error, the reason is :~p~n", [Reason]),
                    {error, Reason};
                ok ->
                    ok
            end;
        {error, max_limit} ->
            lager:info("##########tcp_client start child error:~p~n", [max_limit]),
            {error, max_limit};
        {error, Error} ->
            lager:error("###### tcp_client_sup start child error, The Error is:~p~n", [Error]),
            {error, Error}
    end.


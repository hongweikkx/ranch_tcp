%%%-------------------------------------------------------------------
%%% @author hongweigaokkx@163.com
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%      监听进程
%%%           todo 1. 暴露tcp_opts 的接口给对方。 并且将tcp_listener 后置启动
%%% @end
%%% Created : 20. 二月 2017 下午10:52
%%%-------------------------------------------------------------------
-module(tcp_listener).
-behaviour(gen_server).
-include("common.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(TCP_OPT,[{ip, any}, inet, {backlog, 5}, {active, false}, {packet, 2}, {keepalive, true}, binary]).
-include("common.hrl").

-record(state, {opts = []}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

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
init(Args) ->
    {ok, #state{opts = Args}, 0}.


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
            lager:error("module:~p, do_call info:~p wrong, the reason is:~p~n", [?MODULE, _Info, Reason]),
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
            lager:error("module:~p do_call info:~p wrong, the reason is:~p~n", [?MODULE, Info, Reason]),
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
            lager:error("module:~p do_info:~p wrong, the reason is:~p~n", [?MODULE, _Info, {Reason,
             erlang:get_stacktrace()}]),
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
    lager:error("####module:~p do_call msg:~p is undefined.~n", [?MODULE, _Info]),
    {reply, error, State}.

do_cast(_Info, State) ->
    lager:error("####module:~p do_cast msg:~p is undefined.~n", [?MODULE, _Info]),
    {noreply, State}.

do_info(timeout, State) ->
    Opts = State#state.opts,
    TcpOpt = tcp_opt(Opts),
    case gen_tcp:listen(0, TcpOpt) of
        {ok, LSock} ->
            AccepterSum = proplists:get_value(acceptor_num, Opts, ?DEFAULT_ACCEPTOR_NUM),
            [tcp_acceptor_sup:start_child(AcceperNum, LSock) || AcceperNum <- lists:seq(1, AccepterSum)],
            {noreply, State};
        _Error ->
            {stop, error, State}
    end;

do_info(_Info, State) ->
    lager:error("####module:~p do_info msg:~p is undefined.~n", [?MODULE, _Info]),
    {noreply, State}.

<<<<<<< Updated upstream
tcp_opt() ->
    Ip = config:get_ranch_tcp_env(host, "127.0.0.1"),
    case config:get_ranch_tcp_env(port, 12345) of
        Port when is_integer(Port) ->
            [{ip, Ip}, {port, Port} | ?TCP_OPT];
        _ ->
            [{ip, Ip}, ?TCP_OPT]
    end.
=======
tcp_opt(Opts) ->
    Ip = proplists:get_value(host, Opts, ?DEFAULT_HOST),
    Port = proplists:get_value(port, Opts, ?DEFAULT_PORT),
    [{ip, Ip}, {port, Port} | ?TCP_OPT].



>>>>>>> Stashed changes

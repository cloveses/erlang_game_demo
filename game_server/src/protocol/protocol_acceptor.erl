-module(protocol_acceptor).

-behaviour(gen_server).

%% ====================================================================
%% Include files  
%% ====================================================================
-define(IP, "127.0.0.1").
-define(PROT, 9999).
-define(ACCEPTORNUMS, 99).
-define(MAX_CLIENTS, 10000000).

-define(TCP_OPTIONS, [
        %%{ip, {0,0,0,0,0,0,0,1}},
        binary,
        {packet, raw},
        %{buffer, 1024},
        {reuseaddr, true},
        {backlog, 1024},
        {nodelay, false}]).

-define(WAIT_FOR_AUTH, wait_for_auth).
-define(WAIT_FOR_DATA, wait_for_data).
%% ====================================================================
%% API functions
%% ====================================================================
%% gen_server callbacks 
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0, start_link/1]).

-record(state, {state, conn, peername, protocol, player_pid}).

start() ->
    [ok = application:start(App) || App <- [sasl, gen_logger, esockd]],
    {ok, Params} = application:get_env(game_server, protocol),
    IP = proplists:get_value(ip, Params, ?IP),
    Port = proplists:get_value(port, Params, ?PROT),
    Access = application:get_env(esockd, access, [{allow, all}]),
    SockOpts = [{access, Access},
                {acceptors, proplists:get_value(acceptornums, Params, ?ACCEPTORNUMS)},
                {shutdown, infinity},
                {max_clients, proplists:get_value(max_clients, Params, ?MAX_CLIENTS)},
                {sockopts, ?TCP_OPTIONS}],
    MFArgs = {?MODULE, start_link, []},
    esockd:open(?MODULE, {IP, Port}, SockOpts, MFArgs).

start_link(Conn) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [Conn])}.

init(Conn) ->
    {ok, Conn1} = Conn:wait(),
    {_, Peername} = Conn:peername(),
    Conn1:setopts([{active, once}]),
    gen_server:enter_loop(?MODULE, [], #state{state = ?WAIT_FOR_AUTH, conn = Conn1, peername = Peername}).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({send, Bin}, State = #state{protocol = websocket, conn = Conn}) ->
    Conn:send(protocol_websocket:packet(Bin)),
    {noreply, State};

handle_cast({send, Bin}, State = #state{conn = Conn}) when is_binary(Bin) ->
    Conn:send(Bin),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, _Sock, Data}, State = #state{state = ?WAIT_FOR_AUTH, protocol = websocket, conn = Conn}) ->
    [ParseData] = protocol_websocket:parse_data(Data),
    auth(State, ParseData, Conn);

handle_info({tcp, _Sock, Data}, State = #state{state = ?WAIT_FOR_AUTH, conn = Conn}) ->
    case protocol_websocket:is_websocket(Data) of
        true ->
            case protocol_websocket:make_handshake(Data) of
                {ok, Response} ->
                    Conn:send(Response),
                    NewState = State#state{protocol = websocket},
                    Conn:setopts([{active, once}]),
                    {noreply, NewState};
                _ ->
                    {stop, normal, State}
            end;
        _ ->
            auth(State, Data, Conn)
    end;

handle_info({tcp, _Sock, Data}, State = #state{state = ?WAIT_FOR_DATA, protocol = websocket, 
                                               player_pid = PlayerPid}) ->
    [gen_server:cast(PlayerPid, {socket_event, protocol:parse(EachData)}) 
      || EachData <- protocol_websocket:parse_data(Data)],
    {noreply, State};

handle_info({tcp, _Sock, Data}, State = #state{state = ?WAIT_FOR_DATA, player_pid = PlayerPid}) ->
    gen_server:cast(PlayerPid, {socket_event, protocol:parse(Data)}),
    {noreply, State};

handle_info({tcp_error, Sock, Reason}, State) ->
    io:format("Error from: ~p~n", [Sock]),
    io:format("tcp_error: ~s~n", [Reason]),
    {stop, {shutdown, {tcp_error, Reason}}, State};

handle_info({tcp_closed, _Sock}, State = #state{player_pid = PlayerPid}) ->
    io:format("tcp_closed~n"),
    gen_server:cast(PlayerPid, {stop}),
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%% ====================================================================
%% Internal functions
%% ====================================================================
auth(State, Data, Conn) ->
    case player_account:auth_and_start(Data, self()) of
        {ok, PlayerPid} ->
            Auth = once,
            NewState = State#state{state = ?WAIT_FOR_DATA, player_pid = PlayerPid};
        _ ->
            Auth = false,
            NewState = State
    end,
    case Auth of
        false ->
            {stop, normal, NewState};
        _ ->
            Conn:setopts([{active, true}]),
            {noreply, NewState}
    end.
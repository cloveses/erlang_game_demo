-module(player).

-behaviour(gen_server).

%% ====================================================================
%% Include files  
%% ====================================================================
-include("common.hrl").

-define(DIC_PLAYER, dic_player).
%% ====================================================================
%% API functions
%% ====================================================================
%% gen_server callbacks 
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/1, get_player/0, update/1, send/2, send_to_all/1]).

-record(state, {}).

start_link(Args) ->
    [Account|_] = Args,
    ProcessName = list_to_atom(lists:append(["player_", transform_tool:to_list(Account)])),
    gen_server:start_link({global, ProcessName}, ?MODULE, Args, []).

init(Args) ->
    [Account, SendPid] = Args,
    load_player(Account, SendPid),
    {ok, #state{}}.

handle_call({apply_call, Mod, Fun, Args}, _From, State) ->
    Reply  = 
        case apply(Mod, Fun, Args) of
             {'EXIT', _Info} ->  
                 {false,error};
             DataRet -> 
                 DataRet
        end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Msg, State) ->
    player_cast:do_cast(Msg, State).

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_player() ->
    get(?DIC_PLAYER).

update(Player) ->
    put(?DIC_PLAYER, Player).

send(SendPid, Bin) ->
    gen_server:cast(SendPid, {send, Bin}).

send_to_all(Bin) ->
    [send(Player#player.send_pid, Bin) || Player <- player_sup:get_online_player()].

%% ====================================================================
%% Internal functions
%% ====================================================================

load_player(Account, SendPid) ->
    MapId = 1,
    MapPid = map:get_pid(MapId),
    Player = #player{account = Account, pid = self(), send_pid = SendPid,
                     map_pid = MapPid},
    MapObj = #map_obj{id = Account, type = ?MAP_OBJ_TYPE_PLAYER, pid = self()},
    gen_server:cast(MapPid, {enter_map, MapObj}),
    put(?DIC_PLAYER, Player).
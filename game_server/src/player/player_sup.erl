
-module(player_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_player/2]).

-export([online/1, offline/1, get_online_player/0, get_online_player/1]).

%% Supervisor callbacks
-export([init/1]).

-include("common.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(ONLINE_PLAYER, online_player).
%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_player(ProcessName, Args) ->
    {ok, Pid} = supervisor:start_child(?MODULE, worker_spec(ProcessName, player, start_link, [Args])),
    Pid.
%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ets:new(?ONLINE_PLAYER, [{keypos, #player.account}, named_table, public, set]),
    {ok, { {one_for_all, 0, 1}, []} }.

online(Player) ->
    ets:insert(?ONLINE_PLAYER, Player).

offline(Player) ->
    ets:delete(?ONLINE_PLAYER, Player#player.account).

get_online_player() ->
    ets:tab2list(?ONLINE_PLAYER).

get_online_player(Account) ->
    MS = ets:fun2ms(fun(T) when T#player.account == Account -> T end),
    ets:select(?ONLINE_PLAYER, MS).
%%====================================================================
%% Internal functions
%%====================================================================
worker_spec(ProcessName, M, F, A) ->
    {ProcessName, {M, F, A}, temporary, 10000, worker, [M]}.
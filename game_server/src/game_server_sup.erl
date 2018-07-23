%%%-------------------------------------------------------------------
%% @doc game_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(game_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 0, 1}, [supervisor_spec(player_sup, start_link, []), supervisor_spec(map_sup, start_link, [])]} }.

%%====================================================================
%% Internal functions
%%====================================================================
supervisor_spec(M, F, A) ->
    {M, {M, F, A}, permanent, 10000, supervisor, [M]}.

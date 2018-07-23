
-module(map_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_map/2]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("stdlib/include/ms_transform.hrl").

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_map(ProcessName, Args) ->
    {ok, Pid} = supervisor:start_child(?MODULE, worker_spec(ProcessName, map, start_link, [Args])),
    Pid.
%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
worker_spec(ProcessName, M, F, A) ->
    {ProcessName, {M, F, A}, temporary, 10000, worker, [M]}.
-module(map).

-behaviour(gen_server).

%% ====================================================================
%% Include files  
%% ====================================================================
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
%% gen_server callbacks 
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([get_pid/1, start_link/1]).

-record(state, {map_id, obj_list = []}).

get_processname(MapId) ->
    list_to_atom(lists:append(["map_", transform_tool:to_list(MapId)])).

get_pid(MapId) ->
    ProcessName = get_processname(MapId),
    case whereis(ProcessName) of
        undefined ->
            map_sup:start_map(ProcessName, [MapId]);
        MapPid ->
            MapPid
    end.

start_link([MapId]) ->
    ProcessName = get_processname(MapId),
    gen_server:start_link({local, ProcessName}, ?MODULE, [[MapId]], []).

init([MapId]) ->
    {ok, #state{map_id = MapId}}.

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

handle_cast({enter_map, Obj}, State) ->
    ObjList = State#state.obj_list,
    X = common_tool:rand(1, 100),
    Y = common_tool:rand(1, 100),
    NewObjList = lists:keystore(Obj#map_obj.id, #map_obj.id, ObjList, Obj#map_obj{x = X, y = Y}),
    {noreply, State#state{obj_list = NewObjList}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
    
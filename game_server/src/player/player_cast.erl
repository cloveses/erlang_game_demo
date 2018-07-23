-module(player_cast).

%% ====================================================================
%% Include files  
%% ====================================================================
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([do_cast/2]).

do_cast({socket_event, {ok, Params}}, State) ->
    Player = player:get_player(),
    case protocol_handler:handle(Player, Params) of
        {update, NewPlayer} ->
            player:update(NewPlayer);
        _ ->
            skip
    end,
    {noreply, State};

do_cast({stop}, State) ->
    Player = player:get_player(),
    player_sup:offline(Player),
    {stop, normal, State};

do_cast(Msg, State) ->
    io:format("not format player_cast: ~p~n", [Msg]),
    {noreply, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================



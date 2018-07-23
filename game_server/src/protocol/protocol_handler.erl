-module(protocol_handler).

%% ====================================================================
%% Include files  
%% ====================================================================
-include("common.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([handle/2]).

handle(Player, {?PROTO_PLAYER_ACCOUNT, [Account]}) ->
    Bin = list_to_binary(transform_tool:to_list(Account)),
    player:send_to_all(Bin),
    Player;
handle(Player, Msg) ->
    io:format("protocol_handler not format : ~p~n", [Msg]),
    Player.


%% ====================================================================
%% Internal functions
%% ====================================================================



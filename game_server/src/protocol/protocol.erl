-module(protocol).

%% ====================================================================
%% Include files  
%% ====================================================================
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([parse/1]).

parse(Bin) when is_binary(Bin)->
    try 
        ProtoMap = jsx:decode(Bin, [return_maps]),
        #{<<"protocol">> := Protocol} = ProtoMap,
        parse(Protocol, ProtoMap)
    catch 
        _:Reason ->
            io:format("parse proto decode error ~p~nReason: ~p~n", [Bin, Reason]),
            []
    end.

parse(?PROTO_PLAYER_ACCOUNT, #{<<"account">> := Account}) ->
    io:format("Account: ~p~n", [Account]),
    {ok, {?PROTO_PLAYER_ACCOUNT, [transform_tool:to_atom(Account)]}};
parse(Protocol, ProtoMap) ->
    io:format("parse proto ~p error ~p~n", [Protocol, ProtoMap]),
    [].


%% ====================================================================
%% Internal functions
%% ====================================================================
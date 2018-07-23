-module(protocol_header).

%% ====================================================================
%% Include files  
%% ====================================================================


%% ====================================================================
%% API functions
%% ====================================================================
-export([request_header/1,
         get_value/2]).

request_header(ReqData) ->
    request_header(ReqData, []).
request_header(<<>>, Headers) ->
    Headers;
request_header(ReqData, Headers) ->
    case erlang:decode_packet(httph, ReqData, []) of
        {ok, {http_header, _, H, _, V}, Rest} ->
            request_header(Rest, [{string:to_lower(transform_tool:to_list(H)), V} | Headers]);
        {ok, _, Rest} ->
            request_header(Rest, Headers);
        _ ->
            Headers
    end.

get_value(Headers, H) ->
    case lists:keyfind(H, 1, Headers) of
        {_, V} -> V;
        _ -> []
    end.
%% ====================================================================
%% Internal functions
%% ====================================================================


    
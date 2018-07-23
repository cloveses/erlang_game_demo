-module(player_account).

%% ====================================================================
%% Include files  
%% ====================================================================
-include("common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([auth_and_start/2]).

auth_and_start(ReqData, SendPid) when is_binary(ReqData) ->
    case protocol:parse(ReqData) of
        {ok, {_, [Account]}} ->
            case player_sup:get_online_player(Account) of
                [] ->  
                    ProcessName = list_to_atom(lists:append(["player_", transform_tool:to_list(Account)])),
                    Pid = player_sup:start_player(ProcessName, [Account, SendPid]),
                    Player = #player{account = Account, pid = Pid, send_pid = SendPid},
                    player_sup:online(Player),
                    {ok, Pid};
                _ ->
                    []
            end;
        _ ->
            []
    end;
auth_and_start(_ReqData, _) ->
    [].

%% ====================================================================
%% Internal functions
%% ====================================================================



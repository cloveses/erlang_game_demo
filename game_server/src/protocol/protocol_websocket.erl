-module(protocol_websocket).

%% ====================================================================
%% Include files  
%% ====================================================================


%% ====================================================================
%% API functions
%% ====================================================================
-export([is_websocket/1,
         make_handshake/1,
         parse_data/1,
         packet/1]).

is_websocket(ReqData) ->
    case protocol_header:get_value(protocol_header:request_header(ReqData), "upgrade") of
        [] ->
            false;
        Value -> 
            string:to_lower(Value) == "websocket"
    end.

make_handshake(ReqData) ->
    Headers = protocol_header:request_header(ReqData),
    case protocol_header:get_value(Headers, "sec-websocket-key") of
        [] ->
            false;
        SecKey ->
            BinKey = list_to_binary(SecKey),
            Bin = <<BinKey/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>,
            Challenge = base64:encode(crypto:hash(sha, Bin)),
            Response = [<<"HTTP/1.1 101 Switching Protocols\r\n">>,
                        <<"Connection: Upgrade\r\n">>,
                        <<"Upgrade: websocket\r\n">>,
                        <<"Sec-Websocket-Accept: ">>, Challenge, <<"\r\n\r\n">>],
            {ok, Response}
    end.

parse_data(Data) ->
    RealDatas = parse_frame(Data, []),
    [RealData || {_Opcode, RealData} <- RealDatas].

packet(Payload) ->
    <<1:1, 0:3, 1:4, (payload_length(iolist_size(Payload)))/binary, Payload/binary>>.


%% ====================================================================
%% Internal functions
%% ====================================================================
parse_frame(<<>>, RealDatas) ->
    RealDatas;
parse_frame(<<_Fin:1, _Rsv:3, Opcode:4, _Mask:1, PayloadLen:7, 
              MaskKey:4/binary, PayloadData:PayloadLen/binary-unit:8, 
              Rest/binary>>, RealDatas) when PayloadLen < 126 ->
    RealData = unmask(PayloadData, MaskKey, <<>>),
    parse_frame(Rest, [{Opcode, RealData} | RealDatas]);
parse_frame(<<_Fin:1, _Rsv:3, Opcode:4, _Mask:1, 126:7, PayloadLen:16,
              MaskKey:4/binary, PayloadData:PayloadLen/binary-unit:8,
              Rest/binary>>, RealDatas) ->
    RealData = unmask(PayloadData, MaskKey, <<>>),
    parse_frame(Rest, [{Opcode, RealData} | RealDatas]);
parse_frame(<<_Fin:1, _Rsv:3, Opcode:4, _Mask:1, 127:7, PayloadLen:64,
              MaskKey:4/binary, PayloadData:PayloadLen/binary-unit:8,
              Rest/binary>>, RealDatas) ->
    RealData = unmask(PayloadData, MaskKey, <<>>),
    parse_frame(Rest, [{Opcode, RealData} | RealDatas]).

unmask(<<O:32, Rest/binary>>, MaskKey, Acc) ->
    <<MaskKey2:32>> = MaskKey,
    unmask(Rest, MaskKey, <<Acc/binary, (O bxor MaskKey2):32>>);
unmask(<<O:24>>, MaskKey, Acc) ->
    <<MaskKey2:24, _:8>> = MaskKey,
    <<Acc/binary, (O bxor MaskKey2):24>>;
unmask(<<O:16>>, MaskKey, Acc) ->
    <<MaskKey2:16, _:16>> = MaskKey,
    <<Acc/binary, (O bxor MaskKey2):16>>;
unmask(<<O:8>>, MaskKey, Acc) ->
    <<MaskKey2:8, _:24>> = MaskKey,
    <<Acc/binary, (O bxor MaskKey2):8>>;
unmask(<<>>, _MaskKey, Acc) ->
    Acc.

payload_length(N) ->
    case N of
        N when N =< 125 -> << N >>;
        N when N =< 16#ffff -> << 126, N:16 >>;
        N when N =< 16#7fffffffffffffff -> << 127, N:64 >>
    end.
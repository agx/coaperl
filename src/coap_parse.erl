-module(coap_parse).
-export([parse/1,
         get_option_values/2]).

-include("coaperl.hrl").


-spec parse(<<>>) -> #coap_request{} | #coap_response{} | #coap_empty_message{}.
% @doc: parse a binary coap message
parse(Packet) ->
    {coap, Parsed} = parse_raw(Packet),
    parse_to_rec(Parsed).


% @doc get the named option values from the given response record
get_option_values(#coap_response{options=Options}, OptNum) ->
    [ Val || {coap_option, Num, Val} <- Options, Num =:= OptNum ].


% @doc: convert raw message to request, response or empty message records
parse_to_rec({_Type, Code, MID, _Token, Options, Payload}) when Code > 0, Code =< 31 ->
    #coap_request{method=coap_unparse:method_name(Code),
                  path=parse_path(Options),
                  query=parse_query(Options),
                  mid=MID,
                  ct=parse_content_type(Options),
                  payload=Payload};
parse_to_rec({_Type, 0, MID , _Token, _Options, <<>>}) ->
    #coap_empty_message{mid=MID};
parse_to_rec({_Type, Code, MID, _Token, Options, Payload}) ->
    <<Class:3, Detail:5>> = <<Code>>,
    #coap_response{mid=MID,
                   ct=parse_content_type(Options),
                   options=Options,
                   class=Class,
                   detail=Detail,
                   block2=parse_block2(Options),
                   payload=Payload}.


% @doc: parse path from raw options
parse_path(Options) ->
    erlang:iolist_to_binary(lists:reverse(parse_path(Options, []))).

parse_path([{coap_option, ?COAP_OPTION_URI_PATH, Comp}|T], Acc) ->
    parse_path(T, [["/"|Comp]|Acc]);
parse_path([{coap_option, _, _}|T], Acc) ->
    parse_path(T, Acc);
parse_path([], Acc) ->
    Acc.


% @doc: parse query from raw options
parse_query(Options) ->
    parse_query(Options, []).

parse_query([{coap_option, ?COAP_OPTION_URI_QUERY, Opt}|T], Acc) ->
    parse_query(T, [Opt|Acc]);
parse_query([{coap_option, _, _}|T], Acc) ->
    parse_query(T, Acc);
parse_query([], Acc) ->
    Acc.



% @doc: parse content type from raw options
parse_content_type(Options) ->
    OptVals = [Value || {coap_option, Format, Value} <- Options, Format =:= ?COAP_OPTION_CONTENT_FORMAT],
    case OptVals of
        [<<Val:8>>] -> corerl:content_format_type(Val);
        _ -> undefined
    end.


% @doc: parse block2 option from raw options
parse_block2(Options) ->
    OptVals = [Value || {coap_option, Format, Value} <- Options, Format =:= ?COAP_OPTION_BLOCK2],
    case OptVals of
        [Val] -> 
            NumLen = bit_size(Val)-4,
            <<Num:NumLen, M:1, SZX:3>> = Val,
            {Num, M, 1 bsl (SZX+4)};
        _ -> undefined
    end.


% @doc: parse raw packet
parse_raw(<<Version:2, Type:2, TKL: 4, Code:8, MID:16, Token:TKL/bytes, Tail/bytes>>) when Version =:= 1 ->
    {Options, Payload} = parse_raw_options(Tail),
    {coap, {Type, Code, MID, Token, Options, Payload}};
parse_raw(_AnythingElse) ->
    {bad, "unable to parse packet"}.

% @doc: parse options in raw packet, don't interpret any option types yet
parse_raw_options(OptionBin) ->
    parse_raw_options(OptionBin, 0, []).

parse_raw_options(<<>>, _LastNum, OptionList) ->
    {OptionList, <<>>};
parse_raw_options(<<16#FF, Payload/bytes>>, _LastNum, OptionList) ->
    {OptionList, Payload};
parse_raw_options(<<BaseOptNum:4, BaseOptLen:4, Tail/bytes>>, LastNum, OptionList) ->
    {Tail1, OptNum} = case BaseOptNum of
        X when X < 13 ->
            {Tail, BaseOptNum + LastNum};
        X when X =:= 13 ->
            <<ExtOptNum, NewTail/bytes>> = Tail,
            {NewTail, ExtOptNum + 13 + LastNum};
        X when X =:= 14 ->
            <<ExtOptNum:2/bytes, NewTail/bytes>> = Tail,
            {NewTail, ExtOptNum + 269 + LastNum}
    end,
    {OptLen, Tail3} = case BaseOptLen of
        Y when Y < 13 ->
                {BaseOptLen, Tail1};
        Y when Y =:= 13 ->
                <<ExtOptLen, Tail2/bytes>> = Tail1,
                {ExtOptLen + 13, Tail2};
        Y when Y =:= 14 ->
                <<ExtOptLen:2/bytes, Tail2/bytes>> = Tail1,
                {ExtOptLen + 269, Tail2}
    end,
    <<OptVal:OptLen/bytes, NextOpt/bytes>> = Tail3,
    parse_raw_options(NextOpt, OptNum, [{coap_option, OptNum, OptVal}|OptionList]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_to_rec_test() ->
    % Check if parse to the right rec
    ?assertEqual(parse_to_rec({egal, 100, 1, egal, [{coap_option, ?COAP_OPTION_CONTENT_FORMAT, <<50:8>>}], payload}),
                 {coap_response,3,4,1,
                  <<"application/json">>,
                  undefined,
                  [{coap_option,12,<<"2">>}],
                  payload}).
-endif.

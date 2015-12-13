-module(corerl).
-export([parse/1,
        content_format_number/1,
        content_format_type/1]).

-include("corerl.hrl").


content_formats() ->
    [{"text/plain;charset=utf-8",   0},
     % shortcut for the above
     {"text/plain",                 0},
     {"application/link-format",   40},
     {"application/xml",           41},
     {"application/octext-stream", 42},
     {"application/exi",           47},
     {"application/json",          50}].

content_format_number(ContentFormat) ->
    proplists:get_value(ContentFormat, content_formats()).

content_format_type(Number) ->
    list_to_binary(proplists:get_value(Number, 
                       [{N,F} || {F,N} <- content_formats()]
                      )).


-spec parse(<<>>) -> list(#core_link{}).
% @doc: parse a core message
parse(Data) ->
    parse_link_values(binary:split(Data, <<",">>, [global]), []).


parse_link_values([LinkValue|T], Acc) ->
    Link = parse_link_value(LinkValue),
    parse_link_values(T, [Link|Acc]);
parse_link_values([], Acc) ->
    Acc.

parse_link_value(<<$<, Data/binary>>) ->
    [Value, Params] = binary:split(Data, <<$>>>),
    parse_link_params(binary:split(Params, <<$;>>, [global]), #core_link{uri=Value}).


parse_link_params([LinkParam|T], Rec) ->
    NewRec = case binary:split(LinkParam, <<$=>>, [global]) of
        [<<"title">>, V] ->
            Rec#core_link{title=V};
        [<<"ct">>, V] ->
            Rec#core_link{ct=content_format_type(binary_to_integer(V))};
        [<<"rt">>, V] ->
            Rec#core_link{rt=V};
        [<<"if">>, V] ->
            Rec#core_link{'if'=V};
        _ -> Rec
    end,
    parse_link_params(T, NewRec);
parse_link_params([], Rec) ->
    Rec.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_link_values_test() ->
    Data = <<"</>;title=\"General Info\";ct=0,</time>;if=\"clock\";rt=\"Ticks\";title=\"Internal Clock\";ct=0;obs,</async>;ct=50">>,
    ?assertEqual(parse(Data),
                 [{core_link,<<"/async">>,undefined,undefined,undefined,<<"application/json">>,undefined},
                  {core_link,<<"/time">>,<<"\"Internal Clock\"">>,undefined,<<"\"Ticks\"">>, <<"text/plain;charset=utf-8">>,<<"\"clock\"">>},
                  {core_link,<<"/">>,<<"\"General Info\"">>,undefined,undefined,<<"text/plain;charset=utf-8">>,undefined}
                 ]).
-endif.

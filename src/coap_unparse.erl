-module(coap_unparse).
-export([build_request/2,
         build_response/3,
         build_text_response/4,
         method_name/1]).

-include("coaperl.hrl").

coap_scheme_defaults() ->
    [{coap,       5683},
     {coaps,      5684},
     {'coap+tcp', 5683},
     {'coaps+tcp', 443}
    ].


% RFC7252, Section 3
types() ->
    [{confirmable,     0},
     {nonconfirmable,  1},
     {acknowledgement, 2},
     {reset,           3}].

type_code(Type) ->
    proplists:get_value(Type, types()).


% RFC7252, Section 3
%% code() ->
%%     [{request,     0},
%%      {success,     2},
%%      {clienterror, 4},
%%      {servererror, 5}].



% RFC7252, Section 12.1.1
methods() ->
    [{get,    1},
     {post,   2},
     {put,    3},
     {delete, 4}
    ].
method_code(Method) ->
    proplists:get_value(Method, methods()).
method_name(Number) ->
    {Method, Number} = lists:keyfind(Number, 2, methods()),
    Method.


-spec uri_to_options(string()) -> [{string(), integer(), list()}].
% convert an URI to the corresponding COAP Options
uri_to_options(Uri) ->
    {ok, {_, _, Host, Port, Path, _}} = http_uri:parse(Uri,
                                                       [{scheme_defaults, coap_scheme_defaults()},
                                                        {ipv6_host_with_brackets, true}]),

    % Remove brackets from addr if present
    Host2 = case Host of
                [$[|T] -> [X || X <- T, X =/= $]];
                _ -> Host
    end,
    {Host2, Port, path_to_options(Path)}.


% convert an URI path to the corresponding COAP Options
path_to_options([$/|Path]) ->
    lists:reverse(path_to_options(string:tokens(Path, "/"), [])).

path_to_options([], Options) ->
    Options;
path_to_options([H|T], Options) ->
    path_to_options(T, [{?COAP_OPTION_URI_PATH, H}|Options]).


-spec content_format_to_options(string()) -> [{integer(), integer()}].
% @doc: convert a content format into a number (12.3)
content_format_to_options(ContentFormat) ->
    FormatNum = corerl:content_format_number(ContentFormat),
    [{?COAP_OPTION_CONTENT_FORMAT, FormatNum}].


-spec num_bytes(integer()) -> integer().
num_bytes(Val) when is_integer(Val), Val =< 16#ff ->
    1;
num_bytes(Val) when is_integer(Val), Val =< 16#ffff ->
    2;
num_bytes(Val) when is_integer(Val), Val =< 16#ffffff ->
    3.


-spec option_length(integer() | binary() | list()) -> integer().
option_length(Val) when is_integer(Val) ->
    num_bytes(Val);
option_length(Val) when is_binary(Val) ->
    byte_size(Val);
option_length(Val) when is_list(Val) ->
    string:len(Val).


-spec option_format_int(integer()) -> {integer(), integer(), integer()}.
option_format_int(Val) ->
    case Val of
        X when X < 13 ->
            {X, 0, 0};
        X when 13 =< X, X < 269 ->
            {13, X-13, 8};
        X when X >= 14 ->
            {14, X-269, 16}
    end.


-spec options_to_bin(list()) -> iolist().
options_to_bin(Options) ->
    % FIXME: sort options by number here
    options_to_bin(Options, 0, []).

options_to_bin([], _, Acc) ->
    lists:reverse(Acc);
options_to_bin([{Num, Val}|T], OldNum, Acc) when is_integer(Num)->
    Diff = Num - OldNum,
    if  Diff < 0 -> erlang:error(badarith);
        true -> true
    end,
    Len = option_length(Val),

    {Delta, ExtDelta, ExtDeltaBits} = option_format_int(Diff),
    {Length, ExtLength, ExtLengthBits} = option_format_int(Len),

    NewBin = [[<<Delta:4, Length:4,
                 ExtDelta:ExtDeltaBits,
                 ExtLength:ExtLengthBits>>,
                 Val] | Acc],
    options_to_bin(T, Num, NewBin).


build_request([{url, Url}, {method, Method}], Msgid, Options) ->
    {Host, Port, UriOptions} = uri_to_options(Url),
    BinOptions = options_to_bin(UriOptions ++ Options),
    MethodNumber = method_code(Method),
    Type = type_code(confirmable),
    {Host, Port, [<<1:2, Type:2, 0:4, 0:3, MethodNumber:5, Msgid:16>>, BinOptions]}.

build_request(Url, Msgid) ->
    build_request([{url, Url}, {method, get}], Msgid, []).


build_response(Class, Detail, Msgid) ->
    Type = type_code(confirmable),
    [<<1:2, Type:2, 0:4, Class:3, Detail:5, Msgid:16>>].

build_text_response(Class, Detail, Msgid, Payload) ->
    Options = content_format_to_options("text/plain"),
    BinOptions = options_to_bin(Options),
    Type = type_code(confirmable),
    [<<1:2, Type:2, 0:4, Class:3, Detail:5, Msgid:16>>, BinOptions, ?COAP_PAYLOAD_MARKER, Payload].


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

option_delta_test() ->
    % Check if we encode the option delta correctly
    ?assertEqual(options_to_bin([{11, "foo"}]),
                 [[<<11:4, 3:4>>, "foo"]]),
    ?assertEqual(options_to_bin([{15, "foo"}]),
                 [[<<13:4, 3:4, 2:8>>, "foo"]]),
    ?assertEqual(options_to_bin([{275, "foo"}]),
                 [[<<14:4, 3:4, 6:16>>, "foo"]]).

option_length_test() ->
    % Check if we encode the option value length correctly
    ?assertEqual(options_to_bin([{11, "12345678901234"}]),
                 [[<<11:4, 13:4, 1:8>>, "12345678901234"]]),
    % integer represented in one byte
    ?assertEqual(options_to_bin([{11, 24}]),
                 [[<<11:4, 1:4>>, 24]]),
    % integer represented in two bytes
    ?assertEqual(options_to_bin([{11, 324}]),
                 [[<<11:4, 2:4>>, 324]]),
    % integer represented in three bytes
    ?assertEqual(options_to_bin([{11, 70024}]),
                 [[<<11:4, 3:4>>, 70024]]).

path_to_options_test() ->
    Path = "/.well-known/core",
    Ret = path_to_options(Path),
    ?assertEqual(Ret, [{11,".well-known"},{11,"core"}]).

uri_to_options_test() ->
    Uri = "coap://[::1]/.well-known/core",
    Ret = uri_to_options(Uri),
    ?assertEqual(Ret, {"::1", 5683, [{11,".well-known"},{11,"core"}]}).

-endif.

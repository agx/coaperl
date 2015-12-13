-module(coap_parse_test).
-include_lib("eunit/include/eunit.hrl").

-include("coaperl.hrl").

% Test descriptions
parse_test_() ->
    [{"Option extration works",
      check_get_option_values()}
    ].

% Tests
check_get_option_values() ->
    Ret = coap_parse:get_option_values(#coap_response{options=[{coap_option,11,<<"bar">>},
                                                               {coap_option,11,<<"baz">>},
                                                               {coap_option,12,<<"foo">>}]},
                                       11),
    [?_assertEqual(Ret, [<<"bar">>, <<"baz">>])].


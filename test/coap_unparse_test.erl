-module(coap_unparse_test).
-include_lib("eunit/include/eunit.hrl").

% Test descriptions
unparse_test_() ->
    [{"Building a get request should result in binary data",
      check_build_request()},
     {"Building a response without payload in binary data",
      check_build_response()},
     {"Building a response with payload in binary data",
      check_build_text_response()}
    ].

% Tests
check_build_request() ->
    Uri = "coap://[::1]/.well-known/core",
    Ret = coap_unparse:build_request(Uri, 1),
    [?_assertEqual(Ret, {"::1", 5683,
                         [<<64,1,0,1>>,
                         [[<<11:4,11:4>>,".well-known"],
                          [<<0:4,4:4>>,"core"]]]})].

check_build_response() ->
    Ret = coap_unparse:build_response(4, 4, 1),
    [?_assertEqual(Ret, [<<64,4:3, 4:5,0,1>>])].

check_build_text_response() ->
    Ret = coap_unparse:build_text_response(2, 5, 1, "payload"),
    [?_assertEqual(Ret, [<<64,2:3, 5:5,0,1>>,
                         [[<<12:4,1:4>>,0]],
                         255, "payload"])].

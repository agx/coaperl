Some erlang helpers for COAP messages (RFC7252)

Copyright 2015 Guido Günther <agx@sigxcpu.org>

This is by no means a full coap implementation. It merely allows to ease
parsing of messages a bit by putting things into a record.

Let's assume you receive UDP datagrams in a gen_server. You can then match on
the incoming coap message like:

    handle_info({udp, Socket, Host, Port, Bin}, State) ->
        Msg = coap_parse:parse(Bin),
        case Msg of
            #coap_request{method=post, path= <<"/switches">>, query=[<<"name=", Name/binary>>], payload=Payload} ->
            ... 

Which would allow you to process a request like:
       
    echo '{"mode": "on"}' | coap-client -f- -t application/json -m post "coap://[::1]/switches?name=Lever1"

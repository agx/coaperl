
% RFC7252, Section 12.2
%  0 (Reserved)
%  1 if-match
%  3 uri-host
%  4 eTag
%  5 if-none-match
%  7 uri-port
%  8 location-path
-define(COAP_OPTION_URI_PATH, 11).
-define(COAP_OPTION_CONTENT_FORMAT, 12).
% 14 max-age
-define(COAP_OPTION_URI_QUERY, 15).
% 17 accept
% 20 location-query
% 35 proxy-uri
% 39 proxy-scheme
% 60 size1
%128 (Reserved)
%132 (Reserved)
%136 (Reserved)
%140 (Reserved)

% draft-ietf-core-block-17
-define(COAP_OPTION_BLOCK2, 23).
-define(COAP_OPTION_BLOCK1, 27).
-define(COAP_OPTION_SIZE2, 28).

-define(COAP_PAYLOAD_MARKER, 16#FF).

-record(coap_request, {
          method,
          path,
          query,
          mid,
          ct,
          payload
         }).

-record(coap_response, {
          class,      % response class
          detail,     % repsonse detail
          mid,        % the meassage id
          ct,         % the content type
          block2,     % block2 option
          options,    % all coap options
          payload}).
% 2.01 Location-Path, Location-Query
% 2.03 ETag, Max-Age
% 4.13 Size1

-record(coap_empty_message, {mid}).

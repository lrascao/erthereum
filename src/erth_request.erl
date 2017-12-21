%%
 % Miniclip SA, 2017. All rights reserved.
 %
-module(erth_request).
-author("dinis.rosario").

%% API
-export([request/2]).

-spec request(MethodData :: binary() | {binary(), integer()},
              MethodArgs :: list()) -> {ok, any()} | {error, any()}.
request(MethodName, MethodArgs) when is_binary(MethodName) ->
    request({MethodName, undefined}, MethodArgs);
request({MethodName, MethodId}, MethodArgs) when is_binary(MethodName) ->
    Req = jsonrpc2_client:create_request({MethodName, MethodArgs, MethodId}),
    Encoded = binary_to_list(jiffy:encode(Req)),
    Host = application:get_env(erthereum, host, "localhost"),
    Port = application:get_env(erthereum, port, 8545),
    {ok, ConnPid} = gun:open(Host, Port),
    {ok, _} = gun:await_up(ConnPid),
    StreamRef = gun:post(ConnPid, "/",
                         [{<<"content-type">>, <<"application/json">>}],
                         Encoded),
    case gun:await(ConnPid, StreamRef) of
        {response, fin, _, _} ->
            {error, failed};
        {response, nofin, _, _} ->
            {ok, Response} = gun:await_body(ConnPid, StreamRef),
            case jiffy:decode(Response) of
                {Decoded} ->
                    process_decoded_body(Decoded);
                _ ->
                    {error, decoding_failed}
            end
    end.

-spec process_decoded_body(DecodedBody :: list()) -> {ok, any()} | {error, any()}.
process_decoded_body(DecodedBody) ->
    case kvlist:get(<<"result">>, DecodedBody) of
        undefined ->
            Error = kvlist:get(<<"error">>, DecodedBody),
            {error, Error};
        Result ->
            {ok, Result}
    end.
-module(erthereum).

%% API exports
%% net namespace
-export([net_version/0]).
%% eth namespace
-export([eth_blockNumber/0,
         eth_getBalance/1, eth_getBalance/2]).

%% Type definitions.
-type error() :: failed | decoding_failed.
-type address() :: binary().
-type block_tag() :: latest | earliest | pending.

%% Type exports
-export_type([error/0]).

%% Defines
-define(METHOD_NAME_NET_VERSION, <<"net_version">>).
-define(METHOD_ID_NET_VERSION, 1).
-define(METHOD_NAME_ETH_BLOCKNUMBER, <<"eth_blockNumber">>).
-define(METHOD_ID_ETH_BLOCKNUMBER, 83).
-define(METHOD_NAME_ETH_GET_BALANCE, <<"eth_getBalance">>).
-define(METHOD_ID_ETH_GET_BALANCE, 1).

%%====================================================================
%% API functions
%%====================================================================

%% Returns the current network protocol version.
-spec net_version() -> {ok, non_neg_integer()} | {error, error()}.
net_version() ->
    maybe_int(request(?METHOD_NAME_NET_VERSION, ?METHOD_ID_NET_VERSION)).

%% Returns the number of most recent block.
-spec eth_blockNumber() -> {ok, non_neg_integer()} | {error, error()}.
eth_blockNumber() ->
    maybe_int(request(?METHOD_NAME_ETH_BLOCKNUMBER, ?METHOD_ID_ETH_BLOCKNUMBER)).

%% Returns the balance of the account of given address.
-spec eth_getBalance(Address :: address()) -> {ok, non_neg_integer()} | {error, error()}.
eth_getBalance(Address) ->
    maybe_int(eth_getBalance(Address, latest)).

%% Returns the balance of the account of given address.
-spec eth_getBalance(Address :: address(),
                     BlockNumberOrTag :: non_neg_integer() | block_tag())
        -> {ok, non_neg_integer()} | {error, error()}.
eth_getBalance(Address, BlockNumberOrTag0) ->
    BlockNumberOrTag = block_number_or_tag(BlockNumberOrTag0),
    maybe_int(request(?METHOD_NAME_ETH_GET_BALANCE, ?METHOD_ID_ETH_GET_BALANCE,
              [Address, BlockNumberOrTag])).

%%====================================================================
%% Internal functions
%%====================================================================

request(MethodName, MethodId) ->
    request(MethodName, MethodId, []).

request(MethodName, MethodId, MethodArgs) ->
    Req = jsonrpc2_client:create_request({MethodName, MethodArgs, MethodId}),
    Encoded = binary_to_list(jiffy:encode(Req)),
    io:format("encoded: ~p\n", [Encoded]),
    {ok, ConnPid} = gun:open("localhost", 8545),
    {ok, _} = gun:await_up(ConnPid),
    StreamRef = gun:post(ConnPid, "/",
                         [{"content-type", "application/json"}],
                         Encoded),
    case gun:await(ConnPid, StreamRef) of
        {response, fin, _, _} ->
            {error, failed};
        {response, nofin, _, _} ->
            {ok, Response} = gun:await_body(ConnPid, StreamRef),
            case jiffy:decode(Response) of
                {Decoded} ->
                    proplists:get_value(<<"result">>, Decoded);
                _ -> {error, decoding_failed}
            end
    end.

-spec block_number_or_tag(BlockNumberOrTag :: non_neg_integer() | block_tag()) -> binary().
block_number_or_tag(latest) -> <<"latest">>;
block_number_or_tag(earliest) -> <<"earliest">>;
block_number_or_tag(pending) -> <<"pending">>;
block_number_or_tag(BlockNumber) when is_integer(BlockNumber) ->
    list_to_binary("0x" ++ integer_to_list(BlockNumber, 16)).

-spec maybe_int(binary() | error()) -> integer() | error().
maybe_int({error, _} = Error) -> Error;
maybe_int(<<"0x", Bin/binary>>) ->
    maybe_int(Bin);
maybe_int(Bin) ->
    case catch binary_to_integer(Bin) of
        I when is_integer(I) -> I;
        _ ->
            binary_to_integer(Bin, 16)
    end.

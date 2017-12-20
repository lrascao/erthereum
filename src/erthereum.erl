-module(erthereum).

%% API exports
%% net namespace
-export([net_version/0]).
%% eth namespace
-export([eth_blockNumber/0,
         eth_getBalance/1, eth_getBalance/2]).
%% personal namespace
-export([personal_newAccount/1]).

%% Type definitions.
-type error() :: failed | decoding_failed.
-type management_api_method() :: net_version |
                                 eth_blockNumber |
                                 eth_getBalance |
                                 personal_newAccount.
-type method_name() :: binary().
-type method_id() :: non_neg_integer().
-type address() :: binary().
-type passphrase() :: binary().
-type block_tag() :: latest | earliest | pending.

%% Type exports
-export_type([error/0]).

%% Defines

%%====================================================================
%% API functions
%%====================================================================

%% Returns the current network protocol version.
-spec net_version() -> {ok, non_neg_integer()} | {error, error()}.
net_version() ->
    maybe_int(request(management_api_data(net_version))).

%% Returns the number of most recent block.
-spec eth_blockNumber() -> {ok, non_neg_integer()} | {error, error()}.
eth_blockNumber() ->
    maybe_int(request(management_api_data(eth_blockNumber))).

%% Returns the balance of the account of given address.
-spec eth_getBalance(Address :: address()) -> {ok, non_neg_integer()} | {error, error()}.
eth_getBalance(Address) ->
    eth_getBalance(Address, latest).

%% Returns the balance of the account of given address.
-spec eth_getBalance(Address :: address(),
                     BlockNumberOrTag :: non_neg_integer() | block_tag())
        -> {ok, non_neg_integer()} | {error, error()}.
eth_getBalance(Address, BlockNumberOrTag0) ->
    BlockNumberOrTag = block_number_or_tag(BlockNumberOrTag0),
    maybe_int(request(management_api_data(eth_getBalance),
              [Address, BlockNumberOrTag])).

%% Generates a new private key and stores it in the key storese directory.
%% The key file is encrypted with the given passphrase.
%% Returns the address of the new account.
-spec personal_newAccount(Passphrase :: passphrase())
        -> {ok, address()} | {error, error()}.
personal_newAccount(Passphrase) ->
    maybe_binary(request(management_api_data(personal_newAccount),
                         [Passphrase])).

%%====================================================================
%% Internal functions
%%====================================================================

request(MethodData) ->
    request(MethodData, []).

request(MethodName, MethodArgs) when is_binary(MethodName) ->
    request({MethodName, undefined}, MethodArgs);
request({MethodName, MethodId}, MethodArgs) when is_binary(MethodName) ->
    Req = jsonrpc2_client:create_request({MethodName, MethodArgs, MethodId}),
    Encoded = binary_to_list(jiffy:encode(Req)),
    io:format("encoded: ~p\n", [Encoded]),
    {ok, ConnPid} = gun:open("localhost", 8545),
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

-spec maybe_int(binary() | error()) -> {ok, integer()} | error().
maybe_int({error, _} = Error) -> Error;
maybe_int(<<"0x", Bin/binary>>) ->
    maybe_int(Bin);
maybe_int(Bin) ->
    case catch binary_to_integer(Bin) of
        I when is_integer(I) -> {ok, I};
        _ ->
            {ok, binary_to_integer(Bin, 16)}
    end.

-spec maybe_binary(binary() | error()) -> {ok, binary()} | error().
maybe_binary({error, _} = Error) -> Error;
maybe_binary(Bin) when is_binary(Bin) ->
    {ok, Bin}.

-spec management_api_data(Method :: management_api_method())
        -> {method_name(), method_id()} | method_name().
management_api_data(net_version) ->
    {<<"net_version">>, 1};
management_api_data(eth_blockNumber) ->
    {<<"eth_blockNumber">>, 83};
management_api_data(eth_getBalance) ->
    {<<"eth_getBalance">>, 1};
management_api_data(personal_newAccount) ->
    <<"personal_newAccount">>.


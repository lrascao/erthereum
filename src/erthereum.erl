-module(erthereum).

%% API exports
%% net namespace
-export([net_version/0]).
%% eth namespace
-export([eth_blockNumber/0,
         eth_getBalance/1, eth_getBalance/2,
         eth_accounts/0,
         eth_sendTransaction/3]).
%% personal namespace
-export([personal_newAccount/1]).

%% Type definitions.
-type error() :: failed | decoding_failed.
-type management_api_method() :: net_version |
                                 eth_blockNumber |
                                 eth_getBalance |
                                 eth_accounts |
                                 personal_newAccount.
-type method_name() :: binary().
-type method_id() :: non_neg_integer().
-type address() :: binary().
-type passphrase() :: binary().
-type block_tag() :: latest | earliest | pending.
-type quantity() :: integer().
-type data() :: binary().

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

%% Returns a list of addresses owned by client.
-spec eth_accounts() -> {ok, list(address())} | {error, error()}.
eth_accounts() ->
    maybe_list(request(management_api_data(eth_accounts))).

%% Creates new message call transaction or a contract creation, if the data field contains code
-spec eth_sendTransaction(FromAddress :: address(),
                          ToAddress :: address(),
                          Value :: quantity()) -> {ok, TransactionHash :: data()} | {error, error()}.
eth_sendTransaction(FromAddress, ToAddress, Value) ->
    Params = [{[{<<"from">>, FromAddress},
              {<<"to">>, ToAddress},
              {<<"value">>, eth_int(Value)}]}],
    maybe_binary(request(management_api_data(eth_sendTransaction), Params)).


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
                    process_decoded_body(Decoded);
                _ ->
                    {error, decoding_failed}
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

-spec maybe_list(list(binary()) | error()) -> {ok, list(binary())} | error().
maybe_list({error, _} = Error) -> Error;
maybe_list(L) when is_list(L) ->
    {ok, L}.

-spec eth_int(Integer :: integer()) -> binary().
eth_int(Integer) when is_integer(Integer) ->
    <<"0x", (integer_to_binary(Integer, 16))/binary>>.

-spec management_api_data(Method :: management_api_method())
        -> {method_name(), method_id()} | method_name().
management_api_data(net_version) ->
    {<<"net_version">>, 1};
management_api_data(eth_blockNumber) ->
    {<<"eth_blockNumber">>, 83};
management_api_data(eth_getBalance) ->
    {<<"eth_getBalance">>, 1};
management_api_data(personal_newAccount) ->
    <<"personal_newAccount">>;
management_api_data(eth_accounts) ->
    {<<"eth_accounts">>, 1};
management_api_data(eth_sendTransaction) ->
    {<<"eth_sendTransaction">>, 1}.

process_decoded_body(DecodedBody) ->
    case kvlist:get(<<"result">>, DecodedBody) of
        undefined ->
            Error = kvlist:get(<<"error">>, DecodedBody),
            {error, Error};
        Result when is_binary(Result) ->
            Result
    end.


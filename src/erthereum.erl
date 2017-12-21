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
-export([personal_newAccount/1,
         personal_unlockAccount/2]).

%% Type definitions.
-type error() :: failed | decoding_failed.
-type management_api_method() :: net_version |
                                 eth_blockNumber |
                                 eth_getBalance |
                                 eth_accounts |
                                 eth_sendTransaction |
                                 personal_newAccount |
                                 personal_unlockAccount.
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
    maybe_int(request(net_version)).

%% Returns the number of most recent block.
-spec eth_blockNumber() -> {ok, non_neg_integer()} | {error, error()}.
eth_blockNumber() ->
    maybe_int(request(eth_blockNumber)).

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
    maybe_int(request(eth_getBalance, [Address, BlockNumberOrTag])).

%% Returns a list of addresses owned by client.
-spec eth_accounts() -> {ok, list(address())} | {error, error()}.
eth_accounts() ->
    maybe_list(request(eth_accounts)).

%% Creates new message call transaction or a contract creation, if the data field contains code
-spec eth_sendTransaction(FromAddress :: address(),
                          ToAddress :: address(),
                          Value :: quantity()) -> {ok, TransactionHash :: data()} | {error, error()}.
eth_sendTransaction(FromAddress, ToAddress, Value) ->
    Params = [{[{<<"from">>, FromAddress},
              {<<"to">>, ToAddress},
              {<<"value">>, eth_int(Value)}]}],
    maybe_binary(request(eth_sendTransaction, Params)).


%% Generates a new private key and stores it in the key storese directory.
%% The key file is encrypted with the given passphrase.
%% Returns the address of the new account.
-spec personal_newAccount(Passphrase :: passphrase())
        -> {ok, address()} | {error, error()}.
personal_newAccount(Passphrase) ->
    maybe_binary(request(personal_newAccount, [Passphrase])).

-spec personal_unlockAccount(Address :: address(),
                             Passphrase :: passphrase()
                            )
                            -> {ok, address()} | {error, error()}.
personal_unlockAccount(Address, Passphrase) ->
    maybe_binary(request(personal_unlockAccount, [Address, Passphrase])).

%%====================================================================
%% Internal functions
%%====================================================================
-spec request(Method :: management_api_method()) -> any() | {error, any()}.
request(Method) ->
    request(Method, []).

-spec request(Method :: management_api_method(),
              Arguments :: list()) -> any() | {error, any()}.
request(Method, Arguments) ->
    erth_request:request(management_api_data(Method), Arguments).

-spec block_number_or_tag(BlockNumberOrTag :: non_neg_integer() | block_tag()) -> binary().
block_number_or_tag(latest) -> <<"latest">>;
block_number_or_tag(earliest) -> <<"earliest">>;
block_number_or_tag(pending) -> <<"pending">>;
block_number_or_tag(BlockNumber) when is_integer(BlockNumber) -> eth_int(BlockNumber).

-spec maybe_int({ok, binary()} | error()) -> {ok, integer()} | error().
maybe_int({error, _} = Error) -> Error;
maybe_int({ok, <<"0x", Bin/binary>>}) ->
    maybe_int({ok, Bin});
maybe_int({ok, Bin}) ->
    case catch binary_to_integer(Bin) of
        I when is_integer(I) -> {ok, I};
        _ ->
            {ok, binary_to_integer(Bin, 16)}
    end.

-spec maybe_binary({ok, binary()} | error()) -> {ok, binary()} | error().
maybe_binary({error, _} = Error) -> Error;
maybe_binary({ok, Bin}) when is_binary(Bin) ->
    {ok, Bin}.

-spec maybe_list({ok, list(binary())} | error()) -> {ok, list(binary())} | error().
maybe_list({error, _} = Error) -> Error;
maybe_list({ok, L}) when is_list(L) ->
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
management_api_data(personal_unlockAccount) ->
    <<"personal_unlockAccount">>;
management_api_data(eth_accounts) ->
    {<<"eth_accounts">>, 1};
management_api_data(eth_sendTransaction) ->
    {<<"eth_sendTransaction">>, 1}.




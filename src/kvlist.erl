%%
 % Miniclip SA, 2017. All rights reserved.
 %
-module(kvlist).
-author("dinis.rosario").

%% API
-export([get/2, get/3]).

-spec get(Key :: any(),
          List :: list()) -> any().
get(Key, List) ->
    get(Key, List, undefined).

-spec get(Key :: any(),
          List :: list(),
          Default :: any()) -> any().
get(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Value} -> Value;
        _ -> Default
    end.
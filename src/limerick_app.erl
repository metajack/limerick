-module(limerick_app).
-author("Jack Moffitt <jack@metajack.im>").

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    limerick_sup:start_link().

stop(_State) ->
    ok.

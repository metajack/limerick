-module(limerick).
-author("Jack Moffitt <jack@metajack.im>").

-export([start/0, stop/0]).

start() ->
    ok = application:start(crypto),
    ok = application:start(limerick).

stop() ->
    Res = application:stop(limerick),
    ok = application:stop(crypto),
    ok = Res.
    

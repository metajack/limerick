-module(limerick_web).
-author("Jack Moffitt <jack@metajack.im>").

-export([start/1, stop/0, process/3]).

-include("limerick_bosh.hrl").

start(Options) ->
    Process = fun (Req) ->
                      ?MODULE:process(Req, Req:get(method), Req:get(path))
              end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Process} | Options]).

stop() ->
    mochiweb_http:stop(?MODULE).

process(Req, 'GET', "/") ->
    Req:ok({"text/plain", "Limerick server running"});
process(Req, 'GET', _Path) ->
    Req:not_found();
process(Req, 'POST', Path) ->
    Paths = ["/xmpp-httpbind", "/http-bind"],
    case lists:member(Path, Paths) of
        true ->
            handle_bosh(Req);
        false ->
            Req:not_found()
    end;
process(Req, 'OPTIONS', _) ->
    Req:respond({200, [{"Content-Type", "text/plain"} | ?CORS_HEADERS], []});
process(Req, _Method, _Path) ->
    Req:respond({501, ?CORS_HEADERS, <<"Not implemented">>}).

%% handle a BOSH request
handle_bosh(Req) ->
    case Req:get_primary_header_value('content-type') of
        Type when Type =:= "text/xml"; Type =:= "application/xml" ->
            case Req:recv_body() of
                undefined ->
                    Req:respond({400, ?CORS_HEADERS,
                                 <<"Bad request - no body">>});
                Body ->
                    limerick_bosh:process(Req, Body)
            end;
        _ ->
            Req:respond({400, ?CORS_HEADERS,
                         <<"Bad request - bad content-type">>})
    end.

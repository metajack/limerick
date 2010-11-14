-module(limerick_bosh).
-author("Jack Moffitt <jack@metajack.im>").

-include_lib("xmerl/include/xmerl.hrl").

-include("limerick_bosh.hrl").

-export([process/2]).

bad_request(Req, Reason) ->
    Response = list_to_binary("Bad request - " ++ Reason),
    Req:respond({400, ?CORS_HEADERS, Response}).

parse_body(Body) ->
    try
        {Xml, _} = xmerl_scan:string(binary_to_list(Body)),
        case Xml#xmlElement.name of
            body ->
                Sid = lmrx:get_aval(sid, Xml),
                {Sid, Xml};
            _ ->
                error
        end
    catch
        _:_ ->
            error
    end.

new_sid() ->
    {Mega, Secs, Micro} = now(),
    TimeString = io_lib:format("~p:~p:~p", [Mega, Secs, Micro]),
    <<Digest:160/big-unsigned-integer>> = crypto:sha(TimeString),
    lists:flatten(io_lib:format("~40.16.0b", [Digest])).

serialize_response(Response) ->
    iolist_to_binary(
      xmerl:export_simple(Response, xmerl_xml, [{prolog, ""}])).

process(Req, Body) ->
    case parse_body(Body) of
        {undefined, BodyElem} ->
            case create_session(BodyElem) of
                error ->
                    bad_request(Req, "bad create");
                Response ->
                    Req:respond({200, ?CORS_HEADERS,
                                 serialize_response(Response)})
            end;
        {Sid, BodyElem} ->
            case ets:lookup(limerick_sessions, Sid) of
                [{Sid, Pid}] ->
                    Response = limerick_session:handle_body(Pid, BodyElem),
                    Req:respond({200, ?CORS_HEADERS,
                                 serialize_response(Response)});
                [] ->
                    Req:respond({404, ?CORS_HEADERS,
                                 <<"Session not found">>})
            end;
        _ ->
            bad_request(Req, "bad body")
    end.

create_session(#xmlElement{attributes=Attrs}) ->
    Sid = new_sid(),
    Rid = lmrx:get_aval_int(rid, Attrs),
    Wait = lmrx:get_aval_int(wait, Attrs),
    Hold = lmrx:get_aval_int(hold, Attrs),

    case {Rid, Wait, Hold} of
        {R, W, H} when is_integer(H),
                       H =:= 1,
                       is_integer(R),
                       R > 0,
                       is_integer(W),
                       W >= 60 ->
            limerick_session:create([{sid, Sid}, {rid, Rid}]),

            [{body, [{sid, Sid}, {hold, 1}, {wait, 60},
                     {inactivity, 30}, {requests, 2}, {maxpause, 120},
                     {ver, "1.10"}, {'xmpp:restartlogic', "true"},
                     {'xmpp:version', "1.0"},
                     {xmlns, ?NS_HTTP_BIND},
                     {'xmlns:xmpp', ?NS_XBOSH},
                     {'xmlns:stream', ?NS_STREAMS}],
              [{'stream:features',
                [{mechanisms, [{xmlns, ?NS_XMPP_SASL}],
                  [{mechanism, ["ANONYMOUS"]}]}]}]}];
        _ ->
            error
    end.
    

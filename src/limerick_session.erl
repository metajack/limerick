-module(limerick_session).
-author("Jack Moffitt <jack@metajack.im>").

-behaviour(gen_fsm).

-include_lib("xmerl/include/xmerl.hrl").

-include("limerick_bosh.hrl").

%% gen_fsm exports
-export([init/1,
         terminate/3,
         code_change/4,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         waiting_for_auth/2,
         waiting_for_auth/3,
         waiting_for_restart/2,
         waiting_for_restart/3,
         waiting_for_bind/2,
         waiting_for_bind/3,
         idle/2,
         idle/3]).

%% API
-export([start_link/1, create/1, handle_body/2]).

-record(sd, {sid, rid, reply_to, cache=[], pending=[]}).

-define(WINDOW, 2).
-define(TIMEOUT, 60000).
-define(INACTIVITY_TIMEOUT, 30000).

start_link(Config) ->
    gen_fsm:start_link(?MODULE, Config, []).

create(Config) ->
    {ok, _} = limerick_session_sup:start_session(Config).

handle_body(Pid, Body) ->
    gen_fsm:sync_send_all_state_event(Pid, {body, Body}, 65000).

init(Config) ->
    Sid = proplists:get_value(sid, Config),
    Rid = proplists:get_value(rid, Config),

    ets:insert(limerick_sessions, {Sid, self()}),
    
    {ok, waiting_for_auth, #sd{sid=Sid, rid=Rid}}.
         
terminate(_Reason, _StateName, StateData) ->
    ets:delete(limerick_sessions, StateData#sd.sid),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

handle_event(_Event, _StateName, StateData) ->
    {stop, unknown_event, StateData}.

handle_sync_event({body, Body}, From, StateName,
                  StateData = #sd{rid=Rid, pending=Pending,
                                  reply_to=ReplyTo}) ->
    %% check for a held request
    StateData1 = case ReplyTo of
                     undefined ->
                         StateData;
                     _ ->
                         %% a held request was waiting, so send empty reponse
                         reply(ReplyTo, [{body, 
                                          [{xmlns, ?NS_HTTP_BIND}], []}],
                               StateData)
                 end,

    case lmrx:get_aval_int(rid, Body) of
        NewRid when NewRid =:= Rid + 1 ->
            %% we got the next RID
            Els = [E || E = #xmlElement{} <- Body#xmlElement.content],
            case Els of
                [] ->
                    case lmrx:get_aval('xmpp:restart', Body) of
                        undefined ->
                            %% Got idle body
                            {next_state, StateName,
                             StateData1#sd{rid=NewRid, reply_to=From},
                             ?TIMEOUT};
                        _ ->
                            ?MODULE:StateName({body, Body}, From,
                                              StateData1#sd{rid=NewRid,
                                                            reply_to=From})
                    end;
                _ ->
                    ?MODULE:StateName({body, Body}, From,
                                      StateData1#sd{rid=NewRid,
                                                    reply_to=From})
            end;
        NewRid when NewRid > Rid, NewRid =< Rid + ?WINDOW ->
            %% we got a RID within the window but out of sequence
            {next_state, StateName,
             StateData1#sd{reply_to=From,
                           pending=[{NewRid, Body} | Pending]}, ?TIMEOUT};
        _ ->
            %% bad RID; terminate the session
            Reply = [{body, [{xmlns, ?NS_HTTP_BIND},
                             {type, "terminate"}]}],
            {stop, normal, Reply, StateData1#sd{reply_to=undefined}}
    end.

handle_info(_Info, _StateName, StateData) ->
    {stop, unknown_info, StateData}.

waiting_for_auth(timeout, StateData) ->
    timeout(waiting_for_auth, StateData).

waiting_for_auth({body, #xmlElement{content=Content}}, _From,
                 StateData = #sd{reply_to=ReplyTo}) ->
    Auth = [E || E = #xmlElement{name=auth} <- Content],
    case Auth of
        [] ->
            {next_state, waiting_for_auth, StateData, ?TIMEOUT};
        [El] ->
            Mech = lmrx:get_aval(mechanism, El),
            case Mech of
                "ANONYMOUS" ->
                    StateData1 = reply(ReplyTo, 
                                       [{body, [{xmlns, ?NS_HTTP_BIND}],
                                         [{success,
                                           [{xmlns, ?NS_XMPP_SASL}], []}]}],
                                       StateData),
                    {next_state, waiting_for_restart,
                     StateData1, ?INACTIVITY_TIMEOUT};
                _ ->
                    StateData1 = reply(ReplyTo,
                                       [{body, [{xmlns, ?NS_HTTP_BIND}],
                                         [{'invalid-mechanism',
                                           [{xmlns, ?NS_XMPP_SASL}], []}]}],
                                       StateData),
                    {next_state, waiting_for_auth,
                     StateData1, ?INACTIVITY_TIMEOUT}
            end
    end.

waiting_for_restart(timeout, StateData) ->
    timeout(waiting_for_restart, StateData).

waiting_for_restart({body, Body}, _From,
                    StateData = #sd{reply_to=ReplyTo}) ->
    case lmrx:get_aval('xmpp:restart', Body) of
        undefined ->
            {next_state, waiting_for_restart, StateData, ?TIMEOUT};
        _ ->
            StateData1 = reply(ReplyTo,
                               [{body, [{'xmlns:stream', ?NS_STREAMS},
                                        {xmlns, ?NS_HTTP_BIND}],
                                 [{'stream:features',
                                   [{bind, [{xmlns, ?NS_XMPP_BIND}], []}]}]}],
                               StateData),
            {next_state, waiting_for_bind,
             StateData1, ?INACTIVITY_TIMEOUT}
    end.
            
waiting_for_bind(timeout, StateData) ->
    timeout(waiting_for_bind, StateData).

waiting_for_bind({body, #xmlElement{content=Content}}, _From,
                 StateData = #sd{reply_to=ReplyTo}) ->
    IQs = [E || E = #xmlElement{name=iq} <- Content],
    case find_bind(IQs) of
        {_, undefined} ->
            {next_state, waiting_for_bind, StateData, ?TIMEOUT};
        {Id, #xmlElement{}} ->
            StateData1 = reply(ReplyTo,
                               [{body, [{xmlns, ?NS_HTTP_BIND}],
                                 [{iq, [{xmlns, ?NS_CLIENT},
                                        {type, "result"},
                                        {id, Id}],
                                   [{bind, [{xmlns, ?NS_XMPP_BIND}],
                                     [{jid, ["fake@localhost/fake"]}]}]}]}],
                               StateData),
            {next_state, idle,
             StateData1, ?INACTIVITY_TIMEOUT}
    end.

idle(timeout, StateData) ->
    timeout(idle, StateData).

idle({body, _}, _From, StateData) ->
    {next_state, idle, StateData, ?TIMEOUT}.

reply(To, Reply, StateData = #sd{rid=Rid, cache=Cache}) ->
    %% cache the request and reply
    NewCache = case Cache of
                   [] ->
                       [{Rid, Reply}];
                   [{SomeRid, _} = Item] when SomeRid < Rid ->
                       [Item, {Rid, Reply}];
                   [Item] ->
                       [{Rid, Reply} | Item];
                   [{SomeRid1, _}, {SomeRid2, _} = Item2] ->
                       case {SomeRid1 < Rid, SomeRid2 < Rid} of
                           {false, _} ->
                               %% cache is newer or equal
                               Cache;
                           {true, false} ->
                               [{Rid, Reply}, Item2];
                           {true, true} ->
                               [Item2, {Rid, Reply}]
                       end
               end,

    gen_fsm:reply(To, Reply),

    StateData#sd{cache=NewCache, reply_to=undefined}.

timeout(StateName, StateData = #sd{reply_to=ReplyTo}) ->
    case ReplyTo of
        undefined ->
            %% inactivity timeout
            {stop, normal, StateData};
        _ ->
            %% time to release the held request, even with no data ready
            StateData1 = reply(ReplyTo,
                               [{body, [{xmlns, ?NS_HTTP_BIND}], []}],
                               StateData),
            {next_state, StateName, StateData1, ?INACTIVITY_TIMEOUT}
    end.

find_bind([]) ->
    {undefined, undefined};
find_bind([Iq | Rest]) ->
    case {lmrx:get_aval(type, Iq), lmrx:get_child(bind, Iq)} of
        {"set", Bind = #xmlElement{}} ->
            {lmrx:get_aval(id, Iq), Bind};
        _ ->
            find_bind(Rest)
    end.

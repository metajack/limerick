-module(limerick_session_sup).
-author("Jack Moffitt <jack@metajack.im>").

-behaviour(supervisor).

%% API
-export([start_link/1, start_session/1]).

%% supervisor callbacks
-export([init/1]).

start_link([]) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Config :: [ConfigItem]
%% ConfigItem :: {sid, string()}
%%            || {rid, integer()}
%%            || {req, mochirequest()}
start_session(Config) ->
    supervisor:start_child(limerick_session_sup, [Config]).

init([]) ->
    %% create session ETS table
    ets:new(limerick_sessions, [set, public, named_table]),

    {ok, {{simple_one_for_one, 5, 10},
          [{undefined,
            {limerick_session, start_link, []},
            temporary, 5000, worker, [limerick_session]}]}}.

-module(limerick_sup).
-author("Jack Moffitt <jack@metajack.im>").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    WebConfig = [{ip, "0.0.0.0"},
                 {port, 5280}],

    Web = {limerick_web,
           {limerick_web, start, [WebConfig]},
           permanent, 5000, worker, dynamic},

    SessionSup = {limerick_session_sup,
                  {limerick_session_sup, start_link, [[]]},
                  permanent, infinity, supervisor, [limerick_session_sup]},

    Processes = [Web, SessionSup],

    {ok, {{one_for_one, 5, 10}, Processes}}.


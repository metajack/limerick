-module(limerick_ctl).
-author("Jack Moffitt <jack@metajack.im>").

-export([sessions/0, kill/2, error/2, start/0]).

sessions() ->
    [S || [S] <- ets:match(limerick_sessions, {'$1', '_'})].

%% force close the socket with no response or force a specific
%% http error response
kill(Sid, How) ->
    case ets:lookup(limerick_sessions, Sid) of
        [] ->
            not_found;
        [{Sid, Pid}] ->
            limerick_session:kill(Pid, How)
    end.

%% force a session to have a single, non-fatal error
error(Sid, How) ->
    case ets:lookup(limerick_sessions, Sid) of
        [] ->
            not_found;
        [{Sid, Pid}] ->
            limerick_session:error(Pid, How)
    end.

start() ->
    Args = init:get_plain_arguments(),
    run(Args).

run([_]) ->
    io:format("usage: limerickctl COMMAND OPTIONS~n"
              "~n"
              "commands available:~n"
              "    sessions - list all active sessions~n"
              "    kill SID - brutal kill session SID~n"
              "    kill SID CODE - kill session SID with HTTP code CODE~n"
              "~n"),
    halt(1);
run([NodeStr, "sessions"]) ->
    Node = list_to_atom(NodeStr),
    case rpc:call(Node, ?MODULE, sessions, []) of
        [] ->
            io:format("No sessions.~n"),
            halt(0);
        Sessions when is_list(Sessions) ->
            io:format("Sessions:~n"),
            lists:map(fun(Sid) ->
                              io:format("    " ++ Sid ++ "~n")
                      end, Sessions),
            halt(0);
        Error ->
            io:format("error: command failed: ~p~n", [Error]),
            halt(1)
    end;
run([NodeStr, "kill", Sid]) ->
    Node = list_to_atom(NodeStr),
    case rpc:call(Node, ?MODULE, kill, [Sid, terminate]) of
        ok ->
            io:format("Session ~p scheduled for brutal kill.~n", [Sid]),
            halt(0);
        not_found ->
            io:format("Session ~p not found.~n", [Sid]),
            halt(1);
        Error ->
            io:format("error: command failed: ~p~n", [Error]),
            halt(1)
    end;
run([NodeStr, "kill", Sid, Code]) ->
    Node = list_to_atom(NodeStr),
    case rpc:call(Node, ?MODULE, kill, [Sid, {http, list_to_integer(Code)}]) of
        ok ->
            io:format("Session ~p scheduled for kill with HTTP status ~p.~n",
                      [Sid, Code]),
            halt(0);
        not_found ->
            io:format("Session ~p not found.~n", [Sid]),
            halt(1);
        Error ->
            io:format("error: command failed: ~p~n", [Error]),
            halt(1)
    end;
run([NodeStr, "error", Sid]) ->
    Node = list_to_atom(NodeStr),
    case rpc:call(Node, ?MODULE, error, [Sid, terminate]) of
        ok ->
            io:format("Session ~p scheduled for forced closed socket error.~n",
                      [Sid]),
            halt(0);
        not_found ->
            io:format("Session ~p not found.~n", [Sid]),
            halt(1);
        Error ->
            io:format("error: command failed: ~p~n", [Error]),
            halt(1)
    end;
run([_NodeStr, "error", _Sid, [C, _, _]]) when C =/= $5 ->
    io:format("error: Error code must be a 5xx error.~n"),
    halt(1);
run([NodeStr, "error", Sid, Code]) ->
    Node = list_to_atom(NodeStr),
    case rpc:call(Node, ?MODULE, error, [Sid, {http, Code}]) of
        ok ->
            io:format("Session ~p scheduled for HTTP error ~p.~n", [Sid, Code]),
            halt(0);
        not_found ->
            io:format("Session ~p not found.~n", [Sid]),
            halt(1);
        Error ->
            io:format("error: command failed: ~p~n", [Error]),
            halt(1)
    end.
                
               
                


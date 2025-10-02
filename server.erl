-module(server).
-export([start/1,stop/1]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    Pid = spawn(fun() -> loop(#{} ) end), %starta med en tom map
    register(ServerAtom, Pid),
    Pid.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok

    case whereis(ServerAtom) of
        undefined -> ok;
        Pid -> exit(Pid, normal), ok
    end.

loop(State) ->
    receive
    % Användare med namn Nick vill gå med i kanalen Channel
        {request, From, Ref, {join, Nick, Channel}} ->
            io:format("~p wants to join ~p~n", [Nick, Channel]),
            Members = maps:get(Channel, State, []),
            case lists:member({From, Nick}, Members) of
                true ->
                    % Redan med i kanalen
                    From ! {result, Ref, {error, user_already_joined, "You already joined this channel"}},
                    loop(State);
                false ->
                    % Lägg till ny medlem
                    NewMembers = [{From, Nick} | Members],
                    NewState = maps:put(Channel, NewMembers, State),
                    From ! {result, Ref, ok},
                    loop(NewState)
            end;

    % Användare med namn Nick vill lämna kanalen Channel
        {request, From, Ref, {leave, Nick, Channel}} ->
            Members = maps:get(Channel, State, []),
            case lists:keyfind(From, 1, Members) of
                false ->
                    % Användaren är inte med i kanalen
                    From ! {result, Ref, {error, user_not_joined, "You are NOT in this channel"}},
                    loop(State);
                {_From, _Nick} ->
                    % Ta bort användaren från listan
                    NewMembers = lists:keydelete(From, 1, Members),
                    NewState = maps:put(Channel, NewMembers, State),
                    From ! {result, Ref, ok},
                    loop(NewState)
            end;

    % Skickar meddelanden till alla medlemmar i kanalen
        {request, From, Ref, {message_send, Nick, Channel, Msg}} ->
            Members = maps:get(Channel, State, []),
            case lists:keyfind(From, 1, Members) of
                false ->
                    % Användaren är inte med i kanalen
                    From ! {result, Ref, {error, user_not_joined, "You are NOT in this channel"}},
                    loop(State);
                _ ->
                    % Skicka meddelandet till alla medlemmar
                    lists:foreach(fun({Pid, MemberNick}) ->
                        Pid ! {message_receive, Channel, Nick, Msg} end, Members),
                    From ! {result, Ref, ok},
                    loop(State)
            end;

    % catch-all (debug)
        Msg ->
            io:format("Unknown msg: ~p~n", [Msg]),
            loop(State)
    end.

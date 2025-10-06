-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server % atom of the chat server
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom
    }.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) ->
    Server = St#client_st.server,
    % Sends a join request to the server and catches all responses
    %  <-- ändrat: skickar också med klientens nick, så servern vet vilka nicks som är upptagna
    Result = (catch genserver:request(Server, {join, self(), Channel, St#client_st.nick})),
    case Result of
        {'EXIT',_}    -> {reply, {error, server_not_reached, "Server does not respond"}, St}; % when server has been stopped
        timeout_error -> {reply, {error, server_not_reached, "Server does not respond"}, St}; % when the server has timed out / unresponsive
        _Else         -> {reply, Result, St} % is either {reply, ok, _} or {reply, {error...} _}.
    end;

% Leave channel
handle(St, {leave, Channel}) ->
    % Sends a leave request to the channel's process
    Result = genserver:request(list_to_atom(Channel), {leave, self()}),
    {reply, Result, St};

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    % Sends a message_send request to the channel's process, along with the message, nick of the sender, and the sender itself
    Result = (catch genserver:request(list_to_atom(Channel), {message_send, Msg, St#client_st.nick, self()})),
    case Result of
        % Server has been shut down/exited, and the channel therefore doesn't exist
        {'EXIT', _} -> {reply, {error, server_not_reached, "Server does not respond"}, St};
        % Either ok or an error that has been caught in server
        _Else       -> {reply, Result, St}
    end;

% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    Server = St#client_st.server,
    % Försök först med servern som står i state
    Result1 = (catch genserver:request(Server, {nick, self(), NewNick})),
    Result = case Result1 of
                 {'EXIT', _}    -> (catch genserver:request(server, {nick, self(), NewNick})); % fallback
                 timeout_error  -> (catch genserver:request(server, {nick, self(), NewNick}));
                 _ -> Result1
             end,
    case Result of
        {'EXIT',_}    -> {reply, {error, server_not_reached, "Server does not respond"}, St};
        timeout_error -> {reply, {error, server_not_reached, "Server does not respond"}, St};
        ok            -> {reply, ok, St#client_st{nick = NewNick}};
        _Else         -> {reply, Result, St}
    end;


% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .

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
    Result = catch genserver:request(Server, {join, self(), Channel, St#client_st.nick}),
    {reply, normalize_result(Result), St};

% Leave channel
handle(St, {leave, Channel}) ->
    {reply, genserver:request(list_to_atom(Channel), {leave, self()}), St};

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    Result = catch genserver:request(list_to_atom(Channel), {message_send, Msg, St#client_st.nick, self()}),
    {reply, normalize_result(Result), St};

%Change nick
handle(St, {nick, NewNick}) ->
    Server = St#client_st.server,
    Result = try_request(Server, {nick, self(), NewNick}),
    case Result of
        ok -> {reply, ok, St#client_st{nick = NewNick}};
        _  -> {reply, normalize_result(Result), St}
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


%Help functions

%Simplifies error messages
normalize_result({'EXIT', _}) ->
    {error, server_not_reached, "Server does not respond"};
normalize_result(timeout_error) ->
    {error, server_not_reached, "Server does not respond"};
normalize_result(Result) ->
    Result.

%Attempts to send a request to the given server
try_request(Server, Request) ->
    case catch genserver:request(Server, Request) of
        {'EXIT', _} -> catch genserver:request(server, Request);
        timeout_error -> catch genserver:request(server, Request);
        Result -> Result
    end.

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


handle(St = #client_st{server = Server, nick = Nick}, {join, Channel}) ->
    Ref = make_ref(),
    Server ! {request, self(), Ref, {join, Nick, Channel}},
    receive
        {result, Ref, ok} ->
            {reply, ok, St};
        {result, Ref, {error, Atom, Msg}} ->
            {reply, {error, Atom, Msg}, St}
    after 1000 ->
        {reply, {error, server_not_reached, "Server not responding"}, St}
    end;


% Leave channel
handle(St = #client_st{server = Server, nick = Nick}, {leave, Channel}) ->
    Ref = make_ref(),
    Server ! {request, self(), Ref, {leave, Nick, Channel}},
    receive
        {result, Ref, ok} ->
            {reply, ok, St};
        {result, Ref, {error, Atom, Msg}} ->
            {reply, {error, Atom, Msg}, St}
    after 1000 ->
        {reply, {error, server_not_reached, "Server not responding"}, St}
    end;

% Sending message (from GUI, to channel)
handle(St = #client_st{server = Server, nick = Nick}, {message_send, Channel, Msg}) ->
    Ref = make_ref(),
    Server ! {request, self(), Ref, {message_send, Nick, Channel, Msg}},
    receive
        {result, Ref, ok} ->
            {reply, ok, St};
        {result, Ref, {error, Atom, Text}} ->
            {reply, {error, Atom, Text}, St}
    after 1000 ->
        {reply, {error, server_not_reached, "Server not responding"}, St}
    end;

% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}} ;

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

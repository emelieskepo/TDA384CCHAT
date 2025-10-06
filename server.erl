-module(server).
-export([start/1,stop/1]).

-record(server_state, {
    server,
    channels,
    nicknames
}).

-record(channel_state, {
    name,
    members}).

new_server(ServerAtom) ->
    #server_state{
        server = ServerAtom,
        channels = [],
        nicknames = []
    }.


new_channel(Channel, MemberPid) ->
    #channel_state{
        name = Channel,
        members = [MemberPid]
    }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    spawn(genserver, start, [ServerAtom, new_server(ServerAtom), fun handle_server/2]).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:request(ServerAtom, kill_channels),
    genserver:stop(ServerAtom).

% handle_server/2 handles each kind of request sent to the server from Client
% Parameters:
%   - the current state of the server (State)
%   - request data from client.
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to the Client
%   - NewState is the updated state of the server


% This one handles all join requests.
handle_server(State, {join, Client, Channel}) ->
    Channels = State#server_state.channels,
    % Checks if the channel exists already.
    case lists:member(Channel, Channels) of
        true  ->
            % Channel already exists,
            % Sends request to the channel to join it.
            Result = (catch genserver:request(list_to_atom(Channel), {join, Client})),
            % Sends response that user successfully joined channel
            {reply, Result, State};
        false ->
            % Initiates channel and spawns process for it.
            spawn(genserver, start, [list_to_atom(Channel), new_channel(Channel, Client), fun handle_channel/2]),
            % Sends response that user successfully joined channel
            {reply, ok, State#server_state{channels = Channels ++ [Channel]}}
    end;

% Handles request to kill all channels (sent upon shutdown)
handle_server(State, kill_channels) ->
    % Stops all channels' processes
    lists:foreach(fun(Ch) -> genserver:stop(list_to_atom(Ch)) end, State#server_state.channels), % kanske ta bort?
    {reply, ok, State#server_state{channels = []}}.

% handle_channel/2 handles each kind of request sent to a channels, either from server or a client.
% Parameters:
%   - the current state of the channel (C_St)
%   - request data.
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to the Client/Server
%   - NewState is the updated state of the channel

% Handles all join requests sent to channel
handle_channel(C_St, {join, Client}) ->
    Members = C_St#channel_state.members,
    % Checks if the user is already a member.
    case lists:member(Client, Members) of
        true  ->
            {reply, {error, user_already_joined, "User already joined"}, C_St};
        false ->
            {reply, ok, C_St#channel_state{members = Members ++ [Client]}}
    end;

% Handles all leave requests sent to channel
handle_channel(C_St, {leave, Client}) ->
    Members = C_St#channel_state.members,
    % Checks if the user is a member.
    case lists:member(Client, Members) of
        % not a member -> fail
        false -> {reply, {error, user_not_joined, "User not in channel"}, C_St};
        % is a member -> remove them from the channel
        true  -> {reply, ok, C_St#channel_state{members = lists:delete(Client, Members)}}
    end;

% Handles all message requests sent to channel
handle_channel(C_St, {message_send, Msg, Nick, Sender}) ->
    Members = C_St#channel_state.members,
    Channel = C_St#channel_state.name,
    % Checks if user is a member of the channel
    case lists:member(Sender, Members) of
        false ->
            {reply, {error, user_not_joined, "User is not in the channel."}, C_St};
        true  ->
            % spawns a request for each member (except the sender) to receive the message
            [spawn(genserver, request, [Recipient, {message_receive, Channel, Nick, Msg}])
                || Recipient <- Members, Recipient =/= Sender],

            {reply, ok, C_St}
    end.

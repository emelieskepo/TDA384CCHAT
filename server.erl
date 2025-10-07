-module(server).
-export([start/1, stop/1]).

-record(server_state, {server, channels = [], nicknames = []}).
-record(channel_state, {name, members = []}).

%Creates a new server
new_server(ServerAtom) ->
    #server_state{server = ServerAtom}.

%Creates a new channel
new_channel(Channel, MemberPid) ->
    #channel_state{name = Channel, members = [MemberPid]}.


% Start a new server process with the given name (ServerAtom)
% Do not change the signature of this function.
start(ServerAtom) ->
    spawn(genserver, start, [ServerAtom, new_server(ServerAtom), fun handle_server/2]).

%Close all channels and server
stop(ServerAtom) ->
    genserver:request(ServerAtom, kill_channels),
    genserver:stop(ServerAtom).


%This one handles all join requests.
handle_server(State, {join, Client, Channel, Nick}) ->
    Channels = State#server_state.channels,
    Nicks = State#server_state.nicknames,

    %Checks if channel already exists
    case lists:member(Channel, Channels) of
        true ->
            % Channel exists - ask it to join
            case catch genserver:request(list_to_atom(Channel), {join, Client}) of
                ok ->
                    % Add nick if new
                    UpdatedNicks = ensure_nick(Nick, Client, Nicks),
                    {reply, ok, State#server_state{nicknames = UpdatedNicks}};
                Result ->
                    {reply, Result, State}
            end;

        false ->
            % Channel doesn't exist - create it and add the client
            spawn(genserver, start, [list_to_atom(Channel), new_channel(Channel, Client), fun handle_channel/2]),
            UpdatedNicks = ensure_nick(Nick, Client, Nicks),
            {reply, ok, State#server_state{
                channels = Channels ++ [Channel],
                nicknames = UpdatedNicks
            }}
    end;

%This one handles nick changes
handle_server(State, {nick, Client, NewNick}) ->
    Nicks = State#server_state.nicknames,

    %Checks if the nick is available
    case lists:keyfind(NewNick, 1, Nicks) of
        {NewNick, Owner} when Owner =:= Client -> %This is already your nick
            {reply, ok, State};
        {NewNick, _} -> %The nick is taken
            {reply, {error, nick_taken, "Nick already taken"}, State};
        false -> %The nick is free
            CleanNicks = remove_old_nick(Client, Nicks),
            NewList = CleanNicks ++ [{NewNick, Client}],
            {reply, ok, State#server_state{nicknames = NewList}}
    end;

%Close all channels
handle_server(State, kill_channels) ->
    lists:foreach(fun(Ch) -> genserver:stop(list_to_atom(Ch)) end, State#server_state.channels),
    {reply, ok, State#server_state{channels = []}}.


%Try to join channel
handle_channel(CSt, {join, Client}) ->
    %Checks if the user already is in the channel
    case lists:member(Client, CSt#channel_state.members) of
        true  -> {reply, {error, user_already_joined, "User already joined"}, CSt};
        false -> {reply, ok, add_member(Client, CSt)}
    end;

%Try to leave channel
handle_channel(CSt, {leave, Client}) ->
    %Checks if the user is in the channel
    case lists:member(Client, CSt#channel_state.members) of
        false -> {reply, {error, user_not_joined, "User not in channel"}, CSt};
        true  -> {reply, ok, remove_member(Client, CSt)}
    end;

%Send message to users in a channel
handle_channel(CSt, {message_send, Msg, Nick, Sender}) ->
    Members = CSt#channel_state.members,
    Channel = CSt#channel_state.name,
    %Checks if user is in the channel
    case lists:member(Sender, Members) of
        false ->
            {reply, {error, user_not_joined, "User is not in the channel."}, CSt};
        true ->
            broadcast_message(Channel, Nick, Msg, Sender, Members),
            {reply, ok, CSt}
    end.

%Help functions

%Adds nick to list if not already there
ensure_nick(Nick, Client, NickList) ->
    %Checks if nick is in the list
    case lists:keymember(Nick, 1, NickList) of
        true -> NickList;
        false -> NickList ++ [{Nick, Client}]
    end.
%Removes nick from list when a user changes nick
remove_old_nick(Client, NickList) ->
    %Check if nick is in the list
    case lists:keyfind(Client, 2, NickList) of
        {OldNick, _} -> lists:keydelete(OldNick, 1, NickList);
        false -> NickList
    end.

%Adds user to member list when joining a channel
add_member(Client, CSt) ->
    CSt#channel_state{members = CSt#channel_state.members ++ [Client]}.

%Remove user from member list when leaving a channel
remove_member(Client, CSt) ->
    CSt#channel_state{members = lists:delete(Client, CSt#channel_state.members)}.

% Sends a message to all channel members except the sender
broadcast_message(Channel, Nick, Msg, Sender, Members) ->
    [spawn(genserver, request, [Recipient, {message_receive, Channel, Nick, Msg}])
        || Recipient <- Members, Recipient =/= Sender].

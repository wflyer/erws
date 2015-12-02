-module(erws_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).
-export([init/3, handle/2, terminate/3]).
-export([
    websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3
]).

-record(state, {
        username,
        room_pid
}).

init({tcp, http}, Req, Opts) ->
    {upgrade, protocol, cowboy_websocket}.


handle(Req, State) ->
    lager:debug("Request not expected: ~p", [Req]),
    {ok, Req2} = cowboy_http_req:reply(404, [{'Content-Type', <<"text/html">>}]),
    {ok, Req2, State}.


websocket_init(_TransportName, Req, Opts) ->
    lager:notice("Init new handler ~p", [self()]),
    {ok, Req, #state{}}.

websocket_handle({text, <<"/login ", Username/binary>>}, Req, State) ->
    case State#state.username of
        undefined ->
            lager:info("Login as ~p", [Username]),
            NewState = State#state{username=Username},
            {reply, {text, << "Hello,", Username/binary >>}, Req, NewState, hibernate };
        OldName ->
            lager:info("Username changed ~p -> ~p", [OldName, Username]),
            NewState = State#state{username=Username},
            {reply, {text, << "Your name changed to ", Username/binary >>}, Req, NewState, hibernate }
    end;

websocket_handle({text, _Msg}, Req, State=#state{username=undefined}) ->
    {reply, {text, << "Login required!! ex. /login flyer" >>}, Req, State, hibernate };

websocket_handle({text, <<"/new ", RoomIdBin/binary>>}, Req, State) ->
    lager:notice("new request for ~p", [RoomIdBin]),
    Return = case gen_server:call(erws_mgr, {create_room, RoomIdBin}) of
        ok ->
            << "Created! To join, enter /join ", RoomIdBin/binary>>;
        {error, already_exists} ->
            << "Room with same name already exists! To join, enter /join ", RoomIdBin/binary>>
    end,
    {reply, {text, Return}, Req, State, hibernate };

websocket_handle({text, <<"/join ", RoomIdBin/binary>>}, Req, State) ->
    try
        RoomPid = get_room_pid(RoomIdBin),
        join_room(RoomPid, State#state.username),
        NewState = State#state{room_pid=RoomPid},
        Return = << "Joined room! To leave, enter /leave">>,
        {reply, {text, Return}, Req, NewState, hibernate }
    catch
        already_exists ->
            ErrReturn = << "Already entered in the room">>,
            {reply, {text, ErrReturn}, Req, State, hibernate };
        not_found ->
            ErrReturn = << "Room not found">>,
            {reply, {text, ErrReturn}, Req, State, hibernate };
        Exception ->
            ErrReturn = << "Error :( Try again.">>,
            lager:error("Failed to join room ~p", [Exception]),
            {reply, {text, ErrReturn}, Req, State, hibernate }
    end;

websocket_handle({text, <<"/", _/binary>>}, Req, State) ->
    ErrReturn = << "Not supported command.">>,
    {reply, {text, ErrReturn}, Req, State, hibernate };

websocket_handle({text, Msg}, Req, State) ->
    case State#state.room_pid of
        undefined ->
            Return = << "To send msg, please join any channel."
                        " To check room list, enter /list">>,
            {reply, {text, Return}, Req, State, hibernate };
        Pid ->
            gen_server:cast(Pid, {send_msg, Msg, State#state.username}),
            {ok, Req, State, hibernate}
    end;

websocket_handle(_Any, Req, State) ->
    {reply, {text, << "whut?">>}, Req, State, hibernate }.

websocket_info({recv_msg, Msg, FromRoomIdBin, FromUsername}, Req, State) ->
    lager:info("Got message ~p from room ~p", [Msg, FromRoomIdBin]),
    Return = <<"#", FromRoomIdBin/binary, " ", FromUsername/binary, " >> ", Msg/binary>>,
    {reply, {text, Return}, Req, State, hibernate };

websocket_info({timeout, _Ref, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};

websocket_info(_Info, Req, State) ->
    lager:debug("websocket info"),
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

terminate(_Reason, _Req, _State) ->
    ok.

get_room_pid(RoomIdBin) ->
    case gen_server:call(erws_mgr, {get_room_pid, RoomIdBin}) of
        {ok, RoomPid} -> RoomPid;
        {error, not_found} -> throw(not_found), ok;
        Error -> throw(Error), ok
    end.

join_room(RoomPid, Username) ->
    case gen_server:call(RoomPid, {join, Username}) of
        ok -> ok;
        {error, already_exists} -> throw(already_exists);
        Error -> throw(Error)
    end.

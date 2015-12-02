-module(erws_room).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
        room_id,
        user_info_dict=dict:new()
}).

-record(user_info, {
        pid
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(RoomId) ->
    gen_server:start_link(?MODULE, [RoomId], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([RoomId]) ->
    lager:notice("Start new room ~p", [RoomId]),
    State = #state{room_id=RoomId},
    {ok, State}.

handle_call({join, Username}, {FromPid, _}, State) ->
    lager:notice("Join request from ~p pid ~p", [Username, FromPid]),
    case dict:is_key(Username, State#state.user_info_dict) of
        true ->
            {reply, {error, already_exists}, State};
        false ->
            UserInfo = #user_info{pid=FromPid},
            NewDict = dict:store(Username, UserInfo, State#state.user_info_dict),
            NewState = State#state{user_info_dict=NewDict},
            {reply, ok, NewState}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({send_msg, Msg, FromUsername}, State) ->
    lager:info("send_msg ~p ~p", [Msg, FromUsername]),
    dict:map(
        fun(Username, UserInfo) ->
            lager:info("Username: ~p, UserInfo: ~p", [Username, UserInfo]),
            lager:info("user_info: ~p", [UserInfo]),
            lager:info("Send msg to ~p", [UserInfo#user_info.pid]),
            erlang:send(UserInfo#user_info.pid, 
                        {recv_msg, Msg, State#state.room_id, FromUsername}),
            ok
        end,
        State#state.user_info_dict),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


-module(erws_mgr).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
        room_info_tab
}).

-record(room_info, {
        id,
        pid
}).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    lager:info(">>>> Room Mgr initialized"),

    Table = ets:new(room_info_tab, [{keypos, 2}, named_table]),

    %% Create ETS Table to store room actors
    {ok, #state{room_info_tab=Table}}.

handle_call({create_room, RoomIdBin}, _From, State) ->
    %% Check if it already exists
    case ets:member(room_info_tab, RoomIdBin) of
        true ->
            {reply, {error, already_exists}, State};
        false ->
            %% Spawn new room process
            case erws_room:start_link(RoomIdBin) of
                %% Successfully spawned
                {ok, Pid} ->
                    NewRoom = #room_info{
                            id=RoomIdBin,
                            pid=Pid
                    },
                    ets:insert(room_info_tab, NewRoom),
                    {reply, ok, State};
                {error, Reason} ->
                    lager:error("Failed to create new room ~p (~p)", [RoomIdBin, Reason]),
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({get_room_pid, RoomIdBin}, _From, State) ->
    case ets:match(room_info_tab, #room_info{id=RoomIdBin, pid='$1', _='_'}) of
        [[RoomPid]] ->
            {reply, {ok, RoomPid}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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


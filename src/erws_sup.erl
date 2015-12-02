
-module(erws_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% child_spec() = #{id => child_id(),
%%                  start => mfargs(),
%%                  restart => restart(),
%%                  shutdown => shutdown(),
%%                  type => worker(),
%%                  modules => modules()}

init([]) ->
    ChildSpec = #{
        id => erws_mgr,
        start => {erws_mgr, start_link, []},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker},
    {ok, { {one_for_one, 5, 10}, [ChildSpec]} }.

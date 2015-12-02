-module(erws_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, erws, "index.html"}},
            {"/websocket", erws_handler, []}
        ]}
    ]),
    WebPort = application:get_env(erws, web_port, 10100),

    lager:error("Start server with port ~p", [WebPort]),
    {ok, _} = cowboy:start_http(
        http, 1, [{port, WebPort}], [{env, [{dispatch, Dispatch}, {hello, hi}]}]),
    erws_sup:start_link().

stop(_State) ->
    ok.

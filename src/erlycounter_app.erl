-module(erlycounter_app).

-behaviour(application).

%% application callbacks
-export([start/0, start/2, stop/1]).

start() ->
    start(normal, []).

-define(ECSERVER_COUNT, 5).
-define(ECTABLE_COUNT, 5).
start(_Type, StartArgs) ->
	{ok, _Pid_ectable_sup} = ectable_sup:start_link(self(), ?ECTABLE_COUNT, 30000, <<"ls">>),
	{ok, _Pid_ecserver_sup} = ecserver_sup:start_link(self(), ?ECSERVER_COUNT, ?ECTABLE_COUNT),
	{ok, _Pid_udp_server} = udp_server_sup:start_link(self(), 29829, 29830).


%%----------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%%----------------------------------------------------------------------
stop(_State) ->
    ok.


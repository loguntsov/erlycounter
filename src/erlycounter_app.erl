-module(erlycounter_app).

-behaviour(application).

%% application callbacks
-export([start/0, start/2, stop/1]).

start() ->
    start(normal, []).

-define(ECSERVER_COUNT, 5).
-define(ECTABLE_COUNT, 5).
start(_Type, _StartArgs) ->
	application:start(gproc),
	error_logger:logfile({open, "erlycounter.log"}),
	error_logger:tty(true),

	Pid = spawn_link(fun() ->
		error_logger:info_report({start_ectable, ectable_sup:start_link(self(), ?ECTABLE_COUNT, 10000, <<"cat > 1.log">>)}),
		error_logger:info_report({start_ecserver, ecserver_sup:start_link(self(), ?ECSERVER_COUNT, ?ECTABLE_COUNT)}),
		error_logger:info_report({start_udp_server, udp_server_sup:start_link(self(), ?ECSERVER_COUNT, 29829, 29830)}),
		loop()
	end),
	{ok, Pid}
.

%%----------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%%----------------------------------------------------------------------
stop(_State) ->
    ok.

loop() ->
	receive
		_ -> ?MODULE:loop()
	end
.




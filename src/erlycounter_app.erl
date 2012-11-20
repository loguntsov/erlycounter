-module(erlycounter_app).

-behaviour(application).

%% application callbacks
-export([start/0, start/2, stop/1]).

-behaviour(supervisor).

-export([init/1]).

-define(ECSERVER_COUNT, 5).
-define(ECTABLE_COUNT, 5).

start() ->
	lager:start(),
	application:start(gproc),
	error_logger:logfile({open, 'erlycounter.log'}),
    error_logger:tty(true),
%	dbg:tracer(),
%	dbg:p(new, [m, c, sos]),
	ok = application:start(erlycounter),
	error_logger:info_report({application_pid,self()}),
	ok
.

start(_Type, _StartArgs) ->
	{ok, [Options]} = file:consult("erlycounter.conf"),
	io:format("~w",[Options]),
	{ok, Pid} = supervisor:start_link(?MODULE, Options),
	error_logger:info_report({application_supervisor,Pid}),
	{ok, Pid}.

%%----------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%%----------------------------------------------------------------------
stop(State) ->
	error_logger:info_report({shutdown, State}),
    ok.

init(Options) ->
	{tables, Tables} = proplists:lookup(tables, Options), % integer
	{servers, Servers} = proplists:lookup(servers, Options), % integer
	{command, Command} = proplists:lookup(command, Options), % binary
	{port_range, Port_range} = proplists:lookup(port_range, Options), % {port_from, port_to}
	{save_process, Save_process} = proplists:lookup(save_process, Options), %integer
	{save_time, Save_time} = proplists:lookup(save_time, Options), %integer
	{ ok, {{one_for_one, 5, 1000}, [
		{ecsaver_pool, { ecsaver, pool_start, [ Save_process ] }, permanent, 5000 , supervisor, [] },
		{ectable_sup, { ectable_sup, start_link, [ server_id, Tables, Save_time*1000, Command ] }, permanent, 5000 , supervisor, [] },
		{ecserver_sup, { ecserver_sup, start_link, [ server_id, Servers, Tables ] }, permanent, 5000 , supervisor, dynamic },
		{udp_server_sup, { udp_server_sup, start_link, [ server_id, Servers, element(1,Port_range), element(2, Port_range)] }, permanent, 5000 , supervisor, dynamic }
	]}}.


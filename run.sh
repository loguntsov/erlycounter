#!/bin/sh
rebar compile
erl -pa ebin -pa deps/*/ebin \
	-eval "io:format(\"~nVKTema redirect by UID~n\")."

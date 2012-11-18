#!/bin/sh
erl -sname erlycounter_app -pa ebin -pa deps/*/ebin -s erlycounter_app \
	-eval "io:format(\"~nVKTema redirect by UID~n\")."


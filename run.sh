#!/bin/sh
rebar clean
rebar compile
erl -pa ebin deps/*/ebin 

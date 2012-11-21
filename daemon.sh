#!/bin/bash
run_erl -daemon /tmp/ log/ "erl -sname erlycounter -pa ebin deps/*/ebin -s erlycounter_app"

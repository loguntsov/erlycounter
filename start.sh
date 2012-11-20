#!/bin/sh
erl -sname erlycounter -pa ebin deps/*/ebin -s erlycounter_app start



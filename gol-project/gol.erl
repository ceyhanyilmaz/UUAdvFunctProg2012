-module(gol).
-export([init/0, gol/0]).

init() ->
	frame:init(),
	frame:set_head("Conway's Game of Life in Erlang"),
	frame:set_foot("By Jonatan and Anders"),
	frame:set_w(100),
	frame:set_h(100),
    spawn_link(gol,gol,[]).

gol() ->
    X = random:uniform(20),
    Y = random:uniform(10),
    frame ! {change_cell, X, Y, purple},
    receive 
        after 100 ->
                gol:gol()
        end.
    
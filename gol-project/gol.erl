-module(gol).

-export([init/0, gol/0]).

init() ->
    spawn_link(gol,gol,[]).

gol() ->
    X = random:uniform(20),
    Y = random:uniform(10),
    frame ! {change_cell, X, Y, purple},
    receive 
        after 100 ->
                gol:gol()
        end.
    
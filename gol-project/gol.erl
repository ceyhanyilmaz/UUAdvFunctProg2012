-module(gol).
-export([gol/3,timer/1,cell/6]).

% Main program
gol(W,H,Alive) ->
	frame:init(),
	frame:set_head("Conway's Game of Life in Erlang"),
	frame:set_foot("By Jonatan and Anders"),
	frame:set_w(W),
	frame:set_h(H),

	Board = create_board(W,H),
	init_board(W,H,Board),
	raiseTheDead(W,Alive,Board),

	spawn(?MODULE,timer,[Board]).

% Set cells to be alive from the beginning
raiseTheDead(_,[],_) -> ok;
raiseTheDead(W,[{X,Y}|T],Board) ->
	lists:nth(Y*W+X,Board) ! {set_state,alive},
	raiseTheDead(W,T,Board).

% Process to signal cells at a specific interval
timer(Board) ->
	receive
		after 100 -> tic(Board)
	end,
	timer(Board).

% Send tic to all cells
tic([]) -> ok;
tic([H|T]) -> H ! {tic}, tic(T).

% Create a new board with W*H cells
create_board(W,H) -> create_board_aux(W,H,0,[]).

% Helper function for create_board
create_board_aux(W,H,I,Board) when W*H == I -> Board;
create_board_aux(W,H,I,Board) ->
	[create_cell(I rem W,I div W) | create_board_aux(W,H,I+1,Board)].

% Initialize all cells to have neighbours
init_board(W,H,Board) -> init_board_aux(W,H,0,Board,Board).

% Helper function for init_board
init_board_aux(_,_,_,[],_) -> ok;
init_board_aux(W,H,I,[Pid|Rest],Board) ->
	init_cell(Pid, 
		[lists:nth(1+mod(I-W-1, W*H),Board),
		 lists:nth(1+mod(I-W, W*H),Board),
		 lists:nth(1+mod(I-W+1, W*H),Board),
		 lists:nth(1+mod(I-1, W*H),Board),
		 lists:nth(1+mod(I+1, W*H),Board),
		 lists:nth(1+mod(I+W-1, W*H),Board),
		 lists:nth(1+mod(I+W, W*H),Board),
		 lists:nth(1+mod(I+W+1, W*H),Board)]),
	init_board_aux(W,H,I+1,Rest,Board).

% Modulo function that always return a non-negative value
mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 -> Y + X rem Y;
mod(0,_) -> 0.


% Spawn a new cell and return its PID
create_cell(X,Y) ->
	spawn(?MODULE,cell,[X,Y,dead,[],0,0]).

% Initialize a cell with new neighbours
init_cell(Pid, Neighbours) -> Pid ! {init,Neighbours}.

% Cell process 
cell(X,Y,State,Neighbours,NrLiving,NrReceived) ->
	receive
		{init,NewNeighbours} ->
			cell(X,Y,State,NewNeighbours,NrLiving,NrReceived);
		{set_state,NewState} ->
			cell(X,Y,NewState,Neighbours,NrLiving,NrReceived);
		{tic} -> send_state(State,Neighbours),
				 cell(X,Y,State,Neighbours,NrLiving,NrReceived);
		{state,dead} ->
			case NrReceived of
				7 -> cell(X,Y,update_cell(X,Y,State,NrLiving),Neighbours,0,0);
				_ -> cell(X,Y,State,Neighbours,NrLiving,NrReceived+1)
			end;
		{state,alive} ->
			case NrReceived of
				7 -> cell(X,Y,update_cell(X,Y,State,NrLiving+1),Neighbours,0,0);
				_ -> cell(X,Y,State,Neighbours,NrLiving+1,NrReceived+1)
			end
	end.

% Send current state to all neighbours
send_state(_,[]) -> ok;
send_state(State,[H|T]) -> H ! {state,State}, send_state(State,T).

% Update the state of a cell and draw the result
update_cell(X,Y,alive,NrLiving) when NrLiving < 2 ->
	frame ! {change_cell,X,Y,black},
	dead;
update_cell(X,Y,alive,NrLiving) when NrLiving < 4 ->
	frame ! {change_cell,X,Y,purple},
	alive;
update_cell(X,Y,alive,_) ->
	frame ! {change_cell,X,Y,black},
	dead;
update_cell(X,Y,dead,3) ->
	frame ! {change_cell,X,Y,purple},
	alive;
update_cell(X,Y,dead,_) ->
	frame ! {change_cell,X,Y,black},
	dead.

% Compile:
% c(ehtml). c(frame). c(gol).

% TEST CASES
% Glider
% gol:gol(40,40,[{5,4},{3,5},{5,5},{4,6},{5,6}]).
%
% Lightweight Spaceship
% gol:gol(40,40,[{6,6},{7,6},{4,7},{5,7},{7,7},{8,7},{4,8},
%	{5,8},{6,8},{7,8},{5,9},{6,9}]).
%
% Pulsar
% gol:gol(40,40,[{4,1},{5,1},{6,1},{10,1},{11,1},{12,1},{2,3},{7,3},{9,3},
% {14,3},{2,4},{7,4},{9,4},{14,4},{2,5},{7,5},{9,5},{14,5},{4,6},{5,6},
% {6,6},{10,6},{11,6},{12,6},{4,8},{5,8},{6,8},{10,8},{11,8},{12,8},{2,9},
% {7,9},{9,9},{14,9},{2,10},{7,10},{9,10},{14,10},{2,11},{7,11},{9,11},
% {14,11},{4,13},{5,13},{6,13},{10,13},{11,13},{12,13}]).
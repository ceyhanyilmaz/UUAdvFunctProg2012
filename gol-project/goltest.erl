-module(goltest).
-compile(export_all).

gol(W,H) ->
	frame:init(),
	frame:set_head("Conway's Game of Life in Erlang"),
	frame:set_foot("By Jonatan and Anders"),
	frame:set_w(W),
	frame:set_h(H),
	DrawPid = spawn_link(?MODULE,draw,[]),

	Board = create_board(W,H,DrawPid),
	init_board(W,H,Board),

	spawn(?MODULE,timer,[Board]).


draw() -> 
	receive
		{draw,X,Y,dead} -> frame ! {change_cell, X, Y, purple};
		{draw,X,Y,alive} ->	frame ! {change_cell, X, Y, black}
	end,
	draw().

timer(Board) ->
	receive
		after 100 -> tic(Board)
	end,
	timer(Board).

tic([]) -> ok;
tic([H|T]) -> H ! {tic}, tic(T).


create_board(W,H,DrawPid) -> create_board_aux(W,H,0,[],DrawPid).

create_board_aux(W,H,I,Board,_) when W*H == I -> Board;
create_board_aux(W,H,I,Board,DrawPid) ->
	[create_cell(I rem W,I div W,DrawPid) | create_board_aux(W,H,I+1,Board,DrawPid)].

init_board(W,H,Board) -> init_board_aux(W,H,0,Board,Board).

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

mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 -> Y + X rem Y;
mod(0,_) -> 0.


% cell interface
create_cell(X,Y,DrawPid) ->
	spawn(?MODULE,cell,[X,Y,dead,[],0,0,DrawPid]).

init_cell(Pid, Neighbours) -> Pid ! {init,Neighbours}.

cell(X,Y,State,Neighbours,NrLiving,NrReceived,DrawPid) ->
	receive
		{init,NewNeighbours} ->
			cell(X,Y,State,NewNeighbours,NrLiving,NrReceived,DrawPid);
		{set_state,NewState} ->
			cell(X,Y,NewState,Neighbours,NrLiving,NrReceived,DrawPid);
		{tic} -> send_state(State,Neighbours),
				 cell(X,Y,State,Neighbours,NrLiving,NrReceived,DrawPid);
		{state,dead} ->
			case NrReceived of
				7 -> cell(X,Y,update_cell(X,Y,State,NrLiving,DrawPid),Neighbours,0,0,DrawPid);
				_ -> cell(X,Y,State,Neighbours,NrLiving,NrReceived+1,DrawPid)
			end;
		{state,alive} ->
			case NrReceived of
				7 -> cell(X,Y,update_cell(X,Y,State,NrLiving+1,DrawPid),Neighbours,0,0,DrawPid);
				_ -> cell(X,Y,State,Neighbours,NrLiving+1,NrReceived+1,DrawPid)
			end
	end.

send_state(_,[]) -> ok;
send_state(State,[H|T]) -> H ! {state,State}, send_state(State,T).

update_cell(X,Y,alive,NrLiving,DrawPid) when NrLiving < 2 -> DrawPid ! {X,Y,dead},dead;
update_cell(X,Y,alive,NrLiving,DrawPid) when NrLiving < 4 -> DrawPid ! {X,Y,alive},alive;
update_cell(X,Y,alive,_,DrawPid) -> DrawPid ! {X,Y,dead},dead;
update_cell(X,Y,dead,3,DrawPid) -> DrawPid ! {X,Y,alive},alive;
update_cell(X,Y,dead,_,DrawPid) -> DrawPid ! {X,Y,dead},dead.

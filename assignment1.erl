% Assignment 1
% Anders Hassis
% Jonatan Jansson
%
-module(assignment1).

-export([dividers_of/1, primes_up_to/1]).
%-export([fibonacci_tree_aux/1]).
-export([fibonacci_tree/1]).
%-export([factorize_initial_state/0, factorize/2, factorize_dispose_state/1]).


% Part 1: a - dividers_of
-spec dividers_of(integer()) -> [integer()].
dividers_of(N) when N =< 1 -> [];
dividers_of(N) -> [X || X <- lists:seq(2,N-1), N rem X == 0].

%assignment1:dividers_of(20).% == [2,4,5,10].

% Part 1: b - primes_up_to
-spec primes_up_to(integer()) -> [integer()].
primes_up_to(N) when N =< 1 -> [];
primes_up_to(N) -> [X || X <- lists:seq(2,N), dividers_of(X) ==  []].


% Part 2: fibonacci_tree
-spec fibonacci_tree_aux(integer()) -> integer().

fibonacci_tree_aux(N) when N =< 1 -> 
	receive 
		{From, sum} ->
			From ! {self(), 1}
	end;
fibonacci_tree_aux(N) -> 
	PIDleft = spawn(?MODULE, fibonacci_tree_aux, [N-2]),
	PIDright = spawn(?MODULE, fibonacci_tree_aux, [N-1]),

	receive
		{From, sum} ->
			PIDleft ! {self(), sum},
			PIDright ! {self(), sum},

			receive 
				{PIDleft, SumLeft} -> ok
			end,

			receive 
				{PIDright, SumRight} -> ok
			end,
			From ! {self(), SumLeft+SumRight+1}
	end.
	
-spec fibonacci_tree(integer()) -> integer().
fibonacci_tree(N) when N =< 1 -> 1;
fibonacci_tree(N) -> 
	PIDleft = spawn(?MODULE, fibonacci_tree_aux, [N-2]),
	PIDright = spawn(?MODULE, fibonacci_tree_aux, [N-1]),

	PIDleft ! {self(), sum},
	PIDright ! {self(), sum},

	receive 
		{PIDleft, SumLeft} -> ok
	end,

	receive 
		{PIDright, SumRight} -> ok
	end,
	SumLeft+SumRight+1.

%fibonacci_tree(3) -> 5;
%fibonacci_tree(4) -> 9.

%-spec factorize_initial_state() -> State :: term().
%
%-spec factorize(integer(), State :: term()) ->
%    {[integer()], NewState :: term()}.
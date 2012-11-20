% Assignment 1
% Anders Hassis
% Jonatan Jansson
%
-module(assignment1).

-export([dividers_of/1, primes_up_to/1]).
-export([fibonacci_tree_aux/1, test_fibonacci/0]).
-export([fibonacci_tree/1]).
-export([factorize_initial_state/0, factorize/2]).%, factorize_dispose_state/1]).
-export([prime_tree/3]).

-include_lib("eunit/include/eunit.hrl").


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
	Root = spawn(?MODULE, fibonacci_tree_aux, [N]),
	Root ! {self(), sum},

	receive 
		{Root, SumLeft} -> SumLeft
	end.

test_fibonacci() ->
	?assert(fibonacci_tree(10) =:= 177),
	?assert(fibonacci_tree(15) =:= 1973).

%fibonacci_tree(3) -> 5;
%fibonacci_tree(4) -> 9.

prime_tree(N, Known_primes, Parent) ->
	case dict:find(N, Known_primes) of
		{ok, [Result | _]} ->
			{Result, Known_primes};
		_ ->
			Divs = [X || X <- lists:seq(N div 2, 2, -1), N rem X == 0],
			case Divs of
				[] -> Parent ! {[N], dict:append(N, [N], Known_primes)};
				[H | _] ->
					spawn(?MODULE, prime_tree, [H, Known_primes, self()]),
					spawn(?MODULE, prime_tree, [N div H, Known_primes, self()]),
					receive
						{Fact1, Known1} -> ok
					end,
					receive
						{Fact2, Known2} -> ok
					end,
					Extended_known = dict:merge(fun(_, _, V) -> V end, 
						Known1, Known2),
					Factors = Fact1 ++ Fact2,
					Parent ! {Factors, dict:append(N, Factors, Extended_known)}
			end
	end.

-spec factorize_initial_state() -> State :: term().

factorize_initial_state() -> dict:new().

-spec factorize(integer(), State :: term()) ->
    {[integer()], NewState :: term()}.


factorize(N, States) -> 
	case dict:find(N, States) of
		{ok, [Result | _]} -> {Result, States};
		_ ->
			spawn(?MODULE, prime_tree, [N, States, self()]),
			receive
				Result -> Result
			end
	end.
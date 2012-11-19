% Assignment 1
% Anders Hassis
% Jonatan Jansson
%
-module(assignment1).

-export([dividers_of/1]).
%-export([dividers_of/1, primes_up_to/1]).
%-export([fibonacci_tree/1]).
%-export([factorize_initial_state/0, factorize/2, factorize_dispose_state/1]).

-spec dividers_of(integer()) -> [integer()].

dividers_aux(N,N) -> [];
dividers_aux(I,N) when N rem I == 0 -> [I | dividers_aux(I+1,N)];
dividers_aux(I,N) -> dividers_aux(I+1,N).

dividers_of(N) -> dividers_aux(2,N).

%assignment1:dividers_of(20).% == [2,4,5,10].

%-spec primes_up_to(integer()) -> [integer()].
%primes_up_to(10) -> [2,3,5,7].

%fibonacci_tree(3) -> 5;
%fibonacci_tree(4) -> 9.

%-spec factorize_initial_state() -> State :: term().
%
%-spec factorize(integer(), State :: term()) ->
%    {[integer()], NewState :: term()}.
%%
% <module> Prolog linter
%
% @author Douglas S. Green
% @license GPL

:- module(prolog_linter, [
        check/1
    ]
).
:- use_module(prolog_lexer).
:- use_module(prolog_operators).

% todo: Check if predicates are already defined by the language

%! check(+Tokens:list) is det
% Check the entire list of tokens.
check([upper(Upper)|Tokens]) :-
    atom_length(Upper, Len),
    Len < 3,
	format("Variable name too short: ~w\n", [Upper]),
	check(Tokens).
check([Token|Tokens]) :-
	check(Tokens).
check([]).

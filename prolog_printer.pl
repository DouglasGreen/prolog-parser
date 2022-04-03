%%
% <module> Prolog printer
%
% @author Douglas S. Green
% @license GPL

:- module(prolog_printer, [
        pretty_print/1
    ]
).

:- use_module(prolog_parser).
:- use_module(prolog_lexer).

pretty_print(file(Sections)) :-
    print_sections(Sections).

print_atom(single_quoted(Atom)) :-
    writeq(Atom).
/*
print_clause(fact(head(Fact))) :-
    print_fact(Fact).
*/
print_clause(directive(body(Terms))) :-
    write(':- '),
    print_terms_indented(Terms, 0),
    writeln('.').
print_clause(X) :-
    writeq(X),
    nl.

print_clauses([Clause|Clauses]) :-
    print_clause(Clause),
    print_clauses(Clauses).
print_clauses([]).

print_comment(comment(line, Line)) :-
    format('%~w\n', Line).
print_comment(comment(block, Block)) :-
    format('/*~w*/\n', Block).

print_comments([Comment|Comments]) :-
    print_comment(Comment),
    print_comments(Comments).
print_comments([]).

print_fact(atom(Atom)) :-
    print_atom(Atom),
    writeln(".\n").

print_section(clauses(Clauses)) :-
	print_clauses(Clauses).
print_section(comments(Comments)) :-
	print_comments(Comments).

print_sections([Section|Sections]) :-
    print_section(Section),
	print_sections(Sections).
print_sections([]).

print_terms_indented([Term], Indent) :-
    print_term_indented(Term, Indent),
    !.
print_terms_indented([Term|Terms], Indent) :-
    print_term_indented(Term, Indent),
    writeln(','),
	print_terms_indented(Terms, Indent).
print_terms_indented([], _).

print_terms_spaced([Term]) :-
    print_term_spaced(Term),
    !.
print_terms_spaced([Term|Terms]) :-
    print_term_spaced(Term),
    write(', '),
	print_terms_spaced(Terms).
print_terms_spaced([], _).

print_term_indented(term(Term), Indent) :-
    print_indent(Indent),
    (
        print_compound(Term), !;
        writeq(Term)
    ).
print_term_spaced(term(Term)) :-
    (
        print_compound(Term), !;
        writeq(Term)
    ).

print_compound(compound(name(Name), terms(Terms))) :-
    write(Name),
    write('('),
    print_terms_spaced(Terms, false),
    write(')').

print_indent(Indent) :-
    Indent > 0,
    write('    '),
    IndentMinus1 is Indent - 1,
    print_indent(IndentMinus1).
print_indent(0).

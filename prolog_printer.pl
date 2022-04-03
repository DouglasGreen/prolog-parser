%%
% <module> Prolog printer
%
% @author Douglas S. Green
% @license GPL

:- module(prolog_printer, [
        print_file/1
    ]
).

:- use_module(prolog_parser).
:- use_module(prolog_lexer).

print_file(file(Sections)) :-
    print_sections(Sections).

print_clause(clause(fact, Head)) :-
	print_head(Head),
    writeln('.'),
    !.
print_clause(clause(rule, Head, body(Terms))) :-
	print_head(Head),
    writeln(' :-'),
    print_terms_indented(Terms, 1),
    writeln('.'),
    !.
print_clause(clause(dcg, Head, body(Terms))) :-
	print_head(Head),
    writeln(' -->'),
    print_terms_indented(Terms, 1),
    writeln('.'),
    !.
print_clause(clause(directive, body(Terms))) :-
    write(':- '),
    print_terms_indented(Terms, 0),
    writeln('.'),
    !.
print_clause(X) :-
    write('Unknown: '),
    print_term(X, [indent_arguments(true)]),
    nl.

print_clauses([Clause|Clauses]) :-
    print_clause(Clause),
    print_clauses(Clauses).
print_clauses([]).

print_codestring(codestring(Codestring)) :-
	format("`~w`", [Codestring]).

print_comment(comment(line, Line), Indent) :-
	print_indent(Indent),
    format('%~w\n', [Line]).
print_comment(comment(block, Block), Indent) :-
	print_indent(Indent),
    format('/*~w*/\n', [Block]).

print_comments([Comment|Comments], Indent) :-
    print_comment(Comment, Indent),
    print_comments(Comments, Indent).
print_comments([], _).

print_head(head(Compound)) :-
    print_compound(Compound).
print_head(head(Head)) :-
    print_atom(Head).

print_section(section(Comments, Clauses)) :-
	print_comments(Comments, 0),
	print_clauses(Clauses).

print_sections([Section]) :-
    print_section(Section),
    !.
print_sections([Section|Sections]) :-
    print_section(Section),
    nl,
	print_sections(Sections).
print_sections([]).

print_string(string(String)) :-
	format("\"~w\"", [String]).

print_term_indented(Term, Indent) :-
    print_indent(Indent),
    (
        print_a_term(Term)
    ).

print_term_spaced(Term) :-
    (
        print_a_term(Term)
    ).

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

print_value(value(_, Value)) :-
	write(Value).

print_a_term(term(Atom)) :-
    print_atom(Atom).
print_a_term(term(Value)) :-
    print_value(Value).
print_a_term(term(Codestring)) :-
    print_codestring(Codestring).
print_a_term(term(String)) :-
    print_string(String).
print_a_term(term(Compound)) :-
    print_compound(Compound).

print_compound(compound(Name, Terms)) :-
    print_atom(Name),
    write('('),
    print_terms_spaced(Terms),
    write(')').

print_indent(Indent) :-
    Indent > 0,
    write('    '),
    IndentMinus1 is Indent - 1,
    print_indent(IndentMinus1).
print_indent(0).

print_atom(atom(lower, Atom)) :-
    write(Atom).
print_atom(atom(quoted, Atom)) :-
    writeq(Atom).
print_atom(atom(cut)) :-
    write('!').


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

print_a_term(term(Atom)) :-
    print_atom(Atom).
print_a_term(term(Value)) :-
    print_value(Value).
print_a_term(term(Codestring)) :-
    print_codestring(Codestring).
print_a_term(term(String)) :-
    print_string(String).
print_a_term(term(Var)) :-
    print_var(Var).
print_a_term(term(Compound)) :-
    print_compound(Compound).
print_a_term(term(Paren)) :-
    print_term_in_parens(Paren).
print_a_term(term(Brace)) :-
    print_term_in_braces(Brace).
print_a_term(term(Bracket)) :-
    print_term_in_brackets(Bracket).

print_atom(atom(lower, Atom)) :-
    write(Atom).
print_atom(atom(quoted, Atom)) :-
    atom_chars(Atom, [Char|Chars]),
    char_type(Char, lower),
    \+ (
        member(Other, Chars),
        \+ char_type(Other, csym)
    ),
    format("~w", [Atom]),
    !.
print_atom(atom(quoted, Atom)) :-
    format("'~w'", [Atom]).
print_atom(atom(cut)) :-
    write('!').

print_clause(clause(fact, Head)) :-
	print_head(Head),
    writeln('.'),
    !.
print_clause(clause(rule, Head, body(ExprList))) :-
	print_head(Head),
    writeln(' :-'),
    print_expression_list_indented(ExprList, 1),
    writeln('.'),
    !.
print_clause(clause(dcg, Head, body(ExprList))) :-
	print_head(Head),
    writeln(' -->'),
    print_expression_list_indented(ExprList, 1),
    writeln('.'),
    !.
print_clause(clause(directive, body(ExprList))) :-
    write(':- '),
    print_expression_list_indented(ExprList, 0),
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

print_compound(compound(Name, ExprList)) :-
    print_atom(Name),
    write('('),
    print_expression_list_spaced(ExprList),
    write(')').

print_head(head(Compound)) :-
    print_compound(Compound).
print_head(head(Head)) :-
    print_atom(Head).

print_indent(Indent) :-
    Indent > 0,
    write('    '),
    IndentMinus1 is Indent - 1,
    print_indent(IndentMinus1).
print_indent(0).

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

print_tail(tail(empty)) :- !.
print_tail(tail(Exprs)) :-
	write('|'),
	print_expressions(Exprs).

print_term_in_braces(brace(empty)) :-
    write('{'),
    write('}'),
	!.
print_term_in_braces(brace(ExprList)) :-
    write('{'),
    print_expression_list_spaced(ExprList),
    write('}').

print_term_in_brackets(bracket(empty)) :-
    write('['),
    write(']'),
	!.
print_term_in_brackets(bracket(ExprList, Tail)) :-
    write('['),
    print_expression_list_spaced(ExprList),
	print_tail(Tail),
    write(']').

print_term_in_parens(paren(ExprList)) :-
    write('('),
    print_expression_list_spaced(ExprList),
    write(')').

print_term_infix(Term1, operator(_, _, Name), Term2) :-
    print_a_term(Term1),
    format(" ~w ", [Name]),
    print_a_term(Term2).

print_term_prefix(operator(_, _, Name), Term) :-
    write(Name),
    write(' '),
    print_a_term(Term).

print_term_postfix(operator(_, _, Name), Term) :-
    print_a_term(Term),
    write(' '),
    write(Name).

print_expression_list_indented([Exprs], Indent) :-
    print_indent(Indent),
    print_expressions(Exprs),
    !.
print_expression_list_indented([Exprs|ExprList], Indent) :-
    print_indent(Indent),
    print_expressions(Exprs),
    writeln(','),
	print_expression_list_indented(ExprList, Indent).
print_expression_list_indented([], _).

print_expression_list_spaced([Exprs]) :-
    print_expressions(Exprs),
    !.
print_expression_list_spaced([Exprs|ExprList]) :-
    print_expressions(Exprs),
    write(', '),
	print_expression_list_spaced(ExprList).
print_expression_list_spaced([], _).

print_expressions([expression(PrefixOps, Term, PostfixOps, InfixOp)|Exprs]) :-
    print_ops_prefix(PrefixOps),
    print_a_term(Term),
    print_ops_postfix(PostfixOps),
    print_op_infix(InfixOp),
    print_expressions(Exprs).
print_expressions([expression(PrefixOps, Term, PostfixOps)]) :-
    print_ops_prefix(PrefixOps),
    print_a_term(Term),
    print_ops_postfix(PostfixOps).

print_op_infix(operator(_, _, Name)) :-
    format(" ~w ", [Name]).

print_ops_postfix([operator(_, _, Name)|PostfixOps]) :-
    format(" ~w", [Name]),
    print_ops_postfix(PostfixOps).
print_ops_postfix([]).

print_ops_prefix([operator(_, _, Name)|PrefixOps]) :-
    format("~w ", [Name]),
    print_ops_prefix(PrefixOps).
print_ops_prefix([]).

print_value(value(_, Value)) :-
	write(Value).

print_var(var(Var)) :-
    write(Var).


%%
% <module> Prolog parser
%
% Graphical tokens and user-defined operators haven't been implemented yet.
%
% Todo: Parse quoted strings
%
% Format: Each singular type is marked with a name of the type, followed by a compound. The first term of the compound
% might be a subtype and the other terms are the parsed input. Plural terms are lists of parsed input.
%
% @author Douglas S. Green
% @license GPL

:- module(prolog_parser, [
        parse_file/3
    ]
).

:- use_module(prolog_lexer).
:- use_module(prolog_operators).

parse_file(file(Sections)) -->
    parse_sections(Sections).

parse_atom(atom(lower, Atom)) -->
    [lower(Atom)].
parse_atom(atom(quoted, Atom)) -->
    [quoted(single, Atom)].
parse_atom(atom(cut)) -->
    [mark('!')].

parse_body(body(ExprList)) -->
    parse_expression_list(ExprList).

parse_clause(clause(fact, Head)) -->
    parse_head(Head),
    [mark('.')].
parse_clause(clause(rule, Head, Body)) -->
    parse_head(Head),
    [mark(':')],
    [mark('-')],
    parse_body(Body),
    [mark('.')].
parse_clause(clause(dcg, Head, Body)) -->
    parse_head(Head),
    [mark('-')],
    [mark('-')],
    [mark('>')],
    parse_body(Body),
    [mark('.')].
parse_clause(clause(directive, Body)) -->
    [mark(':')],
    [mark('-')],
    parse_body(Body),
    [mark('.')].

parse_clauses([Clause|Clauses]) -->
    parse_clause(Clause),
    parse_clauses(Clauses).
parse_clauses([]) -->
    [].

parse_codestring(codestring(Codestring)) -->
    [quoted(back, Codestring)].

parse_comment(comment(line, Line)) -->
    [comment(line, Line)].
parse_comment(comment(block, Block)) -->
    [comment(block, Block)].

parse_comments([Comment|Comments]) -->
    parse_comment(Comment),
    parse_comments(Comments).
parse_comments([]) -->
    [].

parse_compound(compound(Name, ExprList)) -->
    parse_atom(Name),
    [mark('(')],
    parse_expression_list(ExprList),
    [mark(')')].

% todo allow comments inside expr lists
parse_expression_list([Exprs|ExprList]) -->
    parse_expressions(Exprs),
    [mark(',')],
    parse_expression_list(ExprList).
parse_expression_list([Exprs]) -->
    parse_expressions(Exprs).

parse_expressions([expression(PrefixOps, Term, PostfixOps, InfixOp)|Exprs]) -->
    parse_prefix_ops(PrefixOps),
    parse_term(Term),
    parse_postfix_ops(PostfixOps),
    parse_op_infix(InfixOp),
    parse_expressions(Exprs).
parse_expressions([expression(PrefixOps, Term, PostfixOps)]) -->
    parse_prefix_ops(PrefixOps),
    parse_term(Term),
    parse_postfix_ops(PostfixOps).

parse_head(head(Head)) -->
    parse_compound(Head).
parse_head(head(Head)) -->
    parse_atom(Head).

parse_marks([Mark|Marks]) -->
    [mark(Mark)],
    parse_marks(Marks).
parse_marks([Mark]) -->
    [mark(Mark)].

parse_op_infix(operator(Predecence, Associativity, Name)) -->
    (
        [lower(Name)];
        parse_marks(Marks),
        {atomic_list_concat(Marks, Name)}
    ),
    {
        operator(Predecence, Associativity, Name),
        memberchk(Associativity, ['xfx', 'xfy', 'yfx'])
    }.

parse_op_postfix(operator(Predecence, Associativity, Name)) -->
    (
        [lower(Name)];
        parse_marks(Marks),
        {atomic_list_concat(Marks, Name)}
    ),
    {
        operator(Predecence, Associativity, Name),
        memberchk(Associativity, ['xf', 'yf'])
    }.

parse_op_prefix(operator(Predecence, Associativity, Name)) -->
    (
        [lower(Name)];
        parse_marks(Marks),
        {atomic_list_concat(Marks, Name)}
    ),
    {
        operator(Predecence, Associativity, Name),
        memberchk(Associativity, ['fx', 'fy'])
    }.

parse_ops_postfix([operator(Predecence, Associativity, Name)|PostfixOps]) -->
    parse_op_postfix(operator(Predecence, Associativity, Name)),
    parse_postfix_ops(PostfixOps).
parse_ops_postfix([]) -->
    [].

parse_ops_prefix([operator(Predecence, Associativity, Name)|PrefixOps]) -->
    parse_op_prefix(operator(Predecence, Associativity, Name)),
    parse_prefix_ops(PrefixOps).
parse_ops_prefix([]) -->
    [].

parse_section(section(Comments, Clauses)) -->
    parse_comments(Comments),
    parse_clauses(Clauses),
    {
        (
            Comments \= [];
            Clauses \= []
        )
    },
    !.

parse_sections([Section|Sections]) -->
    parse_section(Section),
    parse_sections(Sections).
parse_sections([]) -->
    [].

parse_string(string(String)) -->
    [quoted(double, String)].

parse_tail(tail(Tail)) -->
    [mark('|')],
    parse_term(Tail).
parse_tail(tail(empty)) -->
    [].

parse_term(term(Term)) -->
    parse_atom(Term).
parse_term(term(Term)) -->
    parse_value(Term).
parse_term(term(Term)) -->
    parse_codestring(Term).
parse_term(term(Term)) -->
    parse_string(Term).
parse_term(term(Term)) -->
    parse_var(Term).
parse_term(term(Term)) -->
    parse_compound(Term).
parse_term(term(Term)) -->
    parse_term_in_parens(Term).
parse_term(term(Term)) -->
    parse_term_in_braces(Term).
parse_term(term(Term)) -->
    parse_term_in_brackets(Term).

parse_term_in_braces(brace(empty)) -->
    [mark('{')],
    [mark('}')].
parse_term_in_braces(brace(ExprList)) -->
    [mark('{')],
    parse_expression_list(ExprList),
    [mark('}')].

parse_term_in_brackets(bracket(empty)) -->
    [mark('[')],
    [mark(']')].
parse_term_in_brackets(bracket(ExprList, Tail)) -->
    [mark('[')],
    parse_expression_list(ExprList),
    parse_tail(Tail),
    [mark(']')].

parse_term_in_parens(paren(ExprList)) -->
    [mark('(')],
    parse_expression_list(ExprList),
    [mark(')')].

parse_value(value(Type, Value)) -->
    [value(Type, Value)].

parse_var(var(Var)) -->
    [upper(Var)].

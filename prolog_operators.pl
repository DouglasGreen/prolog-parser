%%
% <module> Prolog operators
%
% @author Douglas S. Green
% @license GPL

:- module(prolog_operators, [
        operator/3
    ]
).

%! operator(?Predecence:int, ?Associativity:atom, ?Name:atom) is nondet
% List of operators
operator(1200, 'xfx', '-->').
operator(1200, 'xfx', ':-').
operator(1200, 'fx', ':-').
operator(1200, 'fx', '?-').
operator(1150, 'fx', 'dynamic').
operator(1150, 'fx', 'discontiguous').
operator(1150, 'fx', 'initialization').
operator(1150, 'fx', 'meta_predicate').
operator(1150, 'fx', 'module_transparent').
operator(1150, 'fx', 'multifile').
operator(1150, 'fx', 'public').
operator(1150, 'fx', 'thread_local').
operator(1150, 'fx', 'thread_initialization').
operator(1150, 'fx', 'volatile').
operator(1105, 'xfy', '|').
operator(1100, 'xfy', ';').
operator(1050, 'xfy', '->').
operator(1050, 'xfy', '*->').
operator(1000, 'xfy', ',').
operator(990, 'xfx', ':=').
operator(900, 'fy', '\\+').
operator(700, 'xfx', '<').
operator(700, 'xfx', '=').
operator(700, 'xfx', '=..').
operator(700, 'xfx', '=@=').
operator(700, 'xfx', '\\=@=').
operator(700, 'xfx', '=:=').
operator(700, 'xfx', '=<').
operator(700, 'xfx', '==').
operator(700, 'xfx', '=\\=').
operator(700, 'xfx', '>').
operator(700, 'xfx', '>=').
operator(700, 'xfx', '@<').
operator(700, 'xfx', '@=<').
operator(700, 'xfx', '@>').
operator(700, 'xfx', '@>=').
operator(700, 'xfx', '\\=').
operator(700, 'xfx', '\\==').
operator(700, 'xfx', 'as').
operator(700, 'xfx', 'is').
operator(700, 'xfx', '>:<').
operator(700, 'xfx', ':<').
operator(600, 'xfy', ':').
operator(500, 'yfx', '+').
operator(500, 'yfx', '-').
operator(500, 'yfx', '/\\').
operator(500, 'yfx', '\\/').
operator(500, 'yfx', 'xor').
operator(500, 'fx', '?').
operator(400, 'yfx', '*').
operator(400, 'yfx', '/').
operator(400, 'yfx', '//').
operator(400, 'yfx', 'div').
operator(400, 'yfx', 'rdiv').
operator(400, 'yfx', '<<').
operator(400, 'yfx', '>>').
operator(400, 'yfx', 'mod').
operator(400, 'yfx', 'rem').
operator(200, 'xfx', '**').
operator(200, 'xfy', '^').
operator(200, 'fy', '+').
operator(200, 'fy', '-').
operator(200, 'fy', '\\').
operator(100, 'yfx', '.').
operator(1, 'fx', '$').

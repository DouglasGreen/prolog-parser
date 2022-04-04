%%
% <module> Prolog lexer
%
% Every token is given a name. See token//1.
%
% See:
% https://www.swi-prolog.org/pldoc/man?section=syntax
%
% @author Douglas S. Green
% @license GPL

:- module(prolog_lexer, [
        tokens/3
    ]
).

:- use_module(library(dcg/basics)).

%! tokens(-Tokens:list) is semidet
% Match a list of all tokens excluding whitespace.

tokens([Token|Tokens]) -->
    token(Token),
    tokens(Tokens).
tokens(Tokens) -->
    blank,
    blanks,
    tokens(Tokens).
tokens([]) -->
     [].

%! base_digit(-Digit:code, -Base:int) is semidet
% Match an digit character code in a base from 2 to 10.
base_digit(Digit, Base) -->
    [Digit],
    {
        char_type(Digit, digit(Weight)),
        Weight < Base
    }.

%! base_digits(-Digits:codes) is semidet
% Match a list of one or more digit codes in a base from 2 to 10.
base_digits([Digit|Digits], Base) -->
    base_digit(Digit, Base),
    base_digits(Digits, Base).
base_digits([Digit], Base) -->
    base_digit(Digit, Base).

%! char(-Code:code) is semidet
% Match a single character code.
char(Code, Type) -->
    [Code],
    {char_type(Code, Type)}.

%! chars(-Codes:codes, -Type:atom|compound) is semidet
% Match a list of character codes.
chars([Code|Codes], Type) -->
    char(Code, Type),
    chars(Codes, Type).
chars([], _) -->
    [].

%! exponent(-ExpDigits:codes) is semidet
% Return a floating-point exponent as a list of codes.
exponent(ExpDigits) -->
    [Code1],
    {
        char_code('e', Code1);
        char_code('E', Code1)
    },
    [Code2],
    {
        char_code('+', Code2);
        char_code('-', Code2)
    },
    base_digits(Exponent, 10),
    {ExpDigits = [Code1, Code2|Exponent]}.
exponent([]) -->
    [].

%! float_digits(-FloatDigits:codes) is semidet
% Parse a floating point except the sign.
float_digits(FloatDigits) -->
    base_digits(WholeDigits, 10),
    `.`,
    base_digits(FracDigits, 10),
    exponent(ExpDigits),
    {
        char_code('.', Point),
        flatten([WholeDigits, Point, FracDigits, ExpDigits], FloatDigits)
    }.

%! hex_char(-Code:code) is semidet
% Match a hexadecimal character code.
hex_char(Code) -->
    [Code],
    {char_type(Code, xdigit(_))}.

%! hex_chars(-Codes:codes) is semidet
% Match and lowercase a list of one or more hexadecimal character codes.
hex_chars([Code|Codes]) -->
    hex_char(Code),
    hex_chars(Codes).
hex_chars([Code]) -->
    hex_char(Code).

%! quoted_chars(-Quote:atom, -Codes:codes) is semidet
% Match a list of character codes up to a the end of a quoted string.
quoted_chars(Quote, [Code|Codes]) -->
    [Code],
    {
        \+ char_code('\\', Code),
        \+ char_code(Quote, Code)
    },
    quoted_chars(Quote, Codes).
quoted_chars(Quote, [Code1, Code2|Codes]) -->
    [Code1],
    {char_code('\\', Code1)},
    [Code2],
    {char_code('x', Code2)},
    hex_chars(HexCodes),
    [Code3],
    {char_code('\\', Code3)},
    quoted_chars(Quote, Rest),
    {append(HexCodes, [Code3|Rest], Codes)}.
quoted_chars(Quote, [Code|Codes]) -->
    [Code],
    {char_code('\\', Code)},
    base_digits(Digits, 8),
    quoted_chars(Quote, Rest),
    {append(Digits, Rest, Codes)}.
quoted_chars(Quote, [Code1, Code2, Code3, Code4, Code5, Code6|Codes]) -->
    [Code1],
    {char_code('\\', Code1)},
    [Code2],
    {char_code('u', Code2)},
    hex_char(Code3),
    hex_char(Code4),
    hex_char(Code5),
    hex_char(Code6),
    quoted_chars(Quote, Codes).
quoted_chars(Quote, [Code1, Code2, Code3, Code4, Code5, Code6, Code7, Code8, Code9, Code10|Codes]) -->
    [Code1],
    {char_code('\\', Code1)},
    [Code2],
    {char_code('U', Code2)},
    hex_char(Code3),
    hex_char(Code4),
    hex_char(Code5),
    hex_char(Code6),
    hex_char(Code7),
    hex_char(Code8),
    hex_char(Code9),
    hex_char(Code10),
    quoted_chars(Quote, Codes).
quoted_chars(Quote, [Code1, Code2|Codes]) -->
    [Code1],
    {char_code('\\', Code1)},
    [Code2],
    quoted_chars(Quote, Codes).
quoted_chars(Quote, [Code, Code|Codes]) -->
    [Code],
    {char_code(Quote, Code)},
    [Code],
    quoted_chars(Quote, Codes).
quoted_chars(Quote, []) -->
    [Code],
    {char_code(Quote, Code)}.

%! token(-Token:compound) is semidet
% Match a single token.
token(comment(line, Comment)) -->
    `%`,
	string_without(`\n`, Codes),
    !,
    {atom_chars(Comment, Codes)}.
token(comment(block, Comment)) -->
    `/`,
    `*`,
	string(Codes),
    `*`,
    `/`,
    !,
    {atom_chars(Comment, Codes)}.
token(quoted(back, String)) -->
    [Code],
    {char_code('`', Code)},
	quoted_chars('`', Codes),
    !,
    {atom_chars(String, Codes)}.
token(quoted(double, String)) -->
    [Code],
    {char_code('"', Code)},
	quoted_chars('"', Codes),
    !,
    {atom_chars(String, Codes)}.
token(quoted(single, String)) -->
    [Code],
    {char_code('\'', Code)},
	quoted_chars('\'', Codes),
    !,
    {atom_chars(String, Codes)}.
token(value(binary, Binary)) -->
    `0b`,
    base_digits(Codes, 2),
    !,
    {
        append(`0b`, Codes, BinaryCodes),
        atom_chars(Binary, BinaryCodes)
    }.
token(value(octal, Octal)) -->
    `0o`,
    base_digits(Codes, 8),
    !,
    {
        append(`0o`, Codes, OctalCodes),
        atom_chars(Octal, OctalCodes)
    }.
token(value(hex, Hex)) -->
    `0x`,
    hex_chars(Codes),
    !,
    {
        append(`0x`, Codes, HexCodes),
        atom_chars(Hex, HexCodes)
    }.
token(value(float, PosFloat)) -->
    float_digits(Digits),
    {atom_chars(PosFloat, Digits)}.
token(value(float, NegFloat)) -->
    `-`,
    float_digits(Digits),
    {
        char_code('-', Sign),
        atom_chars(NegFloat, [Sign|Digits])
    }.
token(value(int, PosInt)) -->
    base_digits(Digits, 10),
    {atom_chars(PosInt, Digits)}.
token(value(int, NegInt)) -->
    `-`,
    base_digits(Digits, 10),
    {
        char_code('-', Sign),
        atom_chars(NegInt, [Sign|Digits])
    }.
token(mark(Mark)) -->
    char(Punct, punct),
    !,
    {atom_chars(Mark, [Punct])}.
token(lower(Lower)) -->
    char(Code, lower),
    chars(Codes, csym),
    !,
    {atom_chars(Lower, [Code|Codes])}.
token(upper(Upper)) -->
    char(Code, csymf),
    chars(Codes, csym),
    !,
    {atom_chars(Upper, [Code|Codes])}.

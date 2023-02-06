/*
    Tokenizer in Prolog. Splits some code into meaningful tokens to be parsed.
    Use the predicate tokenize/2 to split code into tokens:
        tokenize("type X = Y | {5}", Tokens).
    'Tokens' will unify with
        ["type", "X", "=", "Y", "|", "{", "5", "}"].
    The whitespace is stripped out and meaningful symbols are extracted. "Words"
    and numbers are separated from characters that have special meaning in the language,
    like the pipe ('|') or assign operator ('=').

    A greedy algorithm is used to tokenize the input. The predicate specifies that
    a token is the longest string of characters at the beginning of the input
    that satisfies the token/1 predicate. Upon unifying a token, the rest of the input
    can be used to unify the next token, etc. until there is no more input left, or no
    more tokens can be made from the remaining input. If no more tokens can be made
    and there is input left, then the tokenize predicate fails.
*/
:- module(tokenizer, [tokenize/2, tokenize_chars/2, word/1, num/1, without_last/2]).

token(['{']).
token(['}']).
token(['=']).
token([';']).
token([',']).
token(['|']).
token(['&']).
token(['(']).
token([')']).
token(Chars) :-
    space(Chars);
    word(Chars);
    num(Chars).

space('\s').
space('\t').
space('\n').
space([Char]) :-
    space(Char).
space([Char|Chars]) :-
    space(Char),
    space(Chars).

word(Char) :-
    \+ is_list(Char),
    word([Char]).
word([Char]) :-
    char_type((Char), alnum).
word([Char|Chars]) :-
    char_type(Char, alnum),
    word(Chars).

num(Char) :-
    \+ is_list(Char),
    char_type(Char, digit).
num([Char|Chars]) :-
    string_chars(Str, [Char|Chars]),
    number_string(_, Str).

without_last([_], []).
without_last([X|Xs], [X|WithoutLast]) :-
    without_last(Xs, WithoutLast).

/*
    Get the longest token from the beginning of a string. This is an inefficient but simple way
    of doing this.
*/
maximal_token(Chars, TokenStr) :-
    (
        token(Chars),
        string_chars(Str, Chars),
        TokenStr = Str
    );
    (
        without_last(Chars, ShorterChars),
        maximal_token(ShorterChars, TokenStr)
    ).

tokenize_raw(Chars, [Token|Tokens]) :-
    (
        token(Chars),
        string_chars(Str, Chars),
        [Token|Tokens] = [Str]
    );
    (
        maximal_token(Chars, Token),
        string_chars(Token, TokenChars),
        append(TokenChars, RestChars, Chars),
        tokenize_raw(RestChars, Tokens)
    ).

tokenize(Str, Tokens) :-
    string_chars(Str, Chars),
    tokenize_chars(Chars, Tokens),
    !.

tokenize_chars(Chars, Tokens) :-
    tokenize_raw(Chars, RawTokens),
    strip_spaces(RawTokens, Tokens),
    !.

strip_spaces([], []).
strip_spaces([Token|Tokens], StrippedTokens) :-
    string_chars(Token, Chars),
    (
        (
            space(Chars),
            strip_spaces(Tokens, StrippedTokens)
        );
        (
            \+ space(Chars),
            strip_spaces(Tokens, StrippedTokens2),
            append([Token], StrippedTokens2, StrippedTokens)
        )
    ),
    !.
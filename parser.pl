:- module(parser, [parse_code/1, parse_file/1]).
:- use_module(tokenizer).

% Special characters with meaning
assign_op("=").
open_block("{").
close_block("}").
open_group("(").
close_group(")").
statement_sep(";").
list_sep(",").
type_union_op("|").
type_intersect_op("&").

% Language keywords
type_kw("type").

reserved(Word) :-
    type_kw(Word).

% Defines a valid identifier (for a type, variable, or function name)
ident(X) :-
    \+ is_list(X),
    \+ reserved(X),
    string_chars(X, Chars),
    word(Chars).
ident([X]) :-
    ident(X).

% Defines valid numeric literals
num_literal(X) :-
    number_string(_, X).

parse_file(Path) :-
    open(Path, read, Stream),
    read_file_chars(Stream, Chars),
    close(Stream),
    tokenize_chars(Chars, Tokens),
    statement_list(Tokens),
    !.

parse_code(Code) :-
    tokenize(Code, Tokens),
    statement_list(Tokens),
    !.

statement_list([]).
statement_list(Tokens) :-
    append(Left, Right, Tokens),
    statement(Left),
    statement_list(Right).

statement(Tokens) :-
    append(Body, [Sep], Tokens),
    statement_sep(Sep),
    statement_body(Body).

statement_body(Body) :-
    typedecl(Body).

% Unifies if the input is a type declaration
typedecl([TypeKeyword, TypeName, AssignOp|Rest]) :-
    type_kw(TypeKeyword),
    ident(TypeName),
    assign_op(AssignOp),
    type_expr(Rest).

/*
    A block starts with a curly brace and ends with a curly brace. The predicate
    is true with the first argument is a complete block (starting and ending with
    curly braces), and the second argument is the body of the block.
*/
block([First|Rest], Body) :-
    append(Body, [Last], Rest),
    open_block(First),
    close_block(Last).

/*
    Similar to a block, a group starts and ends with parentheses. This predicate
    is identical to block, except the first argument must contain the Body within
    parentheses.
*/
group([First|Rest], Body) :-
    append(Body, [Last], Rest),
    open_group(First),
    close_group(Last).

set_element(Item) :-
    ident(Item);
    num_literal(Item).

set_literal([First,Last]) :-
    block([First,Last], []).
set_literal([First,Item,Last]) :-
    block([First,Item,Last], [Item]),
    set_element(Item).
set_literal(Block) :- 
    block(Block, Body),
    set_element_list(Body).

set_element_list([Item]) :-
    set_element(Item).
set_element_list([Item,Sep|Rest]) :-
    set_element(Item),
    list_sep(Sep),
    set_element_list(Rest).

/*
    A "type expression" is any algebraic expression that constitutes a new type.
*/
type_expr([Ident]) :-
    ident(Ident).
type_expr(Expr) :-
    type_group(Expr);
    set_literal(Expr);
    type_union(Expr);
    type_intersect(Expr).

type_group(Group) :-
    group(Group, Body),
    type_expr(Body).

type_union(Body) :- 
    append(Left, [Sep|Right], Body),
    type_expr(Left),
    type_union_op(Sep),
    type_expr(Right).

type_intersect(Body) :-
    append(Left, [Sep|Right], Body),
    type_expr(Left),
    type_intersect_op(Sep),
    type_expr(Right).

read_file_chars(Stream, Chars) :-
    fill_buffer(Stream),
    read_pending_chars(Stream, CharsIn, []),
    (
        (
            CharsIn = [],
            Chars = CharsIn
        );
        (
            \+ CharsIn = [],
            read_file_chars(Stream, NextChars),
            append(CharsIn, NextChars, Chars)
        )
    ).

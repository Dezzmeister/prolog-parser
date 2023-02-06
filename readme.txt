Very simple parser in Prolog. It is inefficient and could be optimized for speed
in many places. It parses a program as a list of statements, each separated by a semicolon.
The program is written in a language that looks like this:

    type Five = {5};
    type SmallEvens = {0, 2, 4, 6, 8};
    type X = SmallEvens | Five | {6};
    type Y = X & SmallEvens & ({5} | {2});

Right now it only parses a program as a list of statements, where each statement is a type
declaration. Type declarations follow the syntax above and are meant to look like set
operations. You can parse a file with

    parse_file(Path)

The predicate will be true if the code in the file is valid.
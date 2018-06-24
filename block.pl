block --> [].
block --> ['{'], statement, ['}'], !.
block --> printMissingBracket.

printMissingBracket(_,_):-
    write('Missing { or }'),
    false.

statement --> simpleStatement, [';'].
statement --> simpleStatement, [';'], statement.
simpleStatement --> nameVariable, '=', valueVariable.

valueVariable --> [_|_].
nameVariable --> [_|_].

% test
testBlock:-
    block(['{','x','=','y',';','}'],[]).
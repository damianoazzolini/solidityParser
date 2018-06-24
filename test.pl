% test
:- dynamic declaredVariable/2.
:- dynamic parameterFunction/3.

/*
pragma solidity ^0.4.23;

contract Hello {
    event ContractGreets(); //evemto che il contract può scatenare
    function greet() public {
        emit ContractGreets();
    }
}
*/

space:-
    write(' ').

total --> testTest, functionDefinition.
prova:-
    functionDefinition(['function','greet','(','int','x',',','int','y',')','public','{','int','a',';','}'],[]),
    printParameterList,
    checkValidParameterFunction.

% checks if a function has two times the same parameter or
% two parameters have the same name
% TODO
checkValidParameterFunction:-
    true.

printParameterList:-
    findall([X,Y,Z],parameterFunction(X,Y,Z),L),
    writeln(L).

functionDefinition --> ['function'], 
    checkValidFunction, 
    functionIdentifier, 
    optionalReturnIdentifier, 
    block.

functionIdentifier --> ['private'].
functionIdentifier --> ['public'].
functionIdentifier --> ['external'].
functionIdentifier --> ['internal'].

% optionalReturnIdentifier --> ['returns'], parameterList.
optionalReturnIdentifier --> [].

block --> [].
block --> ['{'], statement, ['}'], !.
block --> printMissingBracket.

printMissingBracket(_,_):-
    write('Missing { or }'),
    false.

% statement --> ['int'], ['a'], [';'].

% check if the name is valid TODO, do it with normal prolog
% % devo salvare la tripla nomeFunzione, tipoVariabile e nomeVariabile
% checkValidFunction([FunctionName,'(',')'|T],T).
checkValidFunction([FunctionName,'('|T],R):-
    parameters(FunctionName,T,R).
    

parameterList(N) --> parameters(N), [')'].

parameters(N) --> testOneParameter(N).
parameters(N) --> testParameters(N), parameters(N), !.
parameters --> [].

testOneParameter(FunctionName,[A,B,')'|T],T):- !,
	checkAndSaveParameter(FunctionName,A,B).

testParameters(FunctionName,[A,B,','|T],T):- !,
    checkAndSaveParameter(FunctionName,A,B).

testParameters(_,[A,B,C|_],[]):- !,
    write('Missing , after'), write(A), space, write(B), space, write('. Found '), writeln(C),
    false.

checkAndSaveParameter(FunctionName,A,B):-
    (   identifier(A) ->  
    	assertz(parameterFunction(FunctionName,A,B));
        write('Identifier '), write(A), writeln(' undefined'), false).

testTest --> testVar, {printCouple}.

printCouple:-
    findall([X,Y],declaredVariable(X,Y),L),
    writeln(L).

testVar --> checkOne.
testVar --> check, testVar, !.
testVar --> [].

checkOne([A,B,';'],[]):- !,
	checkAndSaveVariable(A,B).

checkOne([A,B,C],[]):- !,
    write('Missing ; after'), write(A), write(B), write('. Found '), writeln(C),
    false.

check([A,B,';'|T],T):- !,
    checkAndSaveVariable(A,B).

check([A,B,C|_],[]):- !,
    write('Missing ; after'), write(A), write(B), write('. Found '), writeln(C),
    false.


checkAndSaveVariable(A,B):-
    (   identifier(A) ->  
    	assertz(declaredVariable(A,B));
        write('Identifier '), write(A), writeln(' undefined'), false).

identifier(int).
identifier(float).
% per controllare se una stringa è valida usarebetween/3

% ok vado con statement
/*
Statement = IfStatement | WhileStatement | ForStatement | Block | InlineAssemblyStatement |
            ( DoWhileStatement | PlaceholderStatement | Continue | Break | Return |
              Throw | EmitStatement | SimpleStatement ) ';'.
*/

statement --> simpleStatement, [';'].
statement --> simpleStatement, [';'], statement.
simpleStatement --> nameVariable, '=', valueVariable.

valueVariable --> [_|_].
nameVariable --> [_|_].

variableDefinition --> testVar.
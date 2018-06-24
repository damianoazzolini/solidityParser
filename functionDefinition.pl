:- dynamic declaredVariable/2.
:- dynamic parameterFunction/3.

:- [block].

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
% TODO
optionalReturnIdentifier --> [].

checkValidFunction([FunctionName,'('|T],R):-
    parameters(FunctionName,T,R).

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
    (   elementaryTypeName(A) ->  
    	assertz(parameterFunction(FunctionName,A,B));
        write('ElementaryTypeName '), write(A), writeln(' undefined'), false).

printParameterList:-
    findall([X,Y,Z],parameterFunction(X,Y,Z),L),
    writeln(L).

% TODO check used variables
checkValidParameterFunction:-
    true.

% test
testFunctionDefinition:-
    functionDefinition(['function','greet','(','int','x',',','int','y',')','public','{','int','a',';','}'],[]),
    printParameterList,
    checkValidParameterFunction.
% https://github.com/ethereum/solidity/blob/develop/docs/grammar.txt
% SourceUnit = (PragmaDirective | ImportDirective | ContractDefinition)*
% still missing import directive

pragmaDirective --> pragma, solidity, symbol, version, [';'], !. 
pragmaDirective --> pragma, solidity, symbol, version, [A], {write('expecting ; in pragma directive, found '),writeln(A)}, !, {false}. 
pragmaDirective --> pragma, solidity, symbol, version, {writeln('missing ; in pragma directive')}, !, {false}. 

/*
ImportDirective = 
	'import' StringLiteral ('as' Identifier)? ';'
  | 'import' ('*' | Identifier) ('as' Identifier)? 'from' StringLiteral ';'
  | 'import' '{' Identifier ('as' Identifier)? ( ',' Identifier ('as' Identifier)? )* '}' 'from' StringLiteral ';'

StringLiteral = '"' ([^"\r\n\\] | '\\' .)* '"'
*/

/*
ContractDefinition = ( 'contract' | 'library' | 'interface' ) Identifier
                     ( 'is' InheritanceSpecifier (',' InheritanceSpecifier )* )?
                     '{' ContractPart* '}'
*/

contractDefinition --> 
    definitionPart, !,
    {writeln('definition')}, 
    identifier, !,
    {writeln('identifier')},
    optionalContractDefinitionPart, !, 
    {writeln('optional')},
    ['{'], contractPart, !, {writeln('contract')}, ['}'].

definitionPart --> ['contract'].
definitionPart --> ['library'].
definitionPart --> ['interface'].

optionalContractDefinitionPart --> [].
optionalContractDefinitionPart --> optionalContractDefinition.

optionalContractDefinition --> ['is'], inheritanceSpecifier, [','], inheritanceSpecifier.

inheritanceSpecifier --> [].
/*
ContractPart = StateVariableDeclaration | UsingForDeclaration
  | StructDefinition | ModifierDefinition | FunctionDefinition | 
  EventDefinition | EnumDefinition
*/

contractPart --> stateVariableDeclaration, functionDefinition.
/*
contractPart --> usingForDeclaration.
contractPart --> structDefinition.
contractPart --> modifierDefinition.
contractPart --> eventDefinition.
contractPart --> enumDefinition.
*/
/*
StateVariableDeclaration = TypeName ( 'public' | 'internal' | 'private' | 'constant' )* Identifier ('=' Expression)? ';'
*/

stateVariableDeclaration --> typeName, identifier, optionalInitialValue, [';'].

/*
TypeName = ElementaryTypeName
         | UserDefinedTypeName
         | Mapping
         | ArrayTypeName
         | FunctionTypeName

*/

typeName --> elementaryTypeName.
/*
typeName --> userDefinedTypeName.
typeName --> mapping.
typeName --> arrayTypeName.
typeName --> functionTypeName.
*/

elementaryTypeName --> ['address']. 
elementaryTypeName --> ['bool'].
elementaryTypeName --> ['string']. 
elementaryTypeName --> ['var'].
elementaryTypeName --> ['uint'].
/*
elementaryTypeName --> int.
elementaryTypeName --> uint.
elementaryTypeName --> byte.
elementaryTypeName --> fixed.
elementaryTypeName --> ufixed.
*/

variableIdentifier --> ['public'].
variableIdentifier --> ['internal'].
variableIdentifier --> ['private'].
variableIdentifier --> ['constant'].

/*
FunctionDefinition = 'function' Identifier? ParameterList
                     ( ModifierInvocation | StateMutability | 'external' | 'public' | 'internal' | 'private' )*
                     ( 'returns' ParameterList )? ( ';' | Block ) 
*/

% parameterList --> parameterList, elementaryTypeName, identifier.
% parameterList --> [].


optionalReturnIdentifier --> [].
optionalReturnIdentifier --> ['returns'], parameterList.

/*
ParameterList = '(' ( Parameter (',' Parameter)* )? ')'
*/

parameterList --> ['('], parameters, [')'].

parameters --> {[A|_],writeln(A)}, elementaryTypeName, identifier, parameters.
parameters --> [].

optionalInitialValue --> [].
optionalInitialValue --> ['='], expression.
expression --> [].

identifier --> [_].
identifier --> [].

symbol --> ['('].
symbol --> ['['].
symbol --> ['^'].
symbol --> [';'].
symbol --> [']'].
symbol --> ['+'].
symbol --> [')'].

version --> ['0.4.21'].
pragma --> ['pragma'].
solidity --> ['solidity'].

testPragma:-
	pragmaDirective(['pragma', 'solidity', '^','0.4.21',';'],[]).

testContractDefinition:-
	contractDefinition(['contract','myContract','{','uint','owner',';','function','init','(','uint','i_owner',')','private','{',
                    'owner','=','i_owner',';','}','}'],[]).

test --> pragma, solidity, symbol, version.
testPred:-
    test(['pragma','solidity','^','0.4.21'],[]).

%version([H|_],_):-
%    atom_string(H,S),
%    versionV(S,[]).

%versionV --> atom_string, string_chars, checkVersion.
%checkVersion --> digit.
%checkVersion --> digit, ['.'], version, !.

digit --> ['0'].
digit --> ['1'].
digit --> ['2'].
digit --> ['3'].
digit --> ['4'].
digit --> ['5'].
digit --> ['6'].
digit --> ['7'].
digit --> ['8'].
digit --> ['9'].

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
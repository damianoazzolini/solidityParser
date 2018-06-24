:- [symbol].

pragmaDirective --> pragma, solidity, version, [';'], !. 
pragmaDirective --> pragma, solidity, version, [A], {write('expecting ; in pragma directive, found '),writeln(A)}, !, {false}. 
pragmaDirective --> pragma, solidity, version, {writeln('missing ; in pragma directive')}, !, {false}. 

version --> ['^0.4.21'].
pragma --> ['pragma'].
solidity --> ['solidity'].

% test predicate
testPragma:-
    pragmaDirective(['pragma','solidity','^0.4.21',';'],[]).
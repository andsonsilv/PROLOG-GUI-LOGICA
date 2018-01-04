:- use_module(library(pce)). 

%FATOS

masculino(minato).
masculino(hyuga).
masculino(naruto).
masculino(boruto).

feminino(kushima).
feminino(hinata).
feminino(hanabi).
feminino(himaware).

paiOuMae(minato,naruto).
paiOuMae(kushima,naruto).
paiOuMae(naruto,boruto).
paiOuMae(hinata,boruto).
paiOuMae(naruto,himaware).
paiOuMae(hinata,himaware).

paiOuMae(hyuga,hinata).
paiOuMae(hyuga,hanabi).

conjugue(minato,kushima).
conjugue(naruto,hinata).

%REGRAS Nas regras tem uma relação de se-> então

mae(X,Y) :-
	feminino(X),paiOuMae(X,Y).
pai(X,Y) :-
	masculino(X),paiOuMae(X,Y).

filhos(X,Y) :-
	paiOuMae(Y,X).
filho(X,Y) :-
	masculino(X),filhos(X,Y).
filha(X,Y) :-
	feminino(X),filhos(X,Y).


irmaos(X,Y) :-
	paiOuMae(Z,X),paiOuMae(Z,Y), X \= Y.
irma(X,Y) :-
	feminino(X),irmaos(X,Y).
irmao(X,Y) :-
	masculino(X),irmaos(X,Y).

esposa(X,Y) :-
	feminino(X),conjugue(Y,X).
marido(X,Y) :-
	masculino(X),conjugue(X,Y).

tios(X,Y) :-
	irmaos(X,Z),paiOuMae(Z,Y).
tio(X,Y) :-
	masculino(X),tios(X,Y).
tia(X,Y) :-
	feminino(X),tios(X,Y).

primos(X,Y) :-
	paiOuMae(Z,X),tios(Z,Y).
primo(X,Y) :-
	masculino(X), primos(X,Y).
prima(X,Y) :-
	feminino(X), primos(X,Y).

avoMOuavoF(X,Y) :-
	paiOuMae(X,Z), paiOuMae(Z,Y).
avoM(X,Y) :-
	masculino(X), avoMOuavoF(X,Y).
avoF(X,Y) :-
	feminino(X), avoMOuavoF(X,Y).

netoOuNeta(X,Y) :-
	filhos(X,Z), filhos(Z,Y).
neta(X,Y) :-
	feminino(X), netoOuNeta(X,Y).
neto(X,Y) :-
	masculino(X), netoOuNeta(X,Y).	


%INTERFACE GRÁFICA


menuprincipal(variavel):- 
    	new(A,dialog('FAMILIA UZUMAKI-HYUGA')),
	send(A, size, size(500,150)),
    	send(A,append(button(masculino,message(@prolog,todosmasc)))),
 	send(A,append(button(feminino,message(@prolog,todosfem)))),
	send(A,append(button(filhos_naruto,message(@prolog,todosfilhosnaruto)))),
	send(A,append(button(dna_paterno,message(@prolog,dnapaterno)))),
	send(A,append(button(dna_materno,message(@prolog,dnamaterno)))),
	get(A, confirm, respostavariavel),
    	respostavariavel \== 0, % os termos forem diferentes 
    	variavel = respostavariavel,
    	send(A,open).

todosmasc:-
	findall(X,masculino(X),L),
	atomics_to_string(L, ' -- ', S),
	new(A, window('TODOS OS MEMBROS QUE TEM GENERO MASCULINO')),
	send(A, size, size(500, 200)),
	send(A, display, text(S), point(4, 10)),		
	writeln(S),
	send(A, open).

todosfem:- 
	findall(X,feminino(X),L),
	atomics_to_string(L, ' -- ', S),
	new(A, window('TODOS OS MEMBROS QUE TEM GENERO FEMININO')),
	send(A, size, size(500, 200)),
	send(A, display, text(S), point(4, 10)),		
	send(A, open).

todosfilhosnaruto:-
	findall(X,filhos(X,naruto),L),
	atomics_to_string(L, ' -- ', S),
	new(A, window('TODOS OS FILHOS DE NARUTO')),
	send(A, size, size(500, 200)),
	send(A, display, text(S), point(4, 10)),
	send(A, open).

dnapaterno:-
	new(A, dialog('HORA DA VERDADE!')),
	new(X, text_item(pai)),
	new(Y, text_item(filho)),
	send(A, append(button(resultado,message(@prolog,funcaopai,X?selection,Y?selection)))),
	send(A, append,X),
	send(A, append,Y),
	get(A, confirm, respostavariavel),
	respostavariavel \== 0,
	send(A,open).

% \+ é o operador "não provável". Ele é bem sucedido se seu argumento não for provável (e falha se seu argumento for provável).
funcaopai(X,Y):-  
	pai(X,Y),
	new(A, dialog('pai')),
	new(K, text('VERDADE')),
	send(A, append,K),
	send(A, open);
	\+pai(X,Y),
	new(A, dialog('pai')),
	new(K, text('FALSO')),
	send(A, append,K),
	send(A, open).

dnamaterno:-
	new(A, dialog('HORA DA VERDADE!')),
	new(X, text_item(mae)),
	new(Y, text_item(filho)),
	send(A, append(button(resultado,message(@prolog,funcaomae,X?selection,Y?selection)))),
	send(A, append,X),
	send(A, append,Y),
	get(A, confirm, respostavariavel),
	respostavariavel \== 0,
	send(A,open).

% \+ é o operador "não provável". Ele é bem sucedido se seu argumento não for provável (e falha se seu argumento for provável).
funcaomae(X,Y):-  
	mae(X,Y),
	new(A, dialog('mae')),
	new(K, text('VERDADE')),
	send(A, append,K),
	send(A, open);
	\+mae(X,Y),
	new(A, dialog('pai')),
	new(K, text('FALSO')),
	send(A, append,K),
	send(A, open).


:-menuprincipal(X).




















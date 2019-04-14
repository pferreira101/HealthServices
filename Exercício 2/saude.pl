%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Sistemas de Representação de conhecimento e Raciocínio - Exercício 2
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% SICStus PROLOG: Declaracoes iniciais
:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Definicoes Iniciais

:- op(900, xfy, '::').
:- dynamic utente/4.
:- dynamic prestador/4.
:- dynamic cuidado/7.
:- dynamic nulointerdito/1.
:- dynamic '-'/1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -




% Extensão do predicado 'utente': ID, Nome, Idade, Morada => {V, F, D}
utente(1, 'Pedro', 20, 'Famalicao').
utente(2, 'Nelson', 35, 'Gaia').
utente(3, 'Miguel', 28, 'Barcelos').
utente(4, 'Henrique', 10, 'Braga').
utente(5, 'Rui', 65, 'Famalicao').
utente(6, 'Maria', 20, 'Famalicao').
utente(7, 'Catarina', 43, 'Trofa').
utente(8, 'Gabriela', 80, 'Famalicao').
utente(9, 'Joao', desconhecido, desconhecido).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Explicitacao das situacoes de excecao

% A Rosa é da Trofa	ou de Vila do Conde	
excecao(utente(10, 'Rosa', 44, 'Trofa')).
excecao(utente(10, 'Rosa', 44, 'Vila do Conde')).

% E desconhecido o nome de um utente que deu entrada com 60 anos vindo de Barcelos	
utente(11, desconhecido, 60, 'Barcelos').

% Nao se conhece a idade do Mario que vive em Lisboa
utente(12, 'Mario', desconhecido, 'Lisboa').

% Nao se conhece a a morada da Joana de 24 anos.
utente(13, 'Joana', 24, desconhecido).



excecao( utente( Id, Nome, Idade, Morada ) ) :-
    utente( Id, desconhecido, Idade, Morada ).
excecao( utente( Id, Nome, Idade, Morada ) ) :-
    utente( Id, Nome, desconhecido	, Morada ).
excecao( utente( Id, Nome, Idade, Morada ) ) :-
    utente( Id, Nome, Idade, desconhecido ).

excecao( utente( Id, Nome, Idade, Morada ) ) :-
    utente( Id, interdito, Idade, Morada ).



nulointerdito(interdito).
% Invariante  ... : nao poder adicionar conhecimento interdito ao utente.
+utente( Id, Nome, Idade, Morada ) :: (solucoes( Ns ,(utente( Id, Ns, Idade, Morada ),nao(nulointerdito(Ns))),S ),
                  comprimento( S,N ), N == 0 
                  ).
+utente( Id, Nome, Idade, Morada ) :: (solucoes( Is ,(utente( Id, Nome, Is, Morada ),nao(nulointerdito(Is))),S ),
                  comprimento( S,N ), N == 0 
                  ).
+utente( Id, Nome, Idade, Morada ) :: (solucoes( Ms ,(utente( Id, Nome, Idade, Ms ),nao(nulointerdito(Ms))),S ),
                  comprimento( S,N ), N == 0 
                  ).


-utente(ID, Nome, Idade, Morada) :- nao(utente(ID, Nome, Idade, Morada)) , nao(excecao(utente(ID, Nome, Idade, Morada))).



% Extensão do predicado 'prestador': ID, Nome, Especialidade, Instituição => {V, F, D}
prestador(1, 'Maria', 'Geral', 'Hospital Sao Joao').
prestador(2, 'Tiago', 'Oncologia', 'Hospital Sao Joao').
prestador(3, 'Diogo', 'Cardiologia', 'Hospital Sao Joao').
prestador(4, 'Alexandra', 'Urologia', 'Hospital Sao Joao').
prestador(5, 'Ivone', 'Pediatria', 'Hospital Sao Vitor').
prestador(6, 'Costa', 'Dermatologia', 'Hospital Sao Vitor').
prestador(7, 'Antonio', 'Ginecologia', 'Hospital Sao Vitor').
prestador(8, 'Ricardo', 'Radiologia', 'Hospital Santa Maria').
prestador(9, 'Gabriela', 'Neurologia', 'Hospital Santa Maria').
prestador(10, 'Anibal', 'Geral', 'Hospital Santa Maria').

-prestador(ID, Nome, Espec, Inst) :- nao(prestador(ID, Nome, Espec, Inst)) , nao(excecao(prestador(ID, Nome, Espec, Inst))).



% Extensão do predicado 'cuidado': Ano, Mês, Dia, ID Utente, ID Prestador, Descrição, Custo => {V, F, D}
cuidado(2019, 04, 06, 1, 2, 'Geral', 40).
cuidado(2019, 04, 07, 3, 1, 'Urologia', 25).
cuidado(2019, 04, 08, 1, 3, 'Dermatologia', 50).
cuidado(2019, 04, 09, 2, 1, 'Oncologia', 25).

-cuidado(Ano, Mes, Dia, IdU, IdP, Desc, Custo) :- nao(cuidado(Ano, Mes, Dia, IdU, IdP, Desc, Custo)), nao(excecao(cuidado(Ano, Mes, Dia, IdU, IdP, Desc, Custo))).






%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% PREDICADOS AUXILIARES 
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

evolucao( Termo ) :-
    solucoes( Invariante,+Termo::Invariante,Lista ),
    insercao( Termo ),
    teste( Lista ).

insercao( Termo ) :-
    assert( Termo ).
insercao( Termo ) :-
    retract( Termo ),!,fail.

teste( [] ).
teste( [R|LR] ) :-
    R,
    teste( LR ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado capaz de encontrar todas as possibilidades de prova de um teorema.
% 'solucoes': F, Q, S -> {V,F}

solucoes(F, Q, S) :- Q, assert(tmp(F)), fail.
solucoes(F, Q, S) :- construir(S, []).	

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado 'construir': S1,S2 -> {V,F}

construir(S1, S2) :- retract(tmp(X)), !, construir(S1, [X|S2]).
construir(S, S).    


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que calcula o comprimento de uma lista 
% 'comprimento': L, Resultado -> {V,F}

comprimento([], 0).
comprimento([H|T],R) :- comprimento(T,D) , R is D+1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado negação: 
% 'nao': Termo -> {V,F}
nao(Termo) :- Termo, !, fail.
nao(Termo).

% Extensão do predicado 'bissexto': Ano => {V, F}
bissexto(X) :- X mod 4 == 0.

% Extensão do predicado 'data': Ano, Mes, Dia => {V, F}
data(A,M,D) :- M\=2, A>=2000, contains(M,[1,3,5,7,8,10,12]), D>0, D=<31.
data(A,M,D) :- M\=2, A>=2000, contains(M,[4,6,9,11]), D>=1, D=<30.
data(A,M,D) :- M==2 , bissexto(A), A>=2000, D>=1, D=<29.
data(A,M,D) :- M==2 , nao(bissexto(A)), A>=2000, D>=1, D=<28.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado si: Questao,Resposta -> {V,F}
%                            Resposta = { verdadeiro,falso,desconhecido }

si( Questao,verdadeiro ) :-
    Questao.
si( Questao,falso ) :-
    -Questao.
si( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).

siList([], []).
siList([Questao	| T], [H | Y]) :- si(Questao, H), siList(T, Y).    
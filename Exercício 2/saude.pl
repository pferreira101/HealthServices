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
:- op(901, xfy, '//' ).
:- op(902, xfy, '&&' ).
:- op(903, xfy, '->' ).
:- op(900, xfy, ':::').
:- dynamic utente/4.
:- dynamic prestador/4.
:- dynamic cuidado/7.
:- dynamic interdito/1.
:- dynamic desconhecido/1.
:- dynamic excecao/1.
:- dynamic '-'/1.
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

%NOME INTERDITO, IDADE DESCONHECIDA E CIDADES INCERTAS

% Extensão do predicado 'utente': ID, Nome, Idade, Morada => {V, F, D}
utente(1, 'Pedro', 20, 'Famalicao').
utente(2, 'Nelson', 35, 'Gaia').
utente(3, 'Miguel', 28, 'Barcelos').
utente(4, 'Henrique', 10, 'Braga').
utente(5, 'Rui', 65, 'Famalicao').
utente(6, 'Maria', 20, 'Famalicao').
utente(7, 'Catarina', 43, 'Trofa').
utente(8, 'Gabriela', 80, 'Famalicao').
-utente(9, 'Joao', 80, 'Trofa').

-utente(ID, Nome, Idade, Morada) :- nao(utente(ID, Nome, Idade, Morada)) , nao(excecao(utente(ID, Nome, Idade, Morada))).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Explicitacao das situacoes de excecao do predicado utente

% A Rosa é da Trofa	ou de Vila do Conde	
excecao(utente(10, 'Rosa', 44, 'Trofa')).
excecao(utente(10, 'Rosa', 44, 'Vila do Conde')).

% E desconhecido o nome de um utente que deu entrada com 60 anos vindo de Barcelos	
utente(11, desconhecido, 60, 'Barcelos').

% Nao se conhece a idade do Mario que vive em Lisboa
utente(12, 'Mario', desconhecido, 'Lisboa').

% Nao se conhece a morada da Joana de 24 anos.
utente(13, 'Joana', 24, desconhecido).



excecao( utente( Id, Nome, Idade, Morada ) ) :-
    utente( Id, desconhecido, Idade, Morada ).
excecao( utente( Id, Nome, Idade, Morada ) ) :-
    utente( Id, Nome, desconhecido	, Morada ).
excecao( utente( Id, Nome, Idade, Morada ) ) :-
    utente( Id, Nome, Idade, desconhecido ).



/*
nulointerdito(interdito).
 Invariante  ... : nao poder adicionar conhecimento interdito ao utente.
+utente( Id, Nome, Idade, Morada ) :: (solucoes( Ns ,(utente( Id, Ns, Idade, Morada ),nao(nulointerdito(Ns))),S ),
                  comprimento( S,N ), N == 0 
                  ).
+utente( Id, Nome, Idade, Morada ) :: (solucoes( Is ,(utente( Id, Nome, Is, Morada ),nao(nulointerdito(Is))),S ),
                  comprimento( S,N ), N == 0 
                  ).
+utente( Id, Nome, Idade, Morada ) :: (solucoes( Ms ,(utente( Id, Nome, Idade, Ms ),nao(nulointerdito(Ms))),S ),
                  comprimento( S,N ), N == 0 
                  ).
*/






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
-prestador(10, 'Anibal', 'Geral', 'Hospital Santa Maria').

-prestador(ID, Nome, Esp, Inst) :- nao(prestador(ID, Nome, Esp, Inst)) , nao(excecao(prestador(ID, Nome, Esp, Inst))).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Explicitacao das situacoes de excecao do predicado prestador

% Não se sabe se o prestador Severino trabalha no Hospital Sao Vitor ou no Hospital Sao Joao
excecao(prestador(11, 'Severino', 'Geral', 'Hospital Sao Vitor')).
excecao(prestador(11, 'Severino', 'Geral', 'Hospital Sao Joao')).

% Não se sabe a Especialidade do prestador Joaquim que trabalha no Hospital Sao Vitor
excecao(prestador(12, 'Joaquim', 'Geral', 'Hospital Sao Vitor')).
excecao(prestador(12, 'Joaquim', 'Neurologia', 'Hospital Sao Vitor')).

% E desconhecido o nome do prestador com ID 12, que trabalha no Hospital Santa Maria em Cardiologia	
prestador(13, desconhecido, 'Cardiologia', 'Hospital Santa Maria').

% Não se sabe a especialidade do prestador Rui que trabalha no Hospital Sao Joao
prestador(14, 'Rui', desconhecido, 'Hospital Sao Joao').


excecao(prestador(Id, Nome, Esp, Inst)) :-
    prestador(Id, desconhecido, Esp, Inst).
excecao(prestador(Id, Nome, Esp, Inst)) :-
    prestador(Id, Nome, desconhecido, Morada).
excecao(prestador(Id, Nome, Esp, Inst)) :-
    prestador(Id, Nome, Esp, desconhecido).





% Extensão do predicado 'cuidado': Ano, Mês, Dia, ID Utente, ID Prestador, Descrição, Custo => {V, F, D}
cuidado(2019, 04, 06, 1, 2, 'Oncologia', 40).
cuidado(2019, 04, 07, 3, 1, 'Geral', 25).
cuidado(2019, 04, 08, 1, 3, 'Cardiologia', 50).
-cuidado(2019, 04, 09, 2, 1, 'Geral', 25).

-cuidado(Ano, Mes, Dia, IdU, IdP, Desc, Custo) :- nao(cuidado(Ano, Mes, Dia, IdU, IdP, Desc, Custo)), nao(excecao(cuidado(Ano, Mes, Dia, IdU, IdP, Desc, Custo))).


% Não se sabe ao certo o preço do cuidado que o utente 4 realizou no dia 15/4/2019, mas sabe-se que foi 30 ou 35
excecao(cuidado(2019, 04, 15, 4, 3, 'Cardiologia', 30)).
excecao(cuidado(2019, 04, 15, 4, 3, 'Cardiologia', 35)).

% Não se sabe qual foi o prestador do cuidado realizado no dia 15/4/2019, ao utente 5, com um custo de 50
excecao(cuidado(2019, 04, 15, 5, desconhecido, 'Pediatria', 50)).

% Não se sabe a que utente foi realizado o cuidado no dia 15/4/2019, pelo prestador 5, com um custo de 25
excecao(cuidado(2019, 04, 15, desconhecido, 5, 'Pediatria', 25)).

% Não se sabe o custo do cuidado realizado no dia 15/4/2019, pelo prestador 6 ao utente 9
excecao(cuidado(2019, 04, 15, 9, 6, 'Dermatologia', desconhecido)).


excecao(cuidado(Ano, Mes, Dia, IdU, IdP, Desc, Custo)) :-
    cuidado(Ano, Mes, Dia, desconhecido, IdP, Desc, Custo).
excecao(cuidado(Ano, Mes, Dia, IdU, IdP, Desc, Custo)) :-
    cuidado(Ano, Mes, Dia, IdU, desconhecido, Desc, Custo).
excecao(cuidado(Ano, Mes, Dia, IdU, IdP, Desc, Custo)) :-
    cuidado(Ano, Mes, Dia, IdU, desconhecido, Desc, Custo).
excecao(cuidado(Ano, Mes, Dia, IdU, IdP, Desc, Custo)) :-
    cuidado(Ano, Mes, Dia, IdU, IdP, Desc, desconhecido).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% REGISTAR UTENTES, PRESTADORES E CUIDADOS:
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% Extensão do predicado 'regU': ID, Nome, Idade, Cidade -> {V, F}
regU(ID,Nome,Idade,Cidade) :- desconhecido(ID), involucao(utente(ID,Nome,desconhecido,Cidade)), involucao(desconhecido(ID)), evolucao(utente(ID,Nome,Idade,Cidade)).
regU(ID,Nome,Idade,Cidade) :- Nome \== interdito, evolucao(utente(ID,Nome,Idade,Cidade)).
regExcIdadeDescU(ID,Nome,Idade,Cidade) :- evolucaoIdadeDescohecida(utente(ID,Nome,Idade,Cidade)).
regExcMoradaU(ID, Nome, Idade, [Cidade|T]) :- evolucaoMoradaIncerta(utente(ID,Nome,Idade, [Cidade|T])).
regNaoU(ID,Nome,Idade,Cidade) :- evolucao(-utente(ID,Nome,Idade,Cidade)).
regIdadesU(ID,Nome,[Idade1|T],Cidade):- evolucaoIdadesIncertas(utente(ID,Nome,Idade1,Cidade)).
regNomeInterditoU(ID,Nome,Idade,Cidade):- evolucaoNomeInterdito(utente(ID,Nome,Idade,Cidade)).


% Extensão do predicado 'regP': ID, Nome, Especialidade, Instituição -> {V, F}
regP(ID,Nome,Especialidade,Instituicao):- evolucao(prestador(ID,Nome,Especialidade,Instituicao)).
regExcP(ID,Nome,Especialidade,Instituicao):- evolucao(excecao(prestador(ID,Nome,Especialidade,Instituicao))).

% Extensão do predicado 'regC': Ano,Mes,Dia, IDUtente,IDPrestador, Descricao, Custo -> {V, F}
regC(Ano,Mes,Dia,IDU,IDP,Descricao,Custo):- evolucao(cuidado(Ano,Mes,Dia,IDU,IDP,Descricao,Custo)).
regExcC(Ano,Mes,Dia,IDU,IDP,Descricao,Custo):- evolucao(excecao(cuidado(Ano,Mes,Dia,IDU,IDP,Descricao,Custo))).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes Estruturais
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Não permitir conhecimento repetido
+utente(ID,Nome,I,M) :: (solucoes(ID,desconhecido(ID),R),
                            comprimento(R,N),
                            N >= 0, N =< 1).
+utente(ID, Nome, I, M) :: (solucoes((ID, Nome, I, M), (utente(ID, Nome, I, M)), R1),
                            comprimento(R1,N1),
                            solucoes(ID, (excecao(utente(ID,Nomes,Is,Ms))), R2),
                            removeRepetidos(R2,RES2),
                            comprimento(RES2, N2),
                            N is N1+N2,
                            N1 == 1 ).

+prestador(ID, Nome, E, I) :: (solucoes((ID, Nome, E, I), (prestador(ID, Nome, E, I)), R1),
                            comprimento(R1, N1),
                            solucoes(ID, (excecao(prestador(ID,Nomes,Es,Is))), R2),
                            removeRepetidos(R2,RES2),
                            comprimento(RES2, N2),
                            N is N1+N2,
                            N == 1  ).

+cuidado(Ano, Mes, Dia, IdU, IdP, Desc, Custo) :: (solucoes((Ano, Mes, Dia, IdU, IdP, Desc, Custo), 
                                                (cuidado(Ano, Mes, Dia, IdU, IdP, Desc, Custo)), R1),
                                                comprimento(R1, N1), 
                                                solucoes(ID, (excecao(cuidado(Ano, Mes, Dia, IdU, IdP, Desc, Custo))), R2),
                                                removeRepetidos(R2,RES2),
                                                comprimento(RES2, N2),
                                                N is N1+N2, 
                                                N == 1 ).
                                    

% Não funciona
+(-utente(ID, Nome, I, M)) :: (solucoes((ID, Nome, I, M), (-utente(ID, Nome, I, M)), R),
                        comprimento(R, N), 
                        N == 1 ).

/* Nao inserir com o mesmo ID - nao funciona
+(-utente(ID, Nome, I, M)) :: (solucoes((ID, Nome, I, M), (-utente(ID, Ns, Is, Ms)), R),
                        comprimento(R, N), 
                        N == 1 ).
*/
/*
+(-prestador(ID, Nome, E, I)) :: (solucoes((ID, Nome, E, I), (-prestador(ID, Nome, E, I)), R),
                            comprimento(R, N),
							N == 1	).

+(-cuidado(Ano, Mes, Dia, IdU, IdP, Desc, Custo)) :: 
        (solucoes((Ano, Mes, Dia, IdU, IdP, Desc, Custo), 
            (-cuidado(Ano, Mes, Dia, IdU, IdP, Desc, Custo)), R),
        comprimento(R, N), 
        N == 1 ).
*/

% Não permitir conhecimento contraditório
/*
+utente(ID, Nome, I, M) :: (solucoes((ID, Nome, I, M), (-utente(ID, Nome, I, M)), R),
                        comprimento(R, N), 
                        N == 1 ).

+prestador(ID, Nome, E, I) :: (solucoes((ID, Nome, E, I), (-prestador(ID, Nome, E, I)), R),
                            comprimento(R, N),
							N == 1	).

+cuidado(Ano, Mes, Dia, IdU, IdP, Desc, Custo) :: 
        (solucoes((Ano, Mes, Dia, IdU, IdP, Desc, Custo), 
            (-cuidado(Ano, Mes, Dia, IdU, IdP, Desc, Custo)), R),
        comprimento(R, N), 
        N == 1 ).

+(-utente(ID, Nome, I, M)) :: (solucoes((ID, Nome, I, M), (utente(ID, Nome, I, M)), R),
                        comprimento(R, N), 
                        N == 1 ).


+(-prestador(ID, Nome, E, I)) :: (solucoes((ID, Nome, E, I), (prestador(ID, Nome, E, I)), R),
                            comprimento(R, N),
							N == 1	).

+(-cuidado(Ano, Mes, Dia, IdU, IdP, Desc, Custo)) :: 
        (solucoes((Ano, Mes, Dia, IdU, IdP, Desc, Custo), 
            (cuidado(Ano, Mes, Dia, IdU, IdP, Desc, Custo)), R),
        comprimento(R, N), 
        N == 1 ).

*/

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes Referenciais
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% UTENTE

% Nao pode ser inserido um utente com o mesmo ID
+utente(ID, Nome, Idade, Morada) :: (solucoes(ID, (utente(ID, Ns, I, M)), R),
                  			         comprimento(R, N), 
							         N == 1 ).

% IDs têm que ser naturais
%+utente(ID, Nome, Idade, Morada) :: natural(ID).    %  SE INTRODUZIR LETRAS DÁ UM ERRO EM VEZ DE SO "NO"; 
%AO INTRODUZIR CONHECIMENTO REPETIDO NAO ACABA POR CAUSA DESTA LINHA TAMBEM; MAS SE FOR ":-" EM VEZ DE "::" ACHO QUE FUNCIONA

% A Idade dum utente > 0
+utente(ID, Nome, Idade, Morada) :: Idade >= 0.

% Não se pode remover utentes com cuidados marcados
-utente(ID, Nome, Idade, Morada) :: (solucoes(ID, (cuidado(A, M, D, ID, IdP, Desc, C)), R),
                  			       comprimento(R, N), 
							       N == 0 ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% PRESTADOR

% Nao pode ser inserido um prestador com o mesmo ID
+prestador(ID, Nome, Esp, Inst) :: (solucoes(ID, (prestador(ID, Ns, E, I)), R),
                  			       comprimento(R, N), 
							       N == 1 ).

% IDs têm que ser naturais
%+prestador(ID, Nome, Esp, Inst) :: natural(ID).    %  SE INTRODUZIR LETRAS DÁ UM ERRO EM VEZ DE SO "NO"; AO INTRODUZIR CONHECIMENTO REPETIDO NAO ACABA POR CAUSA DESTA LINHA TAMBEM

/*
% Não existem dois prestadores com a mesma especialiade na mesma instituição
+prestador(ID, Nome, Esp, Inst) :: (solucoes((Esp, Inst), (prestador(_, _, Esp, Inst)), R1),
                                    comprimento(R1, N1),
                                    solucoes((Ids), (excecao(prestador(Ids, _, Esp, Inst))), R2),
                                    removeRepetidos(R2, R3),
                                    comprimento(R3, N2),
                                    N is N1+N2,
                                    N == 1).
*/

% Não se pode remover prestadores com cuidados marcados
-prestador(ID, Nome, Esp, Inst) :: (solucoes(ID, (cuidado(A, M, D, IdU, ID, Desc, C)), R),
                  			       comprimento(R, N), 
							       N == 0 ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% CUIDADO

% Não é permitido um utente realizar o mesmo tipo de cuidado no mesmo dia
+cuidado(Ano, Mes, Dia, IdU, IdP, Desc, Custo) :: 
        (solucoes((IdU, IdP), cuidado(Ano, Mes, Dia, IdU, IdP, Desc, C), R), 
         comprimento(R, N), 
         N == 1 ).

+cuidado(A, M, D, IdU, IdP, Desc, C) :: (utente(IdU, Ns, Is, Ms), prestador(IdP, No, Idade, H)).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% EXCEÇÕES
+excecao(utente(ID, Nome, Idade, Morada)) :: nao(utente(ID, N, I, M)).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SISTEMA DE INFERÊNCIA
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% TABELAS DE VERDADE
disj(verdadeiro, Q, verdadeiro).
disj(P, verdadeiro, verdadeiro).
disj(desconhecido, Q, desconhecido) :- Q \= verdadeiro.
disj(falso, desconhecido, desconhecido).
disj(falso, falso, falso).

conj(falso, Q, falso).
conj(P, falso, falso).
conj(desconhecido, Q, desconhecido) :- Q \= falso.
conj(P, desconhecido, desconhecido) :- Q \= falso.
conj(verdadeiro, verdadeiro, verdadeiro).

imp(falso, Q, verdadeiro).
imp(verdadeiro, Q, Q).
imp(desconhecido, Q, desconhecido).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado si: Questao,Resposta -> {V,F}
%                            Resposta = { verdadeiro,falso,desconhecido }

si(P // Q , R) :- si(P, R1) , si(Q, R2) , disj(R1, R2, R).
si(P && Q , R) :- si(P, R1) , si(Q, R2) , conj(R1, R2, R).
si(P -> Q , R) :- si(P, R1) , si(Q, R2) , imp(R1, R2, R).

si(Questao, verdadeiro) :- Questao.
si(Questao, falso) :- -Questao.
si(Questao, desconhecido) :- nao(Questao), nao(-Questao).

siList([], []).
siList([Questao	| T], [H | Y]) :- si(Questao, H), siList(T, Y).  

teste(pedro).
teste(miguel).
-teste(p).
-teste(m).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% PREDICADOS EVOLUCAO IMPERFEITA
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

evolucaoIdadeDescohecida(utente(ID, Nome, Idade, Cidade)) :- 
    Idade == desconhecido,
    evolucao(desconhecido(ID)),
    evolucaoDesconhecido(utente(ID, Nome, Idade, Cidade)).

evolucaoDesconhecido(Termo) :-
    solucoes(Invariante, +Termo:::Invariante, Lista),
    insercao(Termo),
    teste(Lista).

evolucaoNomeInterdito(utente(ID,Nome,Idade,Cidade)):-
    Nome == interdito,
    evolucao(interdito(ID)),
    evolucaoInterdito(utente(ID,Nome,Idade,Cidade)).

evolucaoInterdito(Termo):- 
    solucoes(Invariante,+Termo::Invariante,Lista),
    insercao(Termo),
    teste(Lista).


evolucaoMoradaIncerta(utente(ID, Nome, Idade, [])) :- evolucao(incerto(ID)).
evolucaoMoradaIncerta(utente(ID, Nome, Idade, [Cidade|T])):-
    evolucao(excecao(utente(ID, Nome, Idade, Cidade))), 
    evolucaoMoradaIncerta(utente(ID, Nome, Idade, T)).

evolucaoIdadesIncertas(utente(ID,Nome,[],Cidade)).
evolucaoIdadesIncertas(utente(ID,Nome,[Idade|T],Cidade)):-
    evolucao(excecao(utente(ID,Nome,Idade,Cidade))),
    evolucaoIdadesIncertas(utente(ID,Nome,T,Cidade)).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% PREDICADOS AUXILIARES 
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
evolucao(Termo) :-
    solucoes(Invariante, +Termo::Invariante, Lista),
    insercao(Termo),
    teste(Lista).

insercao(Termo) :- assert(Termo).
insercao(Termo) :- retract(Termo), !, fail.

teste([]).
teste([R|LR]) :- R, teste(LR).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado que permite a involucao do conhecimento
% 'involucao': T -> {V,F}

involucao(Termo) :- Termo,
                   solucoes(Invariante, -Termo::Invariante, LInvariantes),
                   remocao(Termo),
                   teste(LInvariantes).

remocao(Termo) :- retract(Termo).
remocao(Termo) :- assert(Termo), !, fail.

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

% Extensão do predicado 'removeRepetidos' que remove os elementos repetidos duma lista
removeRepetidos([], []).
removeRepetidos([H|T], R) :- contains(H, T) , removeRepetidos(T, R). 
removeRepetidos([H|T], [H|R]) :- nao(contains(H, T)) , removeRepetidos(T, R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite verificar se um numero é natural: 
% 'natural':  Numero -> {V,F}
natural(1).
natural(N) :- M is N-1 , natural(M).
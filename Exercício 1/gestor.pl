%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais
:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).



%  Definições auxiliares

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado solucoes: F, Q, S -> {V,F}
solucoes(F, Q, S) :- Q, assert(tmp(F)), fail.
solucoes(F, Q, S) :- construir(S, []).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado construir: S1,S2 -> {V,F}
construir(S1, S2) :- retract(tmp(X)), !, construir(S1, [X|S2]).
construir(S, S).




% Extensão do predicado 'utente': ID, Nome, Idade, Cidade => {V, F}
utente(1, pedro, 20, famalicao).
utente(2, nelson, 35, gaia).
utente(3, miguel, 28, barcelos).
utente(4, henrique, 10, braga).
utente(5, rui, 65, famalicao).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'servico': ID, Descrição, Instituição, Cidade => {V, F}

servico(1, geral, sjoao, porto).
servico(2, oncologia, sjoao, porto).
servico(3, oncologia, svitor, braga).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'consulta': Data, ID Utente, ID Serviço, Custo => {V, F}
consulta('20/02/2018', 1, 2, 40).
consulta('21/02/2018', 3, 1, 25).


% REGISTAR UTENTES, SERVIÇOS E CONSULTAS:


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Entensão do predicado 'regU': ID, Nome, Idade, Cidade => {V, F}


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Entensão do predicado 'regS': ID, Descricao, Instituicao, Cidade => {V, F}

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Entensão do predicado 'regC': IdU, IdS, Custo => {V, F}





% REMOVER UTENTES, SERVIÇOS E CONSULTAS:


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Entensão do predicado 'remU': ID, Nome, Idade, Cidade => {V, F}


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Entensão do predicado 'remS': ID, Descricao, Instituicao, Cidade => {V, F}


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Entensão do predicado 'remC': IdU, IdS, Custo => {V, F}





% IDENTIFICAR AS INSTITUIÇÕES PRESTADORAS DE SERVIÇOS:


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Entensão do predicado 'instituicoes': LInstituicoes => {V, F}
instituicoes(L) :- solucoes(Nome,servico(_,_,Nome,_),L).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado solucoes: F, Q, S -> {V,F}

solucoes(F,Q,S) :- Q, assert(tmp(F)), fail.
solucoes(F,Q,S) :- construir(S, []).


% Extensao do predicado construir: S1,S2 -> {V,F}

construir(S1, S2) :-
	retract(tmp(X)), !, construir(S1, [X|S2]).
construir(S,S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado que permite a evolucao do conhecimento


% IDENTIFICAR UTENTES/SERVIÇOS/CONSULTAS POR CRITÉRIOS DE SELEÇÃO:






% IDENTIFICAR SERVIÇOS PRESTADOS POR INSTITUIÇÃO/CIDADE/DATAS/CUSTO:

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Entensão do predicado 'servInstituicao': Instituicao, Resultado => {V, F}
servInstituicao(Instituicao, R) :- solucoes((ID, Nome), servico(ID, Nome, Instituicao, _), R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Entensão do predicado 'servCidade': Cidade, Resultado => {V, F}
servCidade(Cidade, R) :- solucoes((ID, Nome), servico(ID, Nome, _, Cidade), R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Entensão do predicado 'servData': Data, Resultado => {V, F}
servData(Data, R) :- solucoes((ID, Nome), (servico(ID, Nome, _, _), consulta(Data, _, ID, _)), R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Entensão do predicado 'servData': Data, Resultado => {V, F}
servCusto(Custo, R) :- solucoes((ID, Nome), (servico(ID, Nome, _, _), consulta(_, _, ID, Custo)), R).



% IDENTIFICAR OS UTENTES DE UM SERVIÇO/INSTITUIÇÃO:

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Entensão do predicado 'utentesServico': LUtentes, Servico => {V, F}
utentesServico([X],S) :- consulta(X,S,_).
utentesServico([X|T],S) :- consulta(X,S,_), utentesServico(T,S).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Entensão do predicado 'utentesInstituicao': LUtentes, Instituicao => {V, F}
utentesInstituicao([X],I) :- consulta(X,S,_), servico(S,_,I,_).
utentesInstituicao([X|T],I) :- consulta(X,S,_), servico(S,_,I,_), utentesServico(T,S).


% IDENTIFICAR SERVIÇOS REALIZADOS POR UTENTE/INSTITUIÇÃO/CIDADE:





% CALCULAR O CUSTO TOTAL DOS CUIDADOS DE SAÚDE POR UTENTE/SERVIÇO/INSTITUIÇÃO/DATA:

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


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado que permite a evolucao do conhecimento
evolucao(Termo) :- solucoes(Invariante, +Termo::Invariante, LInvariantes),
	               insercao(Termo),
	               teste(LInvariantes).

teste([]).
teste([H|T]) :- H, teste(T).

insercao(Termo) :- assert(Termo).
insercao(Termo) :- retract(Termo), !, fail.




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
consulta('20/02/2019', 1, 2, 40).
consulta('21/02/2019', 3, 1, 25).
consulta('25/02/2019', 1, 3, 50).
consulta('25/02/2019', 2, 1, 25).

% REGISTAR UTENTES, SERVIÇOS E CONSULTAS:


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'regU': ID, Nome, Idade, Cidade => {V, F}


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'regS': ID, Descricao, Instituicao, Cidade => {V, F}

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'regC': IdU, IdS, Custo => {V, F}





% REMOVER UTENTES, SERVIÇOS E CONSULTAS:


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'remU': ID, Nome, Idade, Cidade => {V, F}


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'remS': ID, Descricao, Instituicao, Cidade => {V, F}


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'remC': IdU, IdS, Custo => {V, F}





% IDENTIFICAR AS INSTITUIÇÕES PRESTADORAS DE SERVIÇOS:


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'instituicoes': LInstituicoes => {V, F}
instituicoes(L) :- solucoes(Nome, servico(_, _, Nome, _), L).




% IDENTIFICAR UTENTES/SERVIÇOS/CONSULTAS POR CRITÉRIOS DE SELEÇÃO:
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'utenteByNome': Nome, Id => {V, F}
utenteByNome(Nome, R):- solucoes((Id), utente(Id,Nome, _, _), R).
%utenteByNome(N,Id):- utente(Id,N,X,Y).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'utenteByIdade': Idade, Identificador => {V, F}
utenteByIdade(Idade, R):- solucoes((Id), utente(Id, _, Idade, _), R).
%utenteByIdade(I,D):- utente(I,X,D,Y).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'utenteByCidade': Cidade, Identificador => {V, F}
utenteByCidade(Cidade, R):- solucoes((Id), utente(Id, _, _, Cidade), R).
%utenteByCidade(C,I):- utente(I,X,Y,C).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'servicoByDescricao': Descricao, Identificador => {V, F}
servicoByDescricao(Desc, R):- solucoes((Id), servico(Id, Desc, _, _), R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'consultaByData': Data, Resultado => {V, F}
consultaByData(Data, R) :- solucoes((IdU, IdS), consulta(Data, IdU, IdS, _), R).

% IDENTIFICAR SERVIÇOS PRESTADOS POR INSTITUIÇÃO/CIDADE/DATAS/CUSTO:

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'servInstituicao': Instituicao, Resultado => {V, F}
servInstituicao(Instituicao, R) :- solucoes((ID, Nome), servico(ID, Nome, Instituicao, _), R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'servCidade': Cidade, Resultado => {V, F}
servCidade(Cidade, R) :- solucoes((ID, Nome), servico(ID, Nome, _, Cidade), R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'servData': Data, Resultado => {V, F}
servData(Data, R) :- solucoes((ID, Nome), (servico(ID, Nome, _, _), consulta(Data, _, ID, _)), R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'servData': Data, Resultado => {V, F}
servCusto(Custo, R) :- solucoes((ID, Nome), (servico(ID, Nome, _, _), consulta(_, _, ID, Custo)), R).



% IDENTIFICAR OS UTENTES DE UM SERVIÇO/INSTITUIÇÃO:

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'utentesServico': Serviço, Resultado => {V, F}
utentesServico(IdS, R) :- solucoes((IdU, Nome), (consulta(_, IdU, IdS, _) , utente(IdU, Nome, _, _)), R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'utentesInstituicao': Instituição, Resultado => {V, F}
utentesInstituicao(Inst, R) :- solucoes((IdU, Nome), (consulta(_, IdU, IdS, _) , servico(IdS, _, Inst, _) , utente(IdU, Nome, _, _)), R).


% IDENTIFICAR SERVIÇOS REALIZADOS POR UTENTE/INSTITUIÇÃO/CIDADE:

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'servByUtente': ID Utente, Resultado => {V, F}
servByUtente(IdU, R) :- solucoes((IdS, Desc, Inst), (consulta(_, IdU, IdS, _) , servico(IdS, Desc, Inst, _)), R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'servByInstituicao': ID Utente, Resultado => {V, F}
servByInstituicao(Inst, R) :- solucoes((IdS, Desc), servico(IdS, Desc, Inst, _), R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'servByCidade': ID Utente, Resultado => {V, F}
servByCidade(Cidade, R) :- solucoes((IdS, Desc, Inst), (consulta(_, IdU, IdS, _) , utente(IdU, _, _, Cidade) , servico(IdS, Desc, Inst, _)), R).



% CALCULAR O CUSTO TOTAL DOS CUIDADOS DE SAÚDE POR UTENTE/SERVIÇO/INSTITUIÇÃO/DATA:

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'soma' conjunto valores: X, Resultado -> {V, F}
soma([], 0).
soma([H|T], R) :- sum(T, R1) , R is H+R1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'custosByUtente': ID Utente, Resultado => {V, F}
custosByUtente(IdU, R) :- solucoes(Custo, (consulta(_, IdU, IdS, _) , servico(IdS, _, _, Custo)), L). % NÃO ESTÁ ACABADA, FUI JANTAR :)
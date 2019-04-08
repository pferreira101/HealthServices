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
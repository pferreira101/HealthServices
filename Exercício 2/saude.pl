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
:- dynamic idadeIncertaUtente/1.
:- dynamic idadeImprecisaUtente/1.
:- dynamic cidadeIncertaUtente/1.
:- dynamic cidadeImprecisaUtente/1.
:- dynamic excecao/1.
:- dynamic '-'/1.
:- dynamic '::'/2.
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


% Extensao do predicado que define a negacao forte do predicado 'utente'
-utente(ID, Nome, Idade, Morada) :- nao(utente(ID, Nome, Idade, Morada)) , nao(excecao(utente(ID, Nome, Idade, Morada))).
-utente(9, 'Joao', 80, 'Trofa').

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Representação de conhecimento imperfeito relacionado com o predicado 'utente'


%-------------------- Valores nulos do tipo impreciso -------------------
% A Rosa é da Trofa	ou de Vila do Conde	
excecao(utente(10, 'Rosa', 44, 'Trofa')).
excecao(utente(10, 'Rosa', 44, 'Vila do Conde')).
cidadeImprecisaUtente(10).

% O Manel do Porto tem 38 ou 39 anos.
excecao(utente(11, 'Manel', 38, 'Porto')).
excecao(utente(11, 'Manel', 39, 'Porto')).
idadeImprecisaUtente(11).

%---------------- // ------------- // ------------ // -----------------


%--------------------- Valores nulos do tipo incerto --------------------
% E desconhecido o nome de um utente que deu entrada com 60 anos vindo de Barcelos	
utente(12, desconhecido, 60, 'Barcelos').

% Nao se conhece a idade do Mario que vive em Lisboa
utente(13, 'Mario', desconhecido, 'Lisboa').
idadeIncertaUtente(13).

% Nao se conhece a morada da Joana de 24 anos.
utente(14, 'Joana', 24, desconhecido).


excecao( utente( Id, Nome, Idade, Morada ) ) :-
    utente( Id, desconhecido, Idade, Morada ).
excecao( utente( Id, Nome, Idade, Morada ) ) :-
    utente( Id, Nome, desconhecido	, Morada ).
excecao( utente( Id, Nome, Idade, Morada ) ) :-
    utente( Id, Nome, Idade, desconhecido ).

%---------------- // ------------- // ------------ // -----------------


%--------------------- Valores nulos do tipo interdito --------------------
interdito( nome_interdito ).
interdito( idade_interdita ).
interdito( morada_interdita ).

excecao( utente( Id, Nome, Idade, Cidade ) ) :- utente( Id, nome_interdito, Idade, Cidade ).
excecao( utente( Id, Nome, Idade, Cidade ) ) :- utente( Id, Nome, idade_interdita, Cidade ).
excecao( utente( Id, Nome, Idade, Cidade ) ) :- utente( Id, Nome, Idade, morada_interdita ).


% Não se pode saber o nome do utente com 30 anos de Guimarães 
utente(15, nome_interdito, 30, 'Guimarães').
+utente( Id, Nome, Idade, Cidade ) :: (solucoes( Ns ,(utente( 15, Ns, 30, 'Guimarães' ),nao( interdito(Ns) )),S ),
                  comprimento( S,N ), N == 0 
                  ).


% Não se pode saber a idade do Rodrigo que vive em Braga.
utente(16, 'Rodrigo', idade_interdita, 'Braga').
+utente( Id, Nome, Idade, Cidade ) :: (solucoes( Is ,(utente( 16, 'Rodrigo', Is, 'Braga' ),nao( interdito(Is) )),S ),
                  comprimento( S,N ), N == 0 
                  ).


% Não se pode saber a morada do Cristiano de 34 anos.
utente(17, 'Cristiano', 34, morada_interdita).
+utente( Id, Nome, Idade, Cidade ) :: (solucoes( Cs ,(utente( 17, 'Cristano', 34, Cs ), nao( interdito(Cs) ) ), S),
                  comprimento( S,N ), N == 0 
                  ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -


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


% Extensao do predicado que define a negacao forte do predicado 'prestador'
-prestador(ID, Nome, Esp, Inst) :- nao(prestador(ID, Nome, Esp, Inst)) , nao(excecao(prestador(ID, Nome, Esp, Inst))).
-prestador(10, 'Anibal', 'Geral', 'Hospital Santa Maria').

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Representação de conhecimento imperfeito relacionado com o predicado 'prestador'


%-------------------- Valores nulos do tipo impreciso -------------------

% Não se sabe se o prestador Severino trabalha no Hospital Sao Vitor ou no Hospital Sao Joao
excecao(prestador(11, 'Severino', 'Geral', 'Hospital Sao Vitor')).
excecao(prestador(11, 'Severino', 'Geral', 'Hospital Sao Joao')).

% Não se sabe ao certo Especialidade do prestador Joaquim que trabalha no Hospital Sao Vitor. Ou é Neurologia ou Clinica Geral
excecao(prestador(12, 'Joaquim', 'Geral', 'Hospital Sao Vitor')).
excecao(prestador(12, 'Joaquim', 'Neurologia', 'Hospital Sao Vitor')).

%---------------- // ------------- // ------------ // -------------------



%--------------------- Valores nulos do tipo incerto --------------------
% E desconhecido o nome do prestador com ID 12, que trabalha no Hospital Santa Maria em Cardiologia	
prestador(13, desconhecido, 'Cardiologia', 'Hospital Santa Maria').

% Não se sabe a especialidade do prestador Rui que trabalha no Hospital Sao Joao
prestador(14, 'Rui', desconhecido, 'Hospital Sao Joao').


excecao(prestador(Id, Nome, Esp, Inst)) :-
    prestador(Id, desconhecido, Esp, Inst).
excecao(prestador(Id, Nome, Esp, Inst)) :-
    prestador(Id, Nome, desconhecido, Inst).
excecao(prestador(Id, Nome, Esp, Inst)) :-
    prestador(Id, Nome, Esp, desconhecido).

%---------------- // ------------- // ------------ // -------------------

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'cuidado': Ano, Mês, Dia, ID Utente, ID Prestador, Descrição, Custo => {V, F, D}
cuidado(2019, 04, 06, 1, 2, 'Oncologia', 40).
cuidado(2019, 04, 07, 3, 1, 'Geral', 25).
cuidado(2019, 04, 08, 1, 3, 'Cardiologia', 50).



% Extensao do predicado que define a negacao forte do predicado 'cuidado'
-cuidado(Ano, Mes, Dia, IdU, IdP, Desc, Custo) :- nao(cuidado(Ano, Mes, Dia, IdU, IdP, Desc, Custo)), 
													nao(excecao(cuidado(Ano, Mes, Dia, IdU, IdP, Desc, Custo))).
-cuidado(2019, 04, 09, 2, 1, 'Geral', 25).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Representação de conhecimento imperfeito relacionado com o predicado 'cuidado'

%-------------------- Valores nulos do tipo impreciso -------------------
% Não se sabe ao certo o preço do cuidado que o utente 4 realizou no dia 15/4/2019, mas sabe-se que foi 30 ou 35
excecao(cuidado(2019, 04, 15, 4, 3, 'Cardiologia', 30)).
excecao(cuidado(2019, 04, 15, 4, 3, 'Cardiologia', 35)).

%---------------- // ------------- // ------------ // -------------------



%--------------------- Valores nulos do tipo incerto --------------------
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

%---------------- // ------------- // ------------ // -------------------

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% REGISTAR UTENTES, PRESTADORES E CUIDADOS:
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% ----------------- Adição de conhecimento perfeito -------------------

% Extensão do predicado 'registaUtente': ID, Nome, Idade, Cidade -> {V, F}
% Permite passar conhecimento incerto e impreciso a conhecimento perfeito
registaUtente(ID,Nome,Idade,Cidade) :- idadeIncertaUtente(ID), 
									   involucao(utente(ID,Nome,desconhecido,Cidade)), 
									   involucao(idadeIncertaUtente(ID)), 
									   evolucao(utente(ID,Nome,Idade,Cidade)).

registaUtente(ID,Nome,Idade,Cidade) :- idadeImprecisaUtente(ID), solucoes(excecao(utente(ID,Nome,Idades,Cidade)), (excecao(utente(ID,Nome,Idades,Cidade))),R),
                     comprimento(R,N), N>0, removeExcecoes(R), 
                     evolucao(utente(ID,Nome,Idade,Cidade)). 

registaUtente(ID,Nome,Idade,Cidade) :- cidadeIncertaUtente(ID), 
                     involucao(utente(ID,Nome,Idade,desconhecido)), 
                     involucao(cidadeIncertaUtente(ID)), 
                     evolucao(utente(ID,Nome,Idade,Cidade)).
registaUtente(ID,Nome,Idade,Cidade) :-cidadeImprecisaUtente(ID), solucoes(excecao(utente(ID,Nome,Idade,Cidades)), (excecao(utente(ID,Nome,Idade,Cidades))),R),
                     comprimento(R,N), N>0, removeExcecoes(R), 
                     evolucao(utente(ID,Nome,Idade,Cidade)). 


removeExcecoes([]).
removeExcecoes([H|T]) :- involucao(H), removeExcecoes(T).

% Registo novo utente
registaUtente(ID,Nome,Idade,Cidade) :- evolucao(utente(ID,Nome,Idade,Cidade)).
registaNaoUtente(ID,Nome,Idade,Cidade) :- evolucao(-utente(ID,Nome,Idade,Cidade)).

% Extensão do predicado 'registaPrestador': ID, Nome, Especialidade, Instituição -> {V, F}
registaPrestador(ID,Nome,Especialidade,Instituicao):- evolucao(prestador(ID,Nome,Especialidade,Instituicao)).


% Extensão do predicado 'registaCuidado': Ano,Mes,Dia, IDUtente,IDPrestador, Descricao, Custo -> {V, F}
registaCuidado(Ano,Mes,Dia,IDU,IDP,Descricao,Custo):- evolucao(cuidado(Ano,Mes,Dia,IDU,IDP,Descricao,Custo)).


% ----------------- Adição de conhecimento imperfeito -----------------

regUtenteIdadeIncerta(ID,Nome,Cidade) :-  evolucaoIdadeIncerta(utente(ID,Nome,desconhecido,Cidade)).

regUtenteIdadeImprecisa(ID,Nome,[Idade1|T],Cidade):- 
										 evolucaoIdadeImprecisa(utente(ID,Nome,[Idade1|T],Cidade)),
										 assert( idadeImprecisaUtente(ID) ).

regUtenteCidadeIncerta(ID,Nome,Idade) :-  evolucaoCidadeIncerta(utente(ID,Nome,Idade,desconhecido)).

regUtenteCidadeImprecisa(ID, Nome, Idade, [Cidade|T]) :-
										 evolucaoCidadeImprecisa(utente(ID,Nome,Idade, [Cidade|T])),
										 assert( cidadeImprecisaUtente(ID) ).

regUtenteCidadeInterdita(ID,Nome,Idade,Cidade):- evolucaoCidadeInterdita(utente(ID,Nome,Idade,Cidade)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% PREDICADOS EVOLUCAO IMPERFEITA
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

evolucaoIdadeIncerta(utente(ID, Nome, desconhecido, Cidade)) :- 
    evolucaoDesconhecido(utente(ID, Nome, Idade, Cidade)),
    assert(idadeIncertaUtente(ID)).


evolucaoIdadeImprecisa(utente(ID,Nome,[],Cidade)).
evolucaoIdadeImprecisa(utente(ID,Nome,[Idade|T],Cidade)):-
    obtemInvariantes( excecao(utente(ID,Nome,Idade,Cidade)),LI1,LI2),
    insercao(excecao(utente(ID,Nome,Idade,Cidade))),
    teste( LI1 ), teste( LI2 ),
    evolucaoIdadeImprecisa(utente(ID,Nome,T,Cidade)).

evolucaoCidadeIncerta(utente(ID, Nome, Idade, desconhecido)) :- 
    evolucaoDesconhecido(utente(ID, Nome, Idade, Cidade)),
    assert(cidadeIncertaUtente(ID)).

evolucaoCidadeImprecisa(utente(ID, Nome, Idade, [])).
evolucaoCidadeImprecisa(utente(ID, Nome, Idade, [Cidade|T])):-
    obtemInvariantes( excecao(utente(ID,Nome,Idade,Cidade)),LI1,LI2),
    insercao(excecao(utente(ID,Nome,Idade,Cidade))),
    teste( LI1 ), teste( LI2 ),
    evolucaoCidadeImprecisa(utente(ID, Nome, Idade, T)).

evolucaoCidadeInterdita(utente(ID,Nome,Idade,CidadeInterdita)):-
	obtemInvariantes( utente(ID,Nome,Idade,CidadeInterdita), LI1, LI2 ),
	insercao( utente(ID,Nome,Idade,CidadeInterdita) ),
	teste( LI1 ), teste( LI2 ),
	assert( interdito( CidadeInterdita ) ),
	assert( (excecao(utente(ID1,Nome1,Idade1,Cidade1)) :- utente(ID1,Nome1,Idade1,CidadeInterdita)) ),
	assert((+utente(ID1,Nome1,Idade1,Cidade1) :: (
				       				solucoes(Cidade1,( utente(ID,Nome,Idade,Cidade1), nao( interdito(Cidade1) ) ),S),
				       				comprimento(S,0)
				     			))).

evolucaoDesconhecido(Termo) :-
    obtemInvariantes( Termo, LI1, LI2 ),
    insercao(Termo),
    teste( LI1 ), teste( LI2 ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes de Conhecimento imperfeito
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Nao é permitido adiciona conhecimento imperfeito a factos que já estão caracterizados como imperfeito
+utente(ID, Nome, Idade, Cidade) ::: nao(idadeImprecisaUtente(ID)).

+utente(ID, Nome, Idade, Cidade) ::: nao(idadeIncertaUtente(ID)).

+utente(ID, Nome, Idade, Cidade) ::: nao(cidadeImprecisaUtente(ID)).

+utente(ID, Nome, Idade, Cidade) ::: nao(cidadeIncertaUtente(ID)).

+utente(ID, Nome, Idade, Cidade) ::: nao(interdito(Cidade)).

+excecao(utente(ID,Nome,Idade,Cidade)) ::: nao(idadeImprecisaUtente(ID)).

+excecao(utente(ID,Nome,Idade,Cidade)) ::: nao(idadeIncertaUtente(ID)).

+excecao(utente(ID,Nome,Idade,Cidade)) ::: nao(cidadeImprecisaUtente(ID)).

+excecao(utente(ID,Nome,Idade,Cidade)) ::: nao(cidadeIncertaUtente(ID)).

+excecao(utente(ID,Nome,Idade,Cidade)) ::: nao(interdito(Cidade)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes Estruturais
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Não permitir conhecimento repetido
+utente(ID,Nome,I,M) :: (solucoes(ID,desconhecido(ID),R),
                            comprimento(R,N),
                            N >= 0, N =< 1).

+utente(ID, Nome, I, M) :: (solucoes((ID, Nome, I, M), (utente(ID, Nome, I, M)), R1),
                            comprimento(R1,N1),
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
%+utente(ID, Nome, Idade, Morada) :: Idade >= 0.


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
imp(desconhecido, Q, desconhecido) :- Q \= verdadeiro.
imp(desconhecido, verdadeiro, verdadeiro).



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
% Extensao do precicado que pertime obter as listas de invariantes para um determinado termo
obtemInvariantes(Termo, LI1, LI2) :-
	solucoes(Invariante, +Termo:::Invariante, LI1),
	solucoes(Invariante, +Termo::Invariante, LI2).

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

% Extensão do predicado 'utente': ID, Nome, Idade, Cidade => {V, F}
utente(1, pedro, 20, famalicao).
utente(2, nelson, 35, gaia).
utente(3, miguel, 28, barcelos).
utente(4, henrique, 10, braga).
utente(5, rui, 65, famalicao).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'servico': ID, Descrição, Instituição, Cidade => {V, F}

servico(1, geral, sjoao, porto).
servico(2, oncologia, svitor, braga).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado 'consulta': ID Utente, ID Serviço, Custo => {V, F}
consulta(1, 2, 40).



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
% Entensão do predicado 'instituicoes': LServiços, LInstituicoes => {V, F}





% IDENTIFICAR UTENTES/SERVIÇOS/CONSULTAS POR CRITÉRIOS DE SELEÇÃO:






% IDENTIFICAR SERVIÇOS PRESTADOS POR INSTITUIÇÃO/CIDADE/DATAS/CUSTO:





% IDENTIFICAR OS UTENTES DE UM SERVIÇO/INSTITUIÇÃO:





% IDENTIFICAR SERVIÇOS REALIZADOS POR UTENTE/INSTITUIÇÃO/CIDADE:





% CALCULAR O CUSTO TOTAL DOS CUIDADOS DE SAÚDE POR UTENTE/SERVIÇO/INSTITUIÇÃO/DATA:
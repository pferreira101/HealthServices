
% Extensão do predicado 'utente': ID, Nome, Idade, Cidade => {V, F}
utente(1, pedro, 20, famalicao).
utente(2, nelson, 35, gaia).
utente(3, miguel, 28, barcelos).
utente(4, henrique, 10, braga).


% Extensão do predicado 'servico': ID, Descrição, Instituição, Cidade => {V, F}
servico(1, geral, sjoao, porto).
servico(2, oncologia, svitor, braga).


% Extensão do predicado 'consulta': ID Utente, ID Serviço, Custo => {V, F}
consulta(1, 2, 40).
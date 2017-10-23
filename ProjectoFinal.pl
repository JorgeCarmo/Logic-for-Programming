/**/

%%% Grupo numero 7
%%% Miguel Antao Pereira Amaral N: 78865
%%% Jorge Miguel Pereira do Carmo N: 79702

%%% transformacao/2
%%% transformacao(C1, C2) em que C1 e C2 sao configuracoes representadas por listas
%%% por exemplo
%%% ?- transformacao([1, 2, 3, 4, 5, 6, 7, 8, 0], [1, 0, 2, 4, 5, 3, 7, 8, 6]).
%%% Transformacao desejada:
%%%  1  2  3      1     2 
%%%  4  5  6  ->  4  5  3 
%%%  7  8         7  8  6 
%%% true .

transformacao([A, B, C, D, E, F, G, H, I],[J, K, L, M, N, O, P, Q, R]) :-	write('Transformacao desejada:'), nl, 
																			escreve(A), escreve(B), escreve(C),  
																			write('    '), 
																			escreve(J), escreve(K), escreve(L),nl, 
																			escreve(D), escreve(E), escreve(F), 
																			write(' -> '), 
																			escreve(M), escreve(N), escreve(O), nl,
																			escreve(G), escreve(H), escreve(I), 
																			write('    '), 
																			escreve(P), escreve(Q), escreve(R).

																			

%%% escreve/1 e um predicado auxiliar de transformacao/2
%%% a primeira regra permite escrever uma configuracao
%%% por exemplo
%%% ?- escreve([1, 2, 3, 4, 5, 6, 7, 8, 0]).
%%%  1  2  3 
%%%  4  5  6 
%%%  7  8    
%%% true .

escreve([A, B, C, D, E, F, G, H, I]) :- escreve(A), escreve(B), escreve(C), nl,
                                        escreve(D), escreve(E), escreve(F), nl,
                                        escreve(G), escreve(H), escreve(I), nl.

escreve(S) :- S = 0, write('   ').
escreve(S) :- S < 10, write(' '), write(S), write(' ').



%%% escreve_solucao/1
%%% escreve_solucao(M) em que M e uma lista de movimentos e um movimento e um par (Mov, Peca) 
%%% por exemplo
%%% ?- escreve_solucao([(b, 6), (b, 3), (d, 2)]).
%%% mova a peca 6 para baixo
%%% mova a peca 3 para baixo
%%% mova a peca 2 para a direita.
%%% true .

escreve_solucao([(M, P) | []]) :- write('mova a peca '), 
                                  write(P), 
                                  traduz(M, Mp), 
                                  write(Mp),
                                  write('.'),
                                  nl.

escreve_solucao([(M, P) | R]) :- write('mova a peca '), 
                                 write(P), 
                                 traduz(M, Mp), 
                                 write(Mp),
                                 nl, 
                                 escreve_solucao(R).


								 
%%% traduz/2 e um predicado auxiliar de escreve_solucao/1

traduz(c, ' para cima').
traduz(b, ' para baixo').
traduz(e, ' para a esquerda').
traduz(d, ' para a direita').



%%% elementOfIndex/4
%%% elementOfIndex(Lista,Ele,Posicao,Aux) em que: 
%			-- Lista   -> Lista Procurada
%			-- Ele     -> Elemento a procurar na lista
%			-- Posicao -> Elemento a procurar na lista
%			-- Aux     -> Variavel usada pela funcao
%
%%% Exemplos:
%%% Pode devolver o indice do elemento a procurar
%%% ?- elementOfIndex([a,b,c,d,e],d,Solution,1).
%%% Solution = 4 
%
%%% Pode devolver o elemento no indice a procurar
%%% ?- elementOfIndex([a,b,c,d,e],Solution,5,1).
%%% Solution = e 
%
%%% Pode ser usado como predicado 
%%% (o terceiro elemento e' o c?)
%%% ?- elementOfIndex([a,b,c,d,e],c,3,1).
%%% true.
%
%%% (o quarto elemeto e' o c?)
%%% ?- elementOfIndex([a,b,c,d,e],c,4,1).
%%% true.

elementOfIndex([Beggining|_],Beggining,Search,Search):-!.
elementOfIndex([_|Rest],Return,Procura,Inc):- Sum is Inc + 1,
    									elementOfIndex(Rest,Return,Procura,Sum).

										
										
%%% junta/5
%%% junta(ListaPrimaria,ListaSecundaria,Final) em que:
%			-- ListaPrimaria    -> Lista que aparece primeiro
%			-- ListaSecundaria  -> Lista a adicionar
%			-- Final            -> Listas concatenadas
%%% concatena a ListaPrimaria com a ListaSecundaria
%%% Exemplo:
%%% ?- junta([1,3,4,2],[a,b],L).
%%% L = [1, 3, 4, 2, a, b].

junta([],L,L).
junta([P|R],L,[P|L2]):-junta(R,L,L2).



%%% substitui/6
%%% substitui(Lista,OldEle,Aux,NewEle,Ac,Final) em que:
%			-- Lista   -> Lista a ser modificada
%			-- OldEle  -> Elemento a substituir
%			-- NewEle  -> Elemento pelo qual e substituido
%			-- Aux     -> Variavel usada pela funcao 
%			-- Ac      -> Variavel usada pela funcao para criar nova Lista
%			-- Final   -> Variavel na qual sai Lista nova
%
%%% Substitui todas as ocorrencias de OldEle por NewEle na Lista.
%%% Exemplo:
%%% ?- substitui([1,2,3,4,5,6,7,8,0],3,1,25,[],Solucao).
%%% Solucao = [1, 2, 25, 4, 5, 6, 7, 8, 0]

substitui([],_,_,_,Ac,Ac).
substitui([Inicio|Resto],Ind,Ind_Actual,Val,Ac,X) :-Ind \= Ind_Actual,New is Ind_Actual+1,
    												junta(Ac,[Inicio],Acumulated),
    												substitui(Resto,Ind,New,Val,Acumulated,X);
    												
    												Ind =:= Ind_Actual,New is Ind_Actual+1,
    												junta(Ac,[Val],Acumulated),
    												substitui(Resto,Ind,New,Val,Acumulated,X).

													
													
% mov_legal([1, 2, 3, 4, 5, 0, 7, 8, 6], M, P, C).
%%% mov_legal/4
%%% mov_legal(TI, M, P, TF) em que:
%			-- TI   -> Tabuleiro Inicial		(Lista)
%			-- M    -> Movimento
%			-- P    -> Peca
%			-- TF   -> Tabuleiro apos movimento (Lista)
%%% Predicado que verifica se mover P na direcao M transforma TI em TF
% ?- mov_legal([1, 2, 3, 4, 5, 0, 7, 8, 6], c, 6, [1, 2, 3, 4, 5, 6, 7, 8, 0]).
% true
%%% Devolve movimentos possiveis a partir de TI
% mov_legal([1, 2, 3, 4, 5, 0, 7, 8, 6], M, P, C).
% M = c,
% P = 6,
% C = [1, 2, 3, 4, 5, 6, 7, 8, 0] ;
% M = b,
% P = 3,
% C = [1, 2, 0, 4, 5, 3, 7, 8, 6] ;
% M = d,
% P = 5,
% C = [1, 2, 3, 4, 0, 5, 7, 8, 6] ;
% false.

mov_legal(C1, M, P, C2) :- 	M = c,
								elementOfIndex(C1,0,Indice,1),
								Indice<7, %entao e valido o movimento cima
								Indice_P is Indice+3,
								elementOfIndex(C1,P,Indice_P,1), %descoberta peca
								substitui(C1,Indice,1,P,[],C_Alt),substitui(C_Alt,Indice_P,1,0,[],C2);
							M = b,
								elementOfIndex(C1,0,Indice,1),
								Indice>3, %entao e valido o movimento baixo
								Indice_P is Indice-3,
								elementOfIndex(C1,P,Indice_P,1), %descoberta peca
								substitui(C1,Indice,1,P,[],C_Alt),substitui(C_Alt,Indice_P,1,0,[],C2);
							M = e,
								elementOfIndex(C1,0,Indice,1),
								Remainder is mod(Indice,3),
								Remainder > 0, %entao e valido o movimento esquerda
									Indice_P is Indice+1,
									elementOfIndex(C1,P,Indice_P,1), %descoberta peca
									substitui(C1,Indice,1,P,[],C_Alt),substitui(C_Alt,Indice_P,1,0,[],C2);
							M = d,
								elementOfIndex(C1,0,Indice,1),
								Remainder is mod(Indice,3),
								Remainder \= 1, %entao e valido o movimento direita
									Indice_P is Indice-1,
									elementOfIndex(C1,P,Indice_P,1), %descoberta peca
									substitui(C1,Indice,1,P,[],C_Alt),substitui(C_Alt,Indice_P,1,0,[],C2).

									
									
%%% resolve_manual/2
%%% resolve_manual(C1,C2) em que C1 e C2 sao configuracoes representadas por listas
%%% resolve_manual cria a interface do jogo puzzle 8, que permite ao utilizador tentar  
%%% transformar o tabuleiro c1 no c2
resolve_manual(C1,C2) :- 	transformacao_possivel(C1,C2),
							transformacao(C1,C2),
							manual(C1,C2).
							
							
%%% manual/2
%%% predicado auxiliar de resolve_manual/2							
manual(C1,C2)	:-	C1 = C2,
						nl,
						write('Parabens!'),
						nl
					;	
						nl,
						write('Qual o seu movimento?'),
						read(M),
						nl,
						mov_legal(C1, M, _, After),
						nl,
						escreve(After),
						manual(After,C2);
						write('Movimento ilegal'),
						manual(C1,C2). %Retry
						
						

%%% resolve_cego/2
%%% resolve_cego(C1,C2) em que C1 e C2 sao configuracoes representadas por listas
%%% resolve_cego imprime uma solucao que transforme C1 em C2
%%% esta solucao e quase sempre muito ineficiente
%%% Exemplo:
% ?- resolve_cego([0, 1, 3, 4, 2, 5, 7, 8, 6], [1, 2, 3, 4, 5, 6, 7, 8, 0]).
% Transformacao desejada:
%    1  3      1  2  3 
% 4  2  5  ->  4  5  6 
% 7  8  6      7  8    
% mova a peca 4 para cima
% mova a peca 7 para cima
% (...298 Linhas...) 
% mova a peca 5 para baixo
% mova a peca 2 para a esquerda
% mova a peca 3 para cima
% mova a peca 6 para cima.
% true.

resolve_cego(TInicial, TFinal) :- 	transformacao_possivel(TInicial,TFinal),
									transformacao(TInicial, TFinal), 
									nl,
									procura_cega(TInicial, TFinal,[TInicial],Solucao),
									escreve_solucao(Solucao),
									!.


%%% procura_cega/4 
%%% procura_cega(TActual, Objectivo, ListaTab, ListaMovimentos) em que:
%			-- TActual    		  -> Tabuleiro que vai sofrer proximo movimento
%			-- Objectivo   		  -> Tabuleiro ao qual queremos chegar
%			-- ListaTab			  -> Lista de Tabuleiros ja gerados
%			-- ListaMovimentos    -> Lista com os movimentos ja efectuados
%%% procura_cega/4 e um predicado auxiliar de resolve_cego/2

procura_cega(X,X,_,_):-!.
procura_cega(TActual, TF, LTab, [Movimento|R]) :- 	mov_legal(TActual, Dir, Peca, TGer),
													novo_tab(TGer,LTab),
													junta(LTab,[TGer], TabJuntas),
													procura_cega(TGer,TF,TabJuntas, R),
													!,
													Movimento = (Dir,Peca).

													
													
%%% novo_tab/2
%%% novo_tab(Tabuleiro, ListaTabuleiros) em que:
%			-- Tabuleiro 		  -> Tabuleiro a verificar
%			-- ListaTabuleiros	  -> Lista na qual e procurado
%%% novo_tab/2 e um predicado que returna true caso um tabuleiro nao faca 
%%% parte de uma lista de tabuleiros

novo_tab(_,[]) :- !.
novo_tab(Tabuleiro,[P|R]):- P \= Tabuleiro,
							novo_tab(Tabuleiro,R).
						
												
%%% Cada momento do tabuleiro na procura inteligente e representado por um No:
% No - (Tabuleiro, FuncaoTotal, N_Mov, Ham, ListaMov) em que:
%			-- Tabuleiro 		  -> Representacao do Tabuleiro em lista
%			-- FuncaoTotal 		  -> Segundo heuristica o valor de  prioridade
%			-- N_Mov 		  	  -> Num de movimentos anteriores
%			-- Ham	 		  	  -> Valor de Hamming
%			-- ListaMov	 		  -> Movimentos realizados ate chegar ao Tabuleiro
%%% o valor de prioridade e calculado:
% FuncaoTotal = (N_Mov + Ham)											
			

%%% resolve_info_h/2	
%%% resolve_info_h(C1,C2) em que C1 e C2 sao configuracoes representadas por listas
%%% resolve_info_h imprime uma solucao que transforme C1 em C2
%%% esta solucao e eficiente
%%% Exemplo:
% ?- resolve_info_h([0, 1, 3, 4, 2, 5, 7, 8, 6], [1, 2, 3, 4, 5, 6, 7, 8, 0]).
% Transformacao desejada:
%     1  3      1  2  3 
%  4  2  5  ->  4  5  6 
%  7  8  6      7  8    
% mova a peca 1 para a esquerda
% mova a peca 2 para cima
% mova a peca 5 para a esquerda
% mova a peca 6 para cima.
% true.							
resolve_info_h(TInicial, TFinal) :- transformacao_possivel(TInicial,TFinal),
									transformacao(TInicial, TFinal),
									nl,
									calc_h(TInicial, TFinal,H),							
									resolve_info_aux((TInicial,H,0,H,[]),TFinal,[],[(TInicial,H,0,H,[])],Solucao),
									escreve_solucao(Solucao),
									!.
									
%%% resolve_info_aux/4
%%% Recebe o no actual, o tabuleiro final, a lista de abertos e a lista de fechados e retorna a lista de movimentos necessaria para terminar o jgoo
%%% resolve_info_aux(No_actual, TabFinal,Lista_Abertos_actual,Lista_Fechados_ini,Movimentos) em que Lista_Abertos_actual e Lista_Fechados_ini e uma lista de Nos, 
%%% No_actual e uma lista do tipo No, TabFinal e uma lista do tipo tabuleiro e Movimentos e uma lista de movimentos
%%% TabFinal e uma lista do tipo tabuleiro e Dir e uma lista de letras
%%% por exemplo
%%% ?-resolve_info_aux(([0, 1, 3, 4, 2, 5, 7, 8, 6],H,0,H,[]), [1, 2, 3, 4, 5, 6, 7, 8, 0] ,[],[([0, 1, 3, 4, 2, 5, 7, 8, 6],H,0,H,[])],Movimentos).
%%% Movimentos = [ (e, 1), (c, 2), (e, 5), (c, 6)].
resolve_info_aux((TabFinal,_,_,_,Movimentos), TabFinal,_,_,Movimentos) :- !.								
resolve_info_aux(No_actual, TabFinal,Lista_Abertos_actual,Lista_Fechados_ini,Movimentos) :- 		mov_possiveis(TabFinal,No_actual,[],Lista_nos,[d,e,b,c]),
																									juntar_abertos(Lista_Abertos_actual,Lista_Fechados_ini,Lista_nos,Lista_Abertos_actualizada),
																									!,
																									aberto_mais_pequeno(Lista_Abertos_actualizada,Lista_Abertos_final,No_removido),
																									junta(Lista_Fechados_ini,[No_removido],Lista_Fechados_final),
																									resolve_info_aux(No_removido,TabFinal,Lista_Abertos_final,Lista_Fechados_final,Movimentos).

																
																
%%% mov_possiveis/5
%%% retorna os movimentos que sao possiveis apartir do no No
%%% mov_possiveis(TabFinal, No, Lista_nos_entrada, Lista_nos_saida, Dir) em que Lista_nos_entrada e Lista_nos_saida e uma lista de Nos, No e uma lista do tipo No
%%% TabFinal e uma lista do tipo tabuleiro e Dir e uma lista de letras
%%% por exemplo
%%% ?- mov_possiveis([1,2,3,4,5,6,7,8,0],([1,2,3,4,5,6,0,7,8], 2, 0, 2, []), [],Lista,[c,b,d,e]).
%%% tLista = [ ([1, 2, 3, 0, 5, 6, 4|...], 4, 1, 3, [ (b, 4)]), ([1, 2, 3, 4, 5, 6|...], 2, 1, 1, [ (e, 7)])] 
mov_possiveis(_,(_, _, _, _,_), Lista_nos,Lista_nos, []) .
mov_possiveis(TabFinal,(Tab, _, Num_mov, _, Lista_mov_ate), Lista_nos_entrada, Lista_nos_saida, [Dir|R]) :-	mov_legal(Tab, Dir, Peca, TGer) ->    
																																				calc_h(TGer,TabFinal,H),
																																				Soma is H + Num_mov + 1,
																																				junta(Lista_mov_ate,[(Dir,Peca)],Movimentos_juntos),
																																				New_Num_mov is Num_mov + 1,
																																				junta(Lista_nos_entrada,[(TGer,Soma,New_Num_mov,H,Movimentos_juntos)],Lista_nos_actualizada),
																																				mov_possiveis(TabFinal,(Tab, _, Num_mov, _, Lista_mov_ate),  Lista_nos_actualizada, Lista_nos_saida, R)
																																			;
																																				mov_possiveis(TabFinal,(Tab, _, Num_mov, _, Lista_mov_ate),Lista_nos_entrada,  Lista_nos_saida, R).
%%% no_na_lista/2
%%% Devolve true caso o no esteja na lista 	
%%% no_na_lista(No_a_verificar, Lista) em que Lista e uma lista de Nos e No_a_verificar e uma lista do tipo No
%%% por exemplo
%%% ?- no_na_lista(([1,2,3,4,5,6,7,8,0],1,2,3,[(d,3)]),[([1,2,3,4,5,6,8,7,0],1,1,2,[(d,3)]),([1,2,3,4,5,6,8,7,0],1,1,2,[(d,3),(b,5)])]).
%%% true
no_na_lista(_,[]).
no_na_lista((Tab,_,_,_,_),[(First_tab,_,_,_,_)|Resto]):-	First_tab \= Tab, 
															no_na_lista((Tab,_,_,_,_),Resto).
																

%%% juntar_abertos/2
%%% recebe uma lista de Nos e adiciona a Lista_abertos_actual se os Nos nao estiverem na Lista_abertos_actual ou na Lista_fechados
%%% juntar_abertos(Lista_abertos_actual, Lista_Fechados, ListaNos, Lista_abertos_final) 
%%% em que Lista_abertos_actual, Lista_Fechados, ListaNos e Lista_abertos_final sao listas de Nos
%%% por exemplo
%%% ?- juntar_abertos([],[],[ ([1, 2, 3, 0, 5, 6, 4,7,8], 4, 1, 3, [ (b, 4)]), ([1, 2, 3, 4, 5, 6,7,0,8], 2, 1, 1, [ (e, 7)])] ,ListaRecebida).
%%% ListaRecebida = [ ([1, 2, 3, 0, 5, 6, 4|...], 4, 1, 3, [ (b, 4)]), ([1, 2, 3, 4, 5, 6|...], 2, 1, 1, [ (e, 7)])] 																							
juntar_abertos(Lista_abertos, _, [],Lista_abertos).															
juntar_abertos(Lista_abertos_actual, Lista_Fechados, [Primeiro_no|Resto], Lista_abertos_final) :- 
																no_na_lista(Primeiro_no,Lista_abertos_actual),
																no_na_lista(Primeiro_no,Lista_Fechados),
																junta(Lista_abertos_actual,[Primeiro_no],Lista_abertos_actualizada),
																juntar_abertos(Lista_abertos_actualizada, Lista_Fechados, Resto, Lista_abertos_final)
																;
																%Caso alguma das condicoes impostas em cima falhe
																juntar_abertos(Lista_abertos_actual, Lista_Fechados, Resto, Lista_abertos_final).
																
								
%%% aberto_mais_pequeno/3
%%% escolhe o No com valor de FuncaoTotal mais pequeno
%%% aberto_mais_pequeno(No, Lista_Abertos_final,No_removido) em que No e No_removido sao lista do tipo no e Lista_Abertos_final e uma lista de Nos
%%% por exemplo
%%% ?- aberto_mais_pequeno([([1,2,3,4,5,6,7,8,0],2,3,4,[(d,3)]),([1,2,3,4,5,6,7,8,0],2,4,4,[(d,3)]),([1,2,3,4,5,6,7,8,0],2,5,4,[(d,3)]),([1,2,3,4,5,6,7,8,0],2,1,4,[(d,3)])],_,No).
%%% No = ([1, 2, 3, 4, 5, 6, 7, 8|...], 2, 1, 4, [ (d, 3)]).
aberto_mais_pequeno([(Tabuleiro, FuncaoTotal, N_Mov, Ham, ListaMov)|Resto],Lista_Abertos_final,No_removido) :- 	aberto_aux(Resto, FuncaoTotal,(Tabuleiro, FuncaoTotal, N_Mov, Ham, ListaMov),No_removido),
																												remove([(Tabuleiro, FuncaoTotal, N_Mov, Ham, ListaMov)|Resto],No_removido,Lista_Abertos_final).


%%% remove/3
%%% remove o elemento Ele da lista Lista
%%% remove(Lista,Ele,Final) em que Lista, Ele e Final sao listas
%%% por exemplo
%%% ?- remove([([1,2,3,4,5,6,7,8,0],2,3,4,[(d,3)]),([1,2,3,4,5,6,7,8,0],2,4,4,[(d,3)])],([1,2,3,4,5,6,7,8,0],2,3,4,[(d,3)]),Final).
%%% Final = [ ([1, 2, 3, 4, 5, 6, 7|...], 2, 4, 4, [ (d, 3)])].
remove(Lista,Ele,Final) :- 	remove_aux(Lista,Ele,[],Final).																										
remove_aux([],_,Lista,Lista).
remove_aux([Primeiro|Resto],Ele,Lista,Final) :- Primeiro = Ele ->
													remove_aux(Resto,Ele,Lista,Final)
												;
													junta(Lista,[Primeiro],Lista_Junta),
													remove_aux(Resto,Ele,Lista_Junta,Final).


%%% aberto_aux/4
%%% verifica se Func e maior que Valor e retorna NewValue caso o seja
%%% aberto_aux(ListaNos, Func_minimo,No_Entrada, No_removido) em que ListaNos e uma lista,
%%%	Func_minimo e um numero e No_Entrada e No_removido sao listas
%%% por exemplo
%%% ?- aberto_aux([([8,0,1,2,5,4,7,3,6], 5, 2, 3, [(d,3)]), ([4,3,1,2,6,8,7,0,5], 3, 1, 2, [(c,4)])], 4, ([8,0,1,2,5,4,7,3,6], 5, 2, 3, [(d,3)]), No_removido).
%%% No_removido = ([4, 3, 1, 2, 6, 8, 7, 0|...], 3, 1, 2, [ (c, 4)]).		
aberto_aux([],_, No_removido, No_removido).
aberto_aux([Primeiro_No|Resto], Func_minimo,No_Entrada, No_removido) :- no_e_minimo(Primeiro_No,Func_minimo,NewMinimo) -> 
																				aberto_aux(Resto, NewMinimo , Primeiro_No, No_removido)
																			;
																				aberto_aux(Resto, Func_minimo, No_Entrada, No_removido).
																					
%%% no_e_minimo/3
%%% verifica se Func e maior que Valor e retorna NewValue caso o seja
%%% no_e_minimo(No, Valor, NewValue) em que No e uma lista e Valor e NewValue sao numeros
%%% por exemplo
%%% ?- no_e_minimo(([8,0,1,2,5,4,7,3,6], 5, 2, 3, [(d,3)]), 6, NewValue).
%%% NewValue = 5					
no_e_minimo((_, Func, _, _, _),Valor,NewValue) :- 	Func @> Valor ->	fail
																	;
																		NewValue is Func.
%%% calc_h/3
%%% calcula a distancia de hamming entre a lista TI e a lista TF
%%% calc_h(TI, TF, H) em que TI e TF sao listas e H e um numero
%%% por exemplo
%%% ?- calc_h([8,0,1,2,5,4,7,3,6],[3,0,7,2,8,6,1,5,4], H).
%%% H = 7					
calc_h([],[],0).
calc_h([0|TI_R],[_|TF_R],H) :- 	calc_h(TI_R, TF_R, H).			
calc_h([Tab|TI_R],[Tab|TF_R],H) :- 	calc_h(TI_R, TF_R, H).			
calc_h([_|TI_R], [_|TF_R], H) 	:- 	calc_h(TI_R, TF_R, H_Aux),
									H is H_Aux + 1.

%%% transformacao_possivel/2
%%% verifica se e possivel transformar o tabuleiro TI no tabuleiro TF
%%% transformacao_possivel(TI, TF) em que TI e TF sao listas 
%%% por exemplo
%%% ?- transformacao_possivel([8,0,1,2,5,4,7,3,6],[3,0,7,2,8,6,1,5,4]).
%%% true																			
transformacao_possivel(TI,TF) :-	contarInversoes(TI,Numero1),
									contarInversoes(TF,Numero2),
									!,
    								0 is Numero1 mod 2,
									0 is Numero2 mod 2.
									
%%% contarInversoes/2
%%% conta o numero de inversoes presentes num tabuleiro
%%% contarInversoes(Lista, Soma) em que Lista e uma lista de numeros e Soma e um numero
%%% por exemplo
%%% ?- contarInversoes([8,2,3,4,5,6,7,1,0],Soma).
%%% Soma = 13																
contarInversoes([],0).
contarInversoes([P|R],Soma) :-  contarInversoes(R,Soma_aux),
								verdadeiraContagem(P,R,NumMenores),
								Soma is NumMenores + Soma_aux.


%%% verdadeiraContagem/2
%%% conta o numero de valores na lista menor que Numero
%%% verdadeiraContagem(Numero, Lista, Soma) em que Numero e um numeros, Lista e uma lista de numeros e Soma e um numero
%%% por exemplo
%%% ?- verdadeiraContagem(3,[1,2,3,4,5,6],Soma).
%%% Soma = 2							
verdadeiraContagem(0,_,0).
verdadeiraContagem(_,[],0).
verdadeiraContagem(Numero,[0|R],Soma)	:- 	verdadeiraContagem(Numero,R,Soma).
verdadeiraContagem(Numero,[P|R],Soma) :-	verdadeiraContagem(Numero,R,Valor),
											maior(Numero, P, Retorno),
											Soma is Valor + Retorno.

%%% maior/3
%%% Retorna 1 se Num1 e maior que Num2 ou 0 caso contrario
%%% maior(Num1, Num2, Retorno) em que Num1 e Num2 sao numeros e Retorno e 1 ou 0
%%% por exemplo
%%% ?- maior(3,2, Retorno).
%%% Retorno = 1
maior(Num1, Num2, Retorno) :- 	Num1>Num2,
    							Retorno is 1;
    							Retorno	is 0.
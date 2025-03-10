{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Luís António Peixoto Soares <a106932@alunos.uminho.pt>
              Kevin Martins Nogueira <a106905@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324

{- |

Nesta tarefa, temos como objetivo definir duas funçẽs:

* __colisoesParede__ que verifica se um personagem ultrapassa os limites do mapa ou colide com certos blocos do mapa;

Para definir esta função fizemos uso de duas funções auxiliares, sendo elas: __colisaoHitboxparede__, que teste se a hitbox de um personagem colide com as hitboxes de certos blocos do mapa, e __testePassalimite__, que testa se a hitbox de um personagem passa dos limites ou sai fora do mapa.


* __colisoesPersonagens__ que verifica se dois personagens estão em colisão.

Para definir esta função usamos uma função auxiliar: __colisaoHitHit__, que testa se duas hitboxes quaisqueres colidem.

Para obtermos as hitboxes necessárias para definir as funções principais usamos outras duas funções auxiliares:

1. __criaHitbox__, que dado um personagem cria a hitbox correspondente;

2. __hitboxMatriz__, que dado uma matriz de blocos, nomeadamente o mapa, dá as hitboxes dos blocos do tipo __Plataforma__ e __Alcapao__.

= Funções:
== Funções Principais:
prop> colisoesParede :: Mapa -> Personagem -> Bool
prop> colisoesParede (Mapa _ _ l) p = colisaoHitboxparede hitboxp lp || testePassalimite l hitboxp
prop>                   where hitboxp = criaHitbox p
prop>                         lp = hitboxMatriz l
prop> colisoesPersonagens :: Personagem -> Personagem -> Bool
prop> colisoesPersonagens p1 p2 = colisaoHitHit hitboxp1 hitboxp2
prop>                   where hitboxp1 = criaHitbox p1
prop>                         hitboxp2 = criaHitbox p2

== Função auxiliar __criaHitbox__:
prop> criaHitbox :: Personagem -> Hitbox
prop> criaHitbox personagem = ((x-l/2, y+c/2), (x+l/2, y-c/2))
prop>          where (x, y) = posicao personagem
prop>                (c, l) = tamanho personagem

== Função auxiliar __testePassalimite__:
prop> testePassalimite :: [[Bloco]] -> Hitbox -> Bool
prop> testePassalimite l ((x1,y1),(x2,y2)) = x1 < 0 || x2 > nColunas || y1 > nLinhas || y2 < 0
prop>                           where nLinhas = fromIntegral (length l)
prop>                                 nColunas = fromIntegral (length (head l)) 

== Função auxiliar __colisaoHitHit__ :
prop> colisaoHitHit :: Hitbox -> Hitbox -> Bool
prop> colisaoHitHit ((x1,y1),(x1',y1')) ((x2,y2),(x2',y2')) = if x1 < x2' && x1' > x2 && y1' < y2 && y1 > y2' then True else False  

== Função auxiliar __colisaoHitboxparede__:
prop> colisaoHitboxparede :: Hitbox -> [Hitbox] -> Bool
prop> colisaoHitboxparede hit [] = False
prop> colisaoHitboxparede hit (h:t) = if colisaoHitHit hit h then True
prop>                                 else colisaoHitboxparede hit t


= Exemplos de utiilização:
>>> colisaoHitHit ((3,6),(6,3)) ((5,4),(4,5))
True

>>> colisaoHitHit ((3,6),(6,3)) ((-1,-1),(-1,-1))
False

>>> criaHitbox (Personagem (5, -1) Jogador (0.5, 0.5) Oeste (1, 1) True True 2 0 (True, 5))
((4.5,-1.5),(5.5,-0.5))
-}


colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede (Mapa _ _ l) p = colisaoHitboxparede hitboxp lp || testePassalimite l hitboxp
                  where hitboxp = criaHitbox p
                        lp = hitboxMatriz l

-- | Função Principal.
colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens p1 p2 = colisaoHitHit hitboxp1 hitboxp2
                  where hitboxp1 = criaHitbox p1
                        hitboxp2 = criaHitbox p2


-- | Função auxiliar usada para obter a hitbox de um personagem em funcão da posição e tamanho.
criaHitbox :: Personagem -> Hitbox
criaHitbox personagem = ((x-l/2, y+c/2), (x+l/2, y-c/2))
         where (x, y) = posicao personagem
               (c, l) = tamanho personagem


-- | Funções auxiliares usadas para obter as hitboxes dos blocos __Plataforma__ e __Alcapao__ de uma matriz de blocos.
hitboxMatriz :: [[Bloco]] -> [Hitbox]
hitboxMatriz [] = []
hitboxMatriz l = concatMap hitboxLinha (zip [1..] l)

-- | Função auxiliar da função __hitboxMatriz__.
hitboxLinha :: (Int, [Bloco]) -> [Hitbox]
hitboxLinha (m, linha) = concatMap (\(n, bloco) -> hitboxBloco m n bloco) (zip [1..] linha)

-- | Função auxiliar da função __hitboxMatriz__.           
hitboxBloco :: Int -> Int -> Bloco -> [Hitbox]
hitboxBloco m n Vazio = []  -- ^ Não existe colisão 
hitboxBloco m n Escada = []  -- ^ Não existe colisão 
hitboxBloco m n _ = [((fromIntegral (n - 1), fromIntegral m), (fromIntegral n, fromIntegral (m - 1)))]


-- | Função que testa se uma hitbox passa dos limites do mapa.
testePassalimite :: [[Bloco]] -> Hitbox -> Bool
testePassalimite l ((x1,y1),(x2,y2)) = x1 < 0 || x2 > nColunas || y1 > nLinhas || y2 < 0
                          where nLinhas = fromIntegral (length l)
                                nColunas = fromIntegral (length (head l)) 


-- | Função auxiliar usada para verificar se duas hitboxes colidem.
colisaoHitHit :: Hitbox -> Hitbox -> Bool
colisaoHitHit ((x1,y1),(x1',y1')) ((x2,y2),(x2',y2')) = if x1 <= x2' && x1' >= x2 && y1' <= y2 && y1 >= y2' then True else False                              


-- | Função recursiva da função __colisaoHitHit__ que verifica se um personagem colide com os limites do mapa ou outra estrutura(plataformas,alcapões).
colisaoHitboxparede :: Hitbox -> [Hitbox] -> Bool
colisaoHitboxparede hit [] = False
colisaoHitboxparede hit (h:t) = if colisaoHitHit hit h then True
                                else colisaoHitboxparede hit t


{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : Luís António Peixoto Soares <a106932@alunos.uminho.pt>
              Kevin Martins Nogueira <a106905@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where

import Data.Maybe

import LI12324

import Tarefa1

import Tarefa2

import Tarefa3

{-|

Nesta tarefa, temos como objetivo definir a função __atualiza__ que deve atualizar as novas direções e velocidades dos personagens de acordo com as ações dadas.

Para isso, precisamos de definir as seguintes funções auxiliares:

* __atuaInimigo__, que trata de atualizar os inimigos.
* __atuaJogador__, que trata de atualizar o Jogador.

Cada uma destas funções vai usar a função __atuaAcao__, que dado um mapa, um personagem individual e uma ação atualiza esse personagem.

= Funções:
== Função Principal:
prop> atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
prop> atualiza acIni acJog jogo = jogo { inimigos = novoIni, jogador = novoJog }
prop>         where novoIni = atuaInimigo (mapa jogo) acIni (inimigos jogo)
prop>               novoJog = atuaJogador (mapa jogo) acJog (jogador jogo)

== Função auxiliar __atuaInimigo__:
prop> atuaInimigo :: Mapa -> [Maybe Acao] -> [Personagem] -> [Personagem] 
prop> atuaInimigo _ [] ini = ini
prop> atuaInimigo mapa (h:t) (x:xs) = case h of
prop>                         Just a -> atuaAcao mapa a x : atuaInimigo mapa t xs 
prop>                         Nothing -> (x:xs)

== Função auxiliar __atuaJogador__:
prop> atuaJogador :: Mapa -> Maybe Acao -> Personagem -> Personagem 
prop> atuaJogador mapa ac jog = case ac of 
prop>                      Just a -> atuaAcao mapa a jog
prop>                      Nothing -> jog

== Função auxiliar __atuaAcao__:
prop> atuaAcao :: Mapa -> Acao -> Personagem -> Personagem 
prop> atuaAcao (Mapa _ _ blc) ac pers = case ac of
prop>      Subir -> if sobEsc blc pers then pers { velocidade = (0,-5), direcao = Norte, emEscada = True} else pers
prop>      Descer -> if sobEsc blc pers then pers { velocidade = (0,5), direcao = Sul, emEscada = True} else pers
prop>      AndarDireita -> if movDir blc pers then pers { velocidade = (-5,0), direcao = Oeste} else pers { velocidade = (5,0), direcao = Este}
prop>      AndarEsquerda -> if movEsq blc pers then pers { velocidade = (5,0), direcao = Este} else pers { velocidade = (-5,0), direcao = Oeste}
prop>      Saltar -> if (emEscada pers) == False then pers { velocidade = (0,-5)} else pers
prop>      Parar -> if permParar blc pers == True then pers { velocidade = (0,0)} else pers

= Exemplos de utiilização:
>>> atualiza ([Nothing,Hothing]) (Nothing) Jogo (Mapa ((4.5, 2), Oeste) (5, 5) [[Plataforma, Plataforma, Plataforma, Alcapao, Alcapao], [Vazio, Plataforma, Escada, Plataforma, Vazio], [Vazio, Vazio, Vazio, Vazio, Vazio], [Vazio, Escada, Vazio, Vazio, Escada], [Plataforma, Plataforma, Alcapao, Plataforma, Plataforma]]) ([(Personagem (4, 0) Fantasma (4.5, 1.5) Oeste (1, 1) False True 1 0 (False, 0)),(Personagem (1, 5) Fantasma (0.5, 1.5) Este (1, 1) False True 1 0 (False, 0)),(Personagem (1, 0) Fantasma (2.5, 2.5) Oeste (1, 1) False True 0 0 (False, 0))]) ([(Martelo,(4.5,4.5)),(Moeda,(1.5,1.5))]) (Personagem (0.5, 0.5) Jogador (0.5, 1.5) Norte (1, 1) False True 2 0 (True, 10))
Jogo (Mapa ((4.5, 2), Oeste) (5, 5) [[Plataforma, Plataforma, Plataforma, Alcapao, Alcapao], [Vazio, Plataforma, Escada, Plataforma, Vazio], [Vazio, Vazio, Vazio, Vazio, Vazio], [Vazio, Escada, Vazio, Vazio, Escada], [Plataforma, Plataforma, Alcapao, Plataforma, Plataforma]]) ([(Personagem (4, 0) Fantasma (4.5, 1.5) Oeste (1, 1) False True 1 0 (False, 0)),(Personagem (1, 5) Fantasma (0.5, 1.5) Este (1, 1) False True 1 0 (False, 0)),(Personagem (1, 0) Fantasma (2.5, 2.5) Oeste (1, 1) False True 0 0 (False, 0))]) ([(Martelo,(4.5,4.5)),(Moeda,(1.5,1.5))]) (Personagem (0.5, 0.5) Jogador (0.5, 1.5) Norte (1, 1) False True 2 0 (True, 10))
-}


atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza acIni acJog jogo = jogo { inimigos = novoIni, jogador = novoJog }
        where novoIni = atuaInimigo (mapa jogo) acIni (inimigos jogo)
              novoJog = atuaJogador (mapa jogo) acJog (jogador jogo)


-- | Função que calcula a hitbox de uma lista de blocos dado a lista das coordenadas do canto superior esquerdo.
hitBlocosCEsq :: [Posicao] -> [Hitbox] 
hitBlocosCEsq = map (\(x, y) -> ((x, y + 1), (x + 1, y)))


-- | Função que atualiza uma lista de inimigos de acordo com a lista de ações dadas.
atuaInimigo :: Mapa -> [Maybe Acao] -> [Personagem] -> [Personagem] 
atuaInimigo _ [] ini = ini
atuaInimigo mapa (h:t) (x:xs) = case h of
                        Just a -> atuaAcao mapa a x : atuaInimigo mapa t xs 
                        Nothing -> (x:xs)



-- | Função que atualiza a personagem do jogador de acordo com a ação dada.
atuaJogador :: Mapa -> Maybe Acao -> Personagem -> Personagem 
atuaJogador mapa ac jog = case ac of 
                     Just a -> atuaAcao mapa a jog
                     Nothing -> jog


-- | Função que, dado um mapa, uma ação e um personagem atualiza o respetivo personagem.
atuaAcao :: Mapa -> Acao -> Personagem -> Personagem 
atuaAcao (Mapa _ _ blc) ac pers = case ac of
     Subir -> if sobEsc blc pers then pers { velocidade = (0,-5), direcao = Norte, emEscada = True} else pers
     Descer -> if sobEsc blc pers then pers { velocidade = (0,5), direcao = Sul, emEscada = True} else pers
     AndarDireita -> if movDir blc pers then pers { velocidade = (-5,0), direcao = Oeste} else pers { velocidade = (5,0), direcao = Este}
     AndarEsquerda -> if movEsq blc pers then pers { velocidade = (5,0), direcao = Este} else pers { velocidade = (-5,0), direcao = Oeste}
     Saltar -> if (emEscada pers) == False then pers { velocidade = (0,-5)} else pers
     Parar -> if permParar blc pers == True then pers { velocidade = (0,0)} else pers



-- | Função que verifica se uma personagem se encontra na posição de um bloco do tipo __Escada__.
sobEsc :: [[Bloco]] -> Personagem -> Bool 
sobEsc blc pers = if (emEscada pers) 
                     then True 
                     else if colisaoHitboxparede (criaHitbox pers) (hitBlocosCEsq (cSupEsqEAux blc)) 
                          then True 
                          else False


-- | Função que verifica se uma personagem não se pode mover para a direita.
movDir :: [[Bloco]] -> Personagem -> Bool 
movDir blc pers = if ((ressalta pers) == True) && (any (`elem` cSupEsqP blc) [(snd (criaHitbox pers))] == True)
                      then True
                      else if ((ressalta pers) == True) && (any (`elem` cSupEsqV blc) [((fst (snd (criaHitbox pers))), snd (fst (criaHitbox pers)))] == True)
                           then True
                           else False  


-- | Função que testa se uma personagem não se pode mover para a esquerda.
movEsq :: [[Bloco]] -> Personagem -> Bool 
movEsq blc pers = if ((ressalta pers) == True) && (any (`elem` (map (\(x,y)-> (x+1,y)) (cSupEsqP blc))) [((fst (fst (criaHitbox pers))), (snd (snd (criaHitbox pers))))] == True) 
                       then True
                       else if ((ressalta pers) == True) && (any (`elem` (map (\(x,y)-> (x+1,y)) (cSupEsqV blc))) [(fst (criaHitbox pers))] == True)
                            then True
                            else False

-- | Função que testa se um personagem pode estar parado.
permParar :: [[Bloco]] -> Personagem -> Bool 
permParar blc pers = if (any (`elem` cSupEsqP blc) [(fst (criaHitbox pers))] == True)
                     then True
                     else False






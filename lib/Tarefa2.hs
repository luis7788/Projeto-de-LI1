{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Luís António Peixoto Soares <a106932@alunos.uminho.pt>
              Kevin Martins Nogueira <a106905@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324

import Tarefa1

{- |

Nesta tarefa, temos como objetivo definir a função __valida__, que tem como objetivo verificar se um dado jogo viola certas restrições previamente dadas:
   
* O mapa tem “chão”, i.e. uma plataforma que impede que o jogador ou outro personagem caia fora do mapa.
* Todos os inimigos tẽm a propriedade __ressalta__ a __True__, enquanto que o jogador a tem a __False__.
* A posição inicial de um jogador não pode colidir com a posição inicial de um outro personagem. Note que as posições iniciais de inimigos podem colidir entre estes.
* Número mínimo de inimigos igual a 2 (dois).
* inimigos __Fantasma__ têm exatamente 1 (uma) vida.
* Escadas não podem começar/terminar em alçaapões, e pelo menos uma das suas extremidades tem que ser do tipo __Plataforma__.
* Alçapões não podem ser menos largos que o jogador.
* Não podem existir personagens nem colecionãveis “dentro” de plataformas ou alçapões, isto é. o bloco (na matriz do mapa) correspendente à posição de um personagem ou objeto tem que ser __Vazio__.

Para a definir, precisamos usar as seguintes funções auxiliares:

* __minIni__: testa se o número mínimo de inimigos no mapa é 2.
* __colidePI__: testa se a posição inicial do jogador coincide com a posição inicial de outro personagem.
* __ress__: testa se todos os inimigos têm a propriedade ressalta a __True__ e o jogador a __False__.
* __existChao__: testa se um mapa possui "chão", ou seja, uma plataforma que impede os personagens de cair fora do mapa.
* __tamanhoAlcp__: testa se um jogador é menor que um bloco do tipo __Alcapao__.
* __fantLife__: testa se todos os inimigo do tipo __Fantasma__ tem uma vida.
* __colideVazio__: testa se o bloco em que os personagens ou colecionáveis estão é do tipo __Vazio__ ou __Escada__.
* __testEscd__: testa se todas as escadas do mapa não começam nem terminam em alçapões, e se pelo menos uma das suas extremidades é do tipo __Plataforma__.

= Funções:
== Função Principal:
prop> valida :: Jogo -> Bool 
prop> valida (Jogo mapa ini colc jog) = let cond1 = minIni ini 
prop>                                       cond2 = not (colidePI mapa ini)
prop>                                       cond3 = ress ini jog
prop>                                       cond4 = existChao mapa
prop>                                       cond5 = tamanhoAlcp jog
prop>                                       cond6 = allTrue (fantLife ini)
prop>                                       cond7 = colideVazio ini jog colc mapa
prop>                                       cond8 = testEscd mapa (blcCimaBaixo mapa (cSupEsqE mapa)) 
prop>                                   in all (==True) [cond1,cond2,cond3,cond4,cond5,cond6,cond7,cond8]

== Função auxiliar __minIni__:
prop> minIni :: [Personagem] -> Bool
prop> minIni ini = length ini >= 2

== Função auxiliar __colidePI__ e __colAux__:
prop> colidePI :: Mapa -> [Personagem] -> Bool
prop> colidePI (Mapa (posI,_) _ _) ini = any (== posI) (map colAux ini)
prop> colAux :: Personagem -> Posicao
prop> colAux indiv = (posicao indiv)

== Função auxiliar __ress__ e __ressAux__:
prop> ress :: [Personagem] -> Personagem -> Bool 
prop> ress ini jog = all (== True) (map ressAux ini) && (ressalta jog) == False 
prop> ressAux :: Personagem -> Bool
prop> ressAux indiv = (ressalta indiv)

== Função auxiliar __existChao__:
prop> existChao :: Mapa -> Bool 
prop> existChao (Mapa _ _ blc) = elem Plataforma (last blc)

== Função auxiliar __tamanhoAlcp__:
prop> tamanhoAlcp :: Personagem -> Bool
prop> tamanhoAlcp jog = l <= 1 && c <= 1
prop>           where (c,l) = tamanho jog

== Função auxiliar __fantLife__:
prop> fantLife :: [Personagem] -> [Bool] 
prop> fantLife [] = []
prop> fantLife (indiv:xs) = if (tipo indiv) == MacacoMalvado || (tipo indiv) == Jogador
prop>                         then True : fantLife xs
prop>                         else if (tipo indiv) == Fantasma && (vida indiv) == 1
prop>                              then True : fantLife xs
prop>                              else False : fantLife xs

== Função auxiliar __colideVazio__:
prop> colideVazio :: [Personagem] -> Personagem -> [(Colecionavel, Posicao)] -> Mapa -> Bool 
prop> colideVazio ini jog colc (Mapa _ _ blc) = all (`elem` (map posCentral ((cSupEsqV blc) ++ (cSupEsqEAux blc)))) (posElementos ini jog colc)

== Função auxiliar __posElementos__:
prop> posElementos :: [Personagem] -> Personagem -> [(Colecionavel, Posicao)] -> [Posicao] 
prop> posElementos ini jog colc =  map (\indiv-> (posicao indiv)) ini ++ [(posicao jog)] ++ map snd colc

== Função auxiliar __testEscd__:
prop> testEscd :: Mapa -> [(Bloco, Bloco)] -> Bool
prop> testEscd (Mapa _ _ blc) list = if null (cSupEsqEAux blc)
prop>                                 then True 
prop>                                 else if not (null (cSupEsqEAux blc)) && length list >= 1 
prop>                                      then all (==True) (auxTestE list) 
prop>                                      else False

== Função auxiliar __auxTesteE__:
prop> auxTestE :: [(Bloco,Bloco)] -> [Bool] 
prop> auxTestE [] = [True]
prop> auxTestE ((x,y):r) = if x == Plataforma && (y == Plataforma || y == Vazio || y == Escada)
prop>                          then True : auxTestE r
prop>                          else if (x == Plataforma || x == Vazio || x == Escada) && y == Plataforma
prop>                               then True : auxTestE r
prop>                               else False : auxTestE r

== Função auxiliar __blcCimaBaixo__:
prop> blcCimaBaixo :: Mapa -> [Posicao] -> [(Bloco, Bloco)]
prop> blcCimaBaixo _ [] = []
prop> blcCimaBaixo (Mapa a b blc) ((x1, x2):xs) = (blcCima, blcBaixo) : blcCimaBaixo (Mapa a b blc) xs
prop>                         where blcCima = if x1 > 0 then blc !! floor (x1 - 1) !! floor x2 else Vazio
prop>                               blcBaixo = if x1 < fromIntegral (length blc) - 1 then blc !! floor (x1 + 1) !! floor x2 else Vazio


-}


valida :: Jogo -> Bool 
valida (Jogo mapa ini colc jog) = let cond1 = minIni ini 
                                      cond2 = not (colidePI mapa ini)
                                      cond3 = ress ini jog
                                      cond4 = existChao mapa
                                      cond5 = tamanhoAlcp jog
                                      cond6 = allTrue (fantLife ini)
                                      cond7 = colideVazio ini jog colc mapa
                                      cond8 = testEscd mapa (blcCimaBaixo mapa (cSupEsqE mapa)) 
                                  in all (==True) [cond1,cond2,cond3,cond4,cond5,cond6,cond7,cond8]

-- | Função testa que verifica se todos os valores lógicos de uma lista são do tipo __True__.
allTrue :: [Bool] -> Bool 
allTrue val = all (True==) val


-- | Função que dado um mapa dá uma lista das coordenadas do canto superior esquerdo de cada bloco do tipo __Escada__.
cSupEsqE :: Mapa -> [Posicao]
cSupEsqE (Mapa _ _ blc) = cSupEsqEAux blc

-- | Função auxiliar de __cSupEsqE__.
cSupEsqEAux :: [[Bloco]] -> [Posicao]
cSupEsqEAux [] = [] 
cSupEsqEAux blc = concatMap (\(nL, linha) -> [(fromIntegral nB, fromIntegral nL) | (nB, bloco) <- zip [0..] linha, bloco == Escada]) (zip [0..] blc)


-- | Função que dado uma matriz de blocos dá uma lista das coordenadas do canto superior esquerdo de cada bloco do tipo __Vazio__.
cSupEsqV :: [[Bloco]] -> [Posicao]
cSupEsqV [] = [] 
cSupEsqV blc = concatMap (\(nL, linha) -> [(fromIntegral nB, fromIntegral nL) | (nB, bloco) <- zip [0..] linha, bloco == Vazio]) (zip [0..] blc)


-- | Função que dado uma matriz de blocos dá uma lista das coordenadas do canto superior esquerdo de cada bloco do tipo __Plataforma__.
cSupEsqP :: [[Bloco]] -> [Posicao]
cSupEsqP [] = [] 
cSupEsqP blc = concatMap (\(nL, linha) -> [(fromIntegral nB, fromIntegral nL) | (nB, bloco) <- zip [0..] linha, bloco == Plataforma]) (zip [0..] blc)


-- | Função que retorna as coodenadas da posição central de um bloco apartir das coordenadas do canto superior esquerdo desse mesmo bloco.  
posCentral :: Posicao -> Posicao 
posCentral (x,y) = (x + 0.5, y + 0.5)


-- | Função que testa se o número mínimo de inimigos no mapa é 2.
minIni :: [Personagem] -> Bool
minIni ini = length ini >= 2


-- | Função que testa se a posição inicial do jogador coincide com a posição inicial de outro personagem.
colidePI :: Mapa -> [Personagem] -> Bool
colidePI (Mapa (posI,_) _ _) ini = any (== posI) (map colAux ini)

-- | Função auxiliar de __colidePi__.
colAux :: Personagem -> Posicao
colAux indiv = (posicao indiv)


-- | Função que testa se todos os inimigos têm a propriedade ressalta a __True__ e o jogador a __False__.
ress :: [Personagem] -> Personagem -> Bool 
ress ini jog = all (== True) (map ressAux ini) && (ressalta jog) == False 

-- | Função auxiliar de __ress__.
ressAux :: Personagem -> Bool
ressAux indiv = (ressalta indiv)


-- | Função que testa se um mapa possui "chão".
existChao :: Mapa -> Bool 
existChao (Mapa _ _ blc) = elem Plataforma (last blc)


-- | Função que testa se um jogador é menor que um bloco do tipo __Alcapao__.
tamanhoAlcp :: Personagem -> Bool
tamanhoAlcp jog = l <= 1 && c <= 1
          where (c,l) = tamanho jog


-- | Função que testa se todos os inimigo do tipo __Fantasma__ tem uma vida.
fantLife :: [Personagem] -> [Bool] 
fantLife [] = []
fantLife (indiv:xs) = if (tipo indiv) == MacacoMalvado || (tipo indiv) == Jogador
                        then True : fantLife xs
                        else if (tipo indiv) == Fantasma && (vida indiv) == 1
                             then True : fantLife xs
                             else False : fantLife xs


-- | Função que testa se o bloco em que os personagens ou colecionáveis estão é do tipo __Vazio__ ou __Escada__.
colideVazio :: [Personagem] -> Personagem -> [(Colecionavel, Posicao)] -> Mapa -> Bool 
colideVazio ini jog colc (Mapa _ _ blc) = all (`elem` (map posCentral ((cSupEsqV blc) ++ (cSupEsqEAux blc)))) (posElementos ini jog colc)


-- | Função que calcula a lista de posições de todos os personagens.
posElementos :: [Personagem] -> Personagem -> [(Colecionavel, Posicao)] -> [Posicao] 
posElementos ini jog colc =  map (\indiv-> (posicao indiv)) ini ++ [(posicao jog)] ++ map snd colc


-- | Função que testa se todas as escadas do mapa não começam nem terminam em alçapões, e se pelo menos uma das suas extremidades é do tipo __Plataforma__.
testEscd :: Mapa -> [(Bloco, Bloco)] -> Bool
testEscd (Mapa _ _ blc) list = if null (cSupEsqEAux blc)
                                then True 
                                else if not (null (cSupEsqEAux blc)) && length list >= 1 
                                     then all (==True) (auxTestE list) 
                                     else False


-- | Função auxiliar que recebe uma lista de pares dos blocos acima e em baixo das escadas e devole a lista dos valores lógicos se estas apresentarem pelo menos um bloco de plataforma e nenhum bloco de alçapão.
auxTestE :: [(Bloco,Bloco)] -> [Bool] 
auxTestE [] = [True]
auxTestE ((x,y):r) = if x == Plataforma && (y == Plataforma || y == Vazio || y == Escada)
                         then True : auxTestE r
                         else if (x == Plataforma || x == Vazio || x == Escada) && y == Plataforma
                              then True : auxTestE r
                              else False : auxTestE r


-- | Função que dada uma matriz de blocos e uma lista das coordenadas do canto superior esquerdo de cada bloco do tipo __Escada__ dessa matriz(calculada pela findblocE), devolve uma lista de pares que correspondem ao bloco acima e em baixo das escada, respetivamente.
blcCimaBaixo :: Mapa -> [Posicao] -> [(Bloco, Bloco)]
blcCimaBaixo _ [] = []
blcCimaBaixo (Mapa a b blc) ((x1, x2):xs) = (blcCima, blcBaixo) : blcCimaBaixo (Mapa a b blc) xs
                        where blcCima = if x1 > 0 then blc !! floor (x1 - 1) !! floor x2 else Vazio
                              blcBaixo = if x1 < fromIntegral (length blc) - 1 then blc !! floor (x1 + 1) !! floor x2 else Vazio
                                               

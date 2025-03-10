{-|
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : Luís António Peixoto Soares <a106932@alunos.uminho.pt>
              Kevin Martins Nogueira <a106905@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where

import LI12324

import Tarefa1

import Tarefa2

{- |

Nesta tarefa, temos como objetivo definir a função __movimenta__, cujo mesma deve animar todos os personagens, i.e calcular as
suas novas posiçõoes e respectivas consequências.

Para a definir, precisamos usar as seguintes funções auxiliares:

* __efeitoGravidade__, que dado um jogo, aplica o vetor gravidade a todos os personagens do jogo;

* __animaPers__, que dado um jogo, retorna um jogo, onde as posições de todos os personagens foram atualizadas;
              
Para esta função usamos a função auxiliar: __animaIndividual__, responsável por atualizar a posição de um personagem individual.

* __tiposColisoes__, que dado um jogo, retorna um jogo, calculando as respetivas consequências de diferentes tipos de colisões.

Para definirmos esta função auxiliar, utilizamos outras funções, uma para cada tipo de colisão:
                   __colisoesPersPers__, que trata das colisões entre dois personagens, atualizando os seus respetivos pontos de vida;
                   __colisoesPersMapa__, que que atualiza as posições dos personagens e o mapa dependendo de certas colisões e
                   __colisoesJogCol__, que atualiza a lista de colecionáveis de um __Jogo__, assim como os pontos do jogador.
                
Optamos por definir outras funções para facilitar o nosso trabalho no decorrer da tarefa:

1. __criaHitboxDano__, que retorna a hitbox de dano de um jogador quando está com o martelo ativado;
2. __hitboxColecionavel__, que retona a hitbox de um colecionável dada a respetiva posição;
3. __hitboxTipoBloco__, análoga à função __hitboxColecionavel__;
4. __desapareceAlcapao__, que retorna __True__ se a hitbox de um jogador sobrepor a de um alcapão, ou seja se um jogador pisar um alcapão.

= Funções:
== Função Principal:
prop> movimenta :: Semente -> Tempo -> Jogo -> Jogo
prop> movimenta semente t jogo = jogoComColisoes
prop>        where random = geraAleatorios semente 10
prop>              jogoGravidade = efeitoGravidade jogo
prop>              jogoAnimado = animaPers random t jogoGravidade
prop>              jogoComColisoes = tiposColisoes jogoAnimado

== Função auxiliar __efeitoGravidade__:
prop> efeitoGravidade :: Jogo -> Jogo
prop> efeitoGravidade jogo = jogo { inimigos = inimigosQueda, jogador = jogadorQueda }
prop>         where inimigosQueda = map (\inimigo -> inimigo { velocidade = sumVetor (velocidade inimigo) gravidade }) (inimigos jogo)
prop>               jogadorQueda = (jogador jogo) { velocidade = sumVetor (velocidade (jogador jogo)) gravidade }

== Função auxiliar __animaPers__:
prop> animaPers :: [Int] -> Tempo -> Jogo -> Jogo
prop> animaPers random t jogo = jogo { inimigos = inimigosAnimados, jogador = jogadorAnimado }
prop>           where inimigosAnimados = map (\inimigo -> animaIndividual random t inimigo) (inimigos jogo)
prop>                 jogadorAnimado = animaIndividual random t (jogador jogo)
prop> animaIndividual :: [Int] -> Tempo -> Personagem -> Personagem
prop> animaIndividual random t personagem = personagemAtualizado
prop>         where (vx, vy) = velocidade personagem
prop>               (x, y) = posicao personagem
prop>               novaPos = (x + vx * t, y + vy * t)
prop>               personagemAtualizado = case (tipo personagem) of
prop>                                       MacacoMalvado -> personagem
prop>                                       Fantasma -> personagem { posicao = novaPos }
prop>                                       Jogador -> personagem { posicao = novaPos }

== Função auxiliar __tipoColisoes__:
prop> tiposColisoes :: Jogo -> Jogo
prop> tiposColisoes jogo = jogoComColisoesJogCol
prop>          where jogoComColisoesPersPers = colisoesPersPers jogo
prop>                jogoComColisoesPersMapa = colisoesPersMapa jogoComColisoesPersPers
prop>                jogoComColisoesJogCol = colisoesJogCol jogoComColisoesPersMapa

== Função auxiliar __colisoesPersPers__:
prop> colisoesPersPers :: Jogo -> Jogo
prop> colisoesPersPers jogo = jogo { jogador = jogadorNovo , inimigos = inimigosAtualizados  }
prop>           where jogadorNovo = foldl (\jog inimigo -> colisaoJog jog inimigo) (jogador jogo) (inimigos jogo)
prop>                 inimigosAtualizados = colisaoInimigos (jogador jogo) (inimigos jogo)

== Função auxiliar __colisoesPersMapa__:
prop> colisoesPersMapa :: Jogo -> Jogo
prop> colisoesPersMapa jogo = jogo { jogador = novoJogador, inimigos = novosInimigos, mapa = novoMapa }
prop>             where novosInimigos = intersetamMapa (mapa jogo) (inimigos jogo)
prop>                   novoJogador = intersetaMapa (mapa jogo) (jogador jogo)
prop>                   novoMapa = pisaBlocosAlcp novoJogador (mapa jogo)

== Função auxiliar __colisoesJogCol__:
prop> colisoesJogCol :: Jogo -> Jogo
prop> colisoesJogCol jogo = jogo { colecionaveis = colecionaveisAtualizados, jogador = jogadorNovo }
prop>       where (colecionaveisAtualizados, colecionaveisObtidos) = foldl (\(colAtualiza, recolhidos) (col, coord) ->
prop>              if colisaoHitHit (criaHitbox (jogador jogo)) (hitboxColecionavel coord)
prop>              then (colAtualiza, col : recolhidos)
prop>              else ((col, coord) : colAtualiza, recolhidos)) ([], []) (colecionaveis jogo)
prop>             jogadorNovo = foldl (\jog col -> apanhaCol jog col) (jogador jogo) colecionaveisObtidos
prop> apanhaCol :: Personagem -> Colecionavel -> Personagem
prop> apanhaCol jog col = case col of
prop>                         Moeda -> jog { pontos = (pontos jog) + 1 }
prop>                         Martelo -> jog { aplicaDano = (True, 10) }


-}

movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta semente t jogo = jogoComColisoes
       where random = geraAleatorios semente 10
             jogoGravidade = efeitoGravidade jogo
             jogoAnimado = animaPers random t jogoGravidade
             jogoComColisoes = tiposColisoes jogoAnimado


-- | Função que obtêm a hitBox de dano do jogador em função da sua posição e tamanho.
criaHitboxDano :: Personagem -> Hitbox
criaHitboxDano personagem = if (direcao personagem) == Oeste then ((x-(3*l/2), y+c/2), (x-l/2, y-c/2)) 
                            else if (direcao personagem) == Este then ((x+l/2, y+c/2), (x+(3*l/2), y-c/2))
                                 else  ((x-l/2, y+c/2), (x+l/2, y-c/2))
                                 where (x, y) = posicao personagem
                                       (c, l) = tamanho personagem

-- | Cria uma hitbox para um colecionável.
hitboxColecionavel :: Posicao -> Hitbox
hitboxColecionavel (x,y) = ((x - 0.5,y + 0.5),(x + 0.5,y - 0.5)) 

-- | Cria uma hitbox para um bloco.
hitboxTipoBloco :: Posicao -> Hitbox
hitboxTipoBloco (x,y) = hitboxColecionavel (x,y)

-- | Transforma hitbox em posição.
hitPosicao :: Hitbox -> Posicao
hitPosicao ((x1, y1), (x2, y2)) = ((x1+x2)/2,(y1+y2)/2)

-- | Retorna True se o jogador pisar um alcapao.
desapareceAlcapao :: Hitbox -> Hitbox -> Bool
desapareceAlcapao ((x1,y1),(x1',y1')) ((x2,y2),(x2',y2')) = if y1 == y2' && x1 < x2' && x1' > x2 || y1' == y2 &&  x1 < x2' && x1' > x2 
                                                            then True
                                                            else False

-- | Função que soma dois vetores.
sumVetor :: Velocidade -> Velocidade -> Velocidade
sumVetor (v1, v1') (v2, v2') = (v1 + v2, v1' + v2')


-- | Função que aplica o vetor gravidade ao jogo.
efeitoGravidade :: Jogo -> Jogo
efeitoGravidade jogo = jogo { inimigos = inimigosQueda, jogador = jogadorQueda }
        where inimigosQueda = map (\inimigo -> inimigo { velocidade = sumVetor (velocidade inimigo) gravidade }) (inimigos jogo)
              jogadorQueda = (jogador jogo) { velocidade = sumVetor (velocidade (jogador jogo)) gravidade }

-- | Função que calcula novas posições dos personagens.
animaPers :: [Int] -> Tempo -> Jogo -> Jogo
animaPers random t jogo = jogo { inimigos = inimigosAnimados, jogador = jogadorAnimado }
          where inimigosAnimados = map (\inimigo -> animaIndividual random t inimigo) (inimigos jogo)
                jogadorAnimado = animaIndividual random t (jogador jogo)

-- | Função auxiliar de __animaPers__.
animaIndividual :: [Int] -> Tempo -> Personagem -> Personagem
animaIndividual random t personagem = personagemAtualizado
        where (vx, vy) = velocidade personagem
              (x, y) = posicao personagem
              novaPos = (x + vx * t, y + vy * t)
              personagemAtualizado = case (tipo personagem) of
                                      MacacoMalvado -> personagem
                                      Fantasma -> personagem { posicao = novaPos }
                                      Jogador -> personagem { posicao = novaPos }
                     

-- | Função que trata das colisões.
tiposColisoes :: Jogo -> Jogo
tiposColisoes jogo = jogoComColisoesJogCol
         where jogoComColisoesPersPers = colisoesPersPers jogo
               jogoComColisoesPersMapa = colisoesPersMapa jogoComColisoesPersPers
               jogoComColisoesJogCol = colisoesJogCol jogoComColisoesPersMapa
  
-- | Função para tratar de colisões Inimigo-Jogador.
colisoesPersPers :: Jogo -> Jogo
colisoesPersPers jogo = jogo { jogador = jogadorNovo , inimigos = inimigosAtualizados  }
          where jogadorNovo = foldl (\jog inimigo -> colisaoJog jog inimigo) (jogador jogo) (inimigos jogo)
                inimigosAtualizados = colisaoInimigos (jogador jogo) (inimigos jogo)

-- | Função auxiliar de __colisoesPersPers__.
colisaoInimigos :: Personagem -> [Personagem] -> [Personagem]                
colisaoInimigos jog [] = []
colisaoInimigos jog (h:t) = case (tipo h) of
                         Jogador -> h : colisaoInimigos jog t
                         MacacoMalvado -> h : colisaoInimigos jog t
                         Fantasma -> if colisaoHitHit (criaHitboxDano jog) (criaHitbox h) && fst (aplicaDano jog) 
                                     then (h { vida = max 0 ((vida h) - 1) }) : colisaoInimigos jog t
                                     else h : colisaoInimigos jog t

-- | Função auxiliar de __colisoesPersPers__.                        
colisaoJog :: Personagem -> Personagem -> Personagem
colisaoJog jog inimigo = case (tipo jog) of
      Jogador -> if colisaoHitHit (criaHitboxDano jog) (criaHitbox inimigo) && fst (aplicaDano jog) 
                 then jog
                 else if (colisaoHitHit (criaHitboxDano jog) (criaHitbox inimigo)) == True && (fst (aplicaDano jog)) == False
                      then jog  
                      else if (colisaoHitHit (criaHitbox jog) (criaHitbox inimigo)) 
                           then jog { vida = max 0 ((vida jog) - 1)}
                           else jog

-- | Função para tratar de colisões Personagem-Mapa.
colisoesPersMapa :: Jogo -> Jogo
colisoesPersMapa jogo = jogo { jogador = novoJogador, inimigos = novosInimigos, mapa = novoMapa }
            where novosInimigos = intersetamMapa (mapa jogo) (inimigos jogo)
                  novoJogador = intersetaMapa (mapa jogo) (jogador jogo)
                  novoMapa = pisaBlocosAlcp novoJogador (mapa jogo)

-- | Função auxiliar de __colisoesPersMapa__.
intersetamMapa :: Mapa -> [Personagem] -> [Personagem]
intersetamMapa mapa [] = []
intersetamMapa mapa (h:t) = intersetaMapa mapa h : intersetamMapa mapa t


-- | Função auxiliar de __colisoesPersMapa__.
intersetaMapa :: Mapa -> Personagem -> Personagem
intersetaMapa mapa personagem = novoPersonagem2
            where intersetaLimites = passaLimites (criaHitbox personagem) mapa
                  novoPersonagem = personagem { posicao = intersetaLimites }
                  intersetaBlocos = passaBlocos (criaHitbox novoPersonagem) mapa
                  novoPersonagem2 = personagem { posicao = intersetaBlocos }

-- | Função para resolver colisoes Limites-Personagem.
passaLimites :: Hitbox -> Mapa -> Posicao
passaLimites hit (Mapa _ _ blc) = if testePassalimite blc hit
                                     then (hitPosicao (auxPassaLimites blc hit))
                                     else hitPosicao hit

-- | Função auxiliar de __passaLimites__.
auxPassaLimites :: [[Bloco]] -> Hitbox -> Hitbox
auxPassaLimites blc ((x1, y1), (x2, y2)) = if x1 < 0 then ((0,y1),(x2 + (0-x1),y2))
                                              else if x2 > nColunas then ((x1 - (x2-nColunas),y1),(nColunas,y2))
                                                   else if y1 > nLinhas then ((x1, nLinhas), (x2, y2 - (y1-nLinhas)))
                                                        else if y2 < 0 then ((x1, y1 + (0-y2)), (x2, 0))
                                                             else ((x1, y1), (x2, y2))
                                                             where  nLinhas = fromIntegral (length blc)
                                                                    nColunas = fromIntegral (length (head blc)) 

-- | Função para resolver colisões Personagem-Blocos(Plataforma/Alcapão)
passaBlocos :: Hitbox -> Mapa -> Posicao          
passaBlocos hit (Mapa _ _ blc) = if aux1PassaBlocos hit blc
                                    then (aux2PassaBlocos hit blc)
                                    else hitPosicao hit


-- | Função auxiliar de __passaBlocos__.
aux1PassaBlocos :: Hitbox -> [[Bloco]] -> Bool
aux1PassaBlocos hit blc = any (\posBloco -> colisaoHitHit hit (hitboxTipoBloco posBloco)) (map hitPosicao (hitboxMatriz blc))

-- | Função auxiliar de __passaBlocos__.
aux2PassaBlocos :: Hitbox -> [[Bloco]] -> Posicao
aux2PassaBlocos hit blc = foldl (\posInicial posBloco -> aux3PassaBlocos hit posInicial posBloco) (hitPosicao hit) (map hitPosicao (hitboxMatriz blc))

-- | Função auxiliar de __passaBlocos__.
aux3PassaBlocos :: Hitbox -> Posicao -> Posicao -> Posicao        
aux3PassaBlocos hit posInicial posBloco = (x', y')
           where x' = fst posInicial                                       
                 y' = if snd (fst hit) <= snd (posBloco)
                      then snd (posBloco) - snd (snd hit)
                      else snd posInicial

-- | Função que transforma __Alcapao__ em __Vazio__ caso o jogador o pise
pisaBlocosAlcp :: Personagem -> Mapa -> Mapa             
pisaBlocosAlcp personagem (Mapa a b blc) = if (aux1PassaBlocosAlcp (criaHitbox personagem) blc) && eJog personagem
                                              then (Mapa a b (aux2PassaBlocosAlcp personagem blc))
                                              else (Mapa a b blc)

-- | Função que testa se um personagem é do tipo __Jogador__.
eJog :: Personagem -> Bool
eJog personagem = if (tipo personagem) == Jogador then True else False

-- | Função auxiliar de __pisaBlocosAlcp__.
hitboxMatriz' :: [[Bloco]] -> [Hitbox]
hitboxMatriz' [] = []
hitboxMatriz' blc = concatMap hitboxLinha' (zip [1..] blc)

-- | Função auxiliar de __hitboxMatriz'__.
hitboxLinha' :: (Int, [Bloco]) -> [Hitbox]
hitboxLinha' (m, linha) = concatMap (\(n, bloco) -> hitboxBloco' m n bloco) (zip [1..] linha)

-- | Função auxiliar de __hitboxMatriz'__.          
hitboxBloco' :: Int -> Int -> Bloco -> [Hitbox]
hitboxBloco' m n Vazio = []  -- ^ Não existe colisão 
hitboxBloco' m n Escada = []  -- ^ Não existe colisão
hitboxBloco' m n Plataforma = [] -- ^ Queremos apenas os Alcapoes
hitboxBloco' m n _ = [((fromIntegral (n - 1), fromIntegral m), (fromIntegral n, fromIntegral (m - 1)))]


-- | Função auxiliar de __pisaBlocosAlcp__.
aux1PassaBlocosAlcp :: Hitbox -> [[Bloco]] -> Bool               
aux1PassaBlocosAlcp hit blc = any (\hitBlocoAlcp -> desapareceAlcapao hit hitBlocoAlcp) (hitboxMatriz' blc)

-- | Função auxiliar de __pisaBlocosAlcp__.
aux2PassaBlocosAlcp :: Personagem -> [[Bloco]] -> [[Bloco]]             
aux2PassaBlocosAlcp personagem blc = map (trocaLinha (criaHitbox personagem)) (zip [1..] blc)

-- | Função auxiliar de __pisaBlocosAlcp__.
trocaLinha :: Hitbox -> (Int, [Bloco]) -> [Bloco]
trocaLinha hit (m, linha) = map (trocaBloco m hit) (zip [1..] linha)

-- | Função auxiliar de __pisaBlocosAlcp__.
trocaBloco :: Int -> Hitbox -> (Int, Bloco) -> Bloco
trocaBloco m hit (n, bloco) = if desapareceAlcapao ((fromIntegral (n - 1), fromIntegral m), (fromIntegral n, fromIntegral (m - 1))) hit                                                 
                              then Vazio
                              else bloco


-- | Função para tratar de colisões Jogador-Colecionável.
colisoesJogCol :: Jogo -> Jogo
colisoesJogCol jogo = jogo { colecionaveis = colecionaveisAtualizados, jogador = jogadorNovo }
      where (colecionaveisAtualizados, colecionaveisObtidos) = foldl (\(colAtualiza, recolhidos) (col, coord) ->
             if colisaoHitHit (criaHitbox (jogador jogo)) (hitboxColecionavel coord)
             then (colAtualiza, col : recolhidos)
             else ((col, coord) : colAtualiza, recolhidos)) ([], []) (colecionaveis jogo)
            jogadorNovo = foldl (\jog col -> apanhaCol jog col) (jogador jogo) colecionaveisObtidos

-- | Função auxiliar de __colisoesJogCol__.
apanhaCol :: Personagem -> Colecionavel -> Personagem
apanhaCol jog col = case col of
                        Moeda -> jog { pontos = (pontos jog) + 1 }
                        Martelo -> jog { aplicaDano = (True, 10) }


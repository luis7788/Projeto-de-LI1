
module Tarefa3spec where

import Test.HUnit

import Tarefa1spec

import Tarefa2spec

import LI12324

import Tarefa1

import Tarefa2

import Tarefa3

inimigosTeste1 = [fantasma1]

inimigosG1 = [fantasmaG1]

colc = [(Moeda,(0.6,5.5)),(Martelo,(3.5,5.5)),(Moeda,(3,5.5))]

jogadorG1 = jogadorTeste1 { velocidade = sumVetor (velocidade (jogadorTeste1)) gravidade }
   
fantasmaG1 = fantasma1 { velocidade = sumVetor (velocidade (fantasma1)) gravidade }

jogadorAnimado1 = animaIndividual [2287595555194033867,-7444196218550052205
                                          ,7729549144295431816,-8651862829265477711
                                          ,-6886271218902090862,8850224402729552525
                                          ,-8200683093968375451,-5556339232160448045
                                          ,661784212558659102,8334257799322613858] 3 jogadorTeste1 

inimigosAnimados = map (\inimigo -> animaIndividual [2287595555194033867,-7444196218550052205
                                          ,7729549144295431816,-8651862829265477711
                                          ,-6886271218902090862,8850224402729552525
                                          ,-8200683093968375451,-5556339232160448045
                                          ,661784212558659102,8334257799322613858] 3 inimigo) inimigosTeste1

jogadorColide1 = foldl (\jog inimigo -> colisaoJog jog inimigo) jogadorTeste1 inimigosTeste1

inimigosColidem1 = colisaoInimigos jogadorTeste1 inimigosTeste1

inimigosDiff1 = intersetamMapa mapaTeste1 inimigosTeste1

novosColc = [(Moeda,(3,5.5)),(Martelo,(3.5,5.5))]

jogadorColc1 = jogadorTeste1 { pontos = 1}

jogoTeste1 = Jogo
    { mapa = mapaTeste1       
    , inimigos = inimigosTeste1     
    , colecionaveis = colc
    , jogador = jogadorTeste1       
    }

jogoTeste2 = Jogo
    { mapa = mapaTeste1       
    , inimigos = []     
    , colecionaveis = []
    , jogador = jogadorTeste1       
    }

jogoGravidade1 = Jogo
    { mapa =mapaTeste1       
    , inimigos = inimigosG1     
    , colecionaveis = colc
    , jogador = jogadorG1     
    }

jogoAnimado1 = Jogo
    { mapa = mapaTeste1       
    , inimigos = inimigosAnimados     
    , colecionaveis = colc
    , jogador = jogadorAnimado1      
    } 

jogoAnimado2 = Jogo
    { mapa = mapaTeste1       
    , inimigos = []     
    , colecionaveis = []
    , jogador = jogadorAnimado1      
    } 

jogoColisao1 = Jogo
    { mapa = mapaTeste1       
    , inimigos = inimigosColidem1     
    , colecionaveis = colc
    , jogador = jogadorColide1      
    } 

jogoColisao2 = jogoTeste2 { jogador = jogadorColide1 }

jogoDiffMapa1 = jogoTeste1 { inimigos = inimigosDiff1 }

jogoDiffMapa2 = jogoTeste2  -- Não há colisão entre o 'jogadorTeste1' e o mapa

jogoColcAtualizado1 = jogoTeste1 { colecionaveis = novosColc, jogador = jogadorColc1}

jogoProdutoFinal1 = movimenta 5 3 jogoTeste1

jogoProdutoFinal2 = movimenta 5 3 jogoTeste2





teste_Tarefa3_EfeitoGravidade = test [
       "Teste: efeitoGravidade" ~: jogoGravidade1 ~=? efeitoGravidade jogoTeste1 ]

teste_Tarefa3_AnimaPers = test [
      "Teste1: animaPers" ~: jogoAnimado2 ~=? animaPers [2287595555194033867,-7444196218550052205
                                          ,7729549144295431816,-8651862829265477711
                                          ,-6886271218902090862,8850224402729552525
                                          ,-8200683093968375451,-5556339232160448045
                                          ,661784212558659102,8334257799322613858] 3 jogoTeste2 ,
      "Teste2: animaPers" ~: jogoAnimado1 ~=? animaPers [2287595555194033867,-7444196218550052205
                                          ,7729549144295431816,-8651862829265477711
                                          ,-6886271218902090862,8850224402729552525
                                          ,-8200683093968375451,-5556339232160448045
                                          ,661784212558659102,8334257799322613858] 3 jogoTeste1 ]

teste_Tarefa3_TiposColisoes = test [
        "Teste1: colisoes Personagem-Personagem" ~: jogoColisao1 ~=? colisoesPersPers jogoTeste1,             
        "Teste2: colisoes Personagem-Personagem" ~: jogoColisao2 ~=? colisoesPersPers jogoTeste2, 
        
        "Teste1: colisoes Personagem-Mapa" ~: jogoDiffMapa1 ~=? colisoesPersMapa jogoTeste1,
        "Teste2: colisoes Personagem-Mapa" ~: jogoDiffMapa2 ~=? colisoesPersMapa jogoTeste2,
        
        "Teste: colisoes Jogador-Colecionavel" ~: jogoColcAtualizado1 ~=? colisoesJogCol jogoTeste1]

teste_Tarefa3_Movimenta = test [
        "Teste1: Funcao Movimenta" ~: jogoProdutoFinal1 ~=? movimenta 5 3 jogoTeste1,
        "Teste2: Funcao Movimenta" ~: jogoProdutoFinal2 ~=? movimenta 5 3 jogoTeste2 ]



module Tarefa1spec where

import Test.HUnit

import LI12324

import Tarefa1

mapaTeste1 = Mapa ((0.5, 5.5), Oeste) (0.5, 2.5) [[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
                                                 ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
                                                 ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
                                                 ,[Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma]
                                                 ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
                                                 ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
                                                 ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]]

jogadorTeste1 = Personagem
    { velocidade = (1,0)
    , tipo = Jogador     
    , posicao = (0.5,5.5)   
    , direcao = Oeste   
    , tamanho = (0.4,0.4)                
    , emEscada = False
    , ressalta = False  
    , vida = 5      
    , pontos = 0     
    , aplicaDano = (True, 10) 
    }

fantasma1 = Personagem
    { velocidade = (1,0)
    , tipo = Fantasma     
    , posicao = (0.5,0.5)   
    , direcao = Este   
    , tamanho = (0.4,0.4)   
    , emEscada = False
    , ressalta = True  
    , vida = 1      
    , pontos = 0     
    , aplicaDano = (True, 10000000000) 
    }

fantasma2 = Personagem
    { velocidade = (1,0)
    , tipo = Fantasma     
    , posicao = (0.7,5.5)   
    , direcao = Oeste   
    , tamanho = (0.4,0.4)   
    , emEscada = False
    , ressalta = True  
    , vida = 1      
    , pontos = 0     
    , aplicaDano = (True, 10000000000) 
    }

teste_Tarefa1_colisaoHitHit = test [
       "Teste1: colisao entre Hitboxes-True" ~: True ~=? colisaoHitHit ((3,6),(6,3)) ((5,4),(4,5)),
       "Teste2: colisao entre Hitboxes-False" ~: False ~=? colisaoHitHit ((3,6),(6,3)) ((-1,-1),(-1,-1)) ]

teste_Tarefa1_ColisoesPersonagens = test [
       "Teste1: colisao entre Personagens-False" ~: False ~=? colisoesPersonagens jogadorTeste1 fantasma1,
       "Teste2: colisao entre Personagens-True" ~: True ~=? colisoesPersonagens jogadorTeste1 fantasma2, 
       "Teste3: colisao entre Personagens-False" ~: False ~=? colisoesPersonagens fantasma1 fantasma2 ]

teste_Tarefa1_ColisoesParede = test [
       "Teste1: Colisao Personagem/Mapa-False" ~: False ~=? colisoesParede mapaTeste1 jogadorTeste1,      
       "Teste2: Colisao Personagem/Mapa-False" ~: False ~=? colisoesParede mapaTeste1 fantasma2,
       "Teste3: Colisao Personagem/Mapa-True" ~: True ~=? colisoesParede mapaTeste1 fantasma1 ]
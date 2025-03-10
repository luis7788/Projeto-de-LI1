
module Tarefa4spec where

import Test.HUnit

import Tarefa1spec

import Tarefa2spec

import Tarefa3spec

import LI12324

import Tarefa1

import Tarefa2

import Tarefa3

import Tarefa4

jogoAtualiza1A = Jogo
    { mapa = mapaAtualiza1       
    , inimigos = inimigosAtualiza1A     
    , colecionaveis = colcAtualiza1
    , jogador = jogadorAtualiza1A       
    }

jogoAtualiza1B = Jogo
    { mapa = mapaAtualiza1       
    , inimigos = inimigosAtualiza1B     
    , colecionaveis = colcAtualiza1
    , jogador = jogadorAtualiza1B       
    }

mapaAtualiza1 = Mapa ((1.5, 1.5), Este) (5, 5) [[Escada, Vazio, Vazio], [Vazio, Vazio, Plataforma], [Plataforma, Plataforma, Plataforma]]

colcAtualiza1 = [(Moeda,(1.5,1.5)),(Martelo,(1.5,1.5))]

inimigosAtualiza1A = [iniA, iniB]

inimigosAtualiza1B = [iniC, iniD]

iniA = Personagem
    { velocidade = (0,0)
    , tipo = Fantasma     
    , posicao = (1.5,1.5)   
    , direcao = Este   
    , tamanho = (1,1)   
    , emEscada = False
    , ressalta = True  
    , vida = 1     
    , pontos = 0     
    , aplicaDano = (False, 0) 
    }  

iniB = Personagem
    { velocidade = (0,0)
    , tipo = Fantasma     
    , posicao = (0.5,1.5)   
    , direcao = Oeste   
    , tamanho = (1,1)   
    , emEscada = False
    , ressalta = True  
    , vida = 1      
    , pontos = 0     
    , aplicaDano = (False, 0) 
    }  

iniC = Personagem
    { velocidade = (-5,0)
    , tipo = Fantasma     
    , posicao = (1.5,1.5)   
    , direcao = Oeste   
    , tamanho = (1,1)   
    , emEscada = False
    , ressalta = True  
    , vida = 1      
    , pontos = 0     
    , aplicaDano = (False, 0) 
    }  

iniD = Personagem
    { velocidade = (5,0)
    , tipo = Fantasma     
    , posicao = (0.5,1.5)   
    , direcao = Este   
    , tamanho = (1,1)   
    , emEscada = False
    , ressalta = True  
    , vida = 1      
    , pontos = 0     
    , aplicaDano = (False, 0) 
    }  

jogadorAtualiza1A = Personagem
    { velocidade = (5,0)
    , tipo = Jogador     
    , posicao = (0.5,0.5)   
    , direcao = Oeste   
    , tamanho = (1,1)                
    , emEscada = True
    , ressalta = False  
    , vida = 2      
    , pontos = 0     
    , aplicaDano = (True, 5) 
    }

jogadorAtualiza1B = Personagem
    { velocidade = (0,-5)
    , tipo = Jogador     
    , posicao = (0.5,0.5)   
    , direcao = Norte   
    , tamanho = (1,1)                
    , emEscada = True
    , ressalta = False  
    , vida = 2      
    , pontos = 0     
    , aplicaDano = (True, 5) 
    }

jogoAtualizado2A = Jogo (Mapa ((2.5, 1.5), Este) (5, 5) [[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma], [Vazio, Vazio, Vazio, Vazio, Vazio], [Vazio, Alcapao, Vazio, Vazio, Vazio], [Vazio, Vazio, Vazio, Vazio, Vazio], [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]]) ([(Personagem (1, 0) Fantasma (3.0, 1.5) Oeste (1, 1) False True 1 0 (False, 0)),(Personagem (1, 0) Fantasma (0.5, 1.5) Oeste (1, 1) False True 1 0 (False, 0)),(Personagem (1, 0) Fantasma (2.5, 2.5) Oeste (1, 1) False True 0 0 (False, 0))]) ([(Martelo,(4.5,4.5))]) (Personagem (0, 0) Jogador (0.5, 1.5) Oeste (1, 1) False True 2 0 (True, 5))
jogoAtualizado2B = jogoAtualizado2A 


jogoAtualiza3A = Jogo
    { mapa = mapaAtualiza1       
    , inimigos = []     
    , colecionaveis = colcAtualiza1
    , jogador = jogadorAtualiza3A       
    }

jogoAtualiza3B = Jogo
    { mapa = mapaAtualiza1       
    , inimigos = []     
    , colecionaveis = colcAtualiza1
    , jogador = jogadorAtualiza3B       
    }


jogadorAtualiza3A = jogadorAtualiza1A {emEscada = False}
jogadorAtualiza3B = jogadorAtualiza1B {emEscada = True, direcao = Norte}



jogoAtualiza3C = Jogo
    { mapa = mapaAtualiza1       
    , inimigos = []     
    , colecionaveis = colcAtualiza1
    , jogador = jogadorAtualiza3C       
    }
    
jogadorAtualiza3C = jogadorAtualiza3A {velocidade = (0,5), direcao = Sul, emEscada = True}


jogoAtualiza3D = Jogo
    { mapa = mapaAtualiza1       
    , inimigos = []     
    , colecionaveis = colcAtualiza1
    , jogador = jogadorAtualiza3D       
    }

jogadorAtualiza3D = jogadorAtualiza3A {velocidade = (-5,0)}

jogoAtualiza3E = Jogo
    { mapa = mapaAtualiza1       
    , inimigos = []     
    , colecionaveis = colcAtualiza1
    , jogador = jogadorAtualiza3E       
    }

jogadorAtualiza3E = jogadorAtualiza3A {velocidade = (5,0), direcao = Este}

jogoAtualiza3F = Jogo
    { mapa = mapaAtualiza1       
    , inimigos = []     
    , colecionaveis = colcAtualiza1
    , jogador = jogadorAtualiza3F      
    }


jogadorAtualiza3F = jogadorAtualiza1A {emEscada = False, direcao = Oeste, velocidade = (0,-5)}


teste_Tarefa4_Atualiza = test [
       "Teste1: Funcao atualiza." ~: jogoAtualiza1B ~=? atualiza ([Just AndarDireita, Just AndarDireita]) ((Just Subir)) jogoAtualiza1A,
       
       "Teste2: Quando não ha acoes o jogo não se altera." ~: jogoAtualizado2B ~=? atualiza ([Nothing]) (Nothing) jogoAtualizado2A,
       
       "Teste3:" ~: jogoAtualiza3F ~=? atualiza [] (Just Saltar) jogoAtualiza3A,
       "Teste3: Quanado a acao e Saltar, a velocidade no eixo y e negativa." ~: True ~=? (snd (velocidade jogadorAtualiza3F)) < 0,
       "Teste3: Quando a acao e Slatar, a direcao não se altera." ~: True ~=? (direcao jogadorAtualiza3F) == (direcao jogadorAtualiza3A),

       "Teste4:" ~: jogoAtualiza3B ~=? atualiza ([]) ((Just Subir)) jogoAtualiza3A,
       "Teste4: Quanado a acao e Subir, a velocidade no eixo y e negativa" ~: True ~=? (snd (velocidade jogadorAtualiza3B)) < 0 ,
       "Teste4: Quando a acao e Subir, a direcao e Norte." ~: True ~=? (direcao jogadorAtualiza3B) == Norte,

       "Teste5:" ~: jogoAtualiza3C ~=? atualiza ([]) ((Just Descer)) jogoAtualiza3A,
       "Teste5: Quanado a acao e Descer, a velocidade no eixo y e positiva." ~: True ~=? (snd (velocidade jogadorAtualiza3C)) > 0,
       "Teste5: Quanado a acao e Descer, a direcao e Sul." ~: True ~=? (direcao jogadorAtualiza3C) == Sul,

       "Teste6:" ~: jogoAtualiza3D ~=? atualiza ([]) ((Just AndarEsquerda)) jogoAtualiza3A,
       "Teste6: Quanado a acao e AndarEsquerda, a velocidade no eixo x e negativa." ~: True ~=? (fst (velocidade jogadorAtualiza3D)) < 0,
       "Teste5: Quanado a acao e AndarEsquerda, a direcao e Oeste." ~: True ~=? (direcao jogadorAtualiza3D) == Oeste,

       "Teste7:" ~: jogoAtualiza3E ~=? atualiza ([]) ((Just AndarDireita)) jogoAtualiza3A,
       "Teste7: Quanado a ação é AndarDireita, a velocidade no eixo x é positiva" ~: True ~=? (fst (velocidade jogadorAtualiza3E)) > 0,
       "Teste7: Quanado a acao e AndarDireita, a direcao e Este." ~: True ~=? (direcao jogadorAtualiza3E) == Este]


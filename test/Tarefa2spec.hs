
module Tarefa2spec where

import Test.HUnit

import Tarefa1spec

import LI12324

import Tarefa1

import Tarefa2


jogoValida1 = Jogo
    { mapa = mapaValida1       
    , inimigos = inimigosValida1     
    , colecionaveis = colcValida1
    , jogador = jogadorValida1       
    }

mapaValida1 = Mapa ((1.5,1.5),Este) (1.5,1.5) [[Vazio, Vazio, Vazio], [Vazio, Vazio, Vazio], [Plataforma, Plataforma, Plataforma]] 

mapaValida2 = Mapa ((1.5,1.5),Este) (1.5,1.5) [[Vazio, Alcapao, Vazio], [Vazio, Escada, Vazio], [Vazio, Vazio, Plataforma]] 

mapaValida3 = Mapa ((1.5,1.5),Este) (1.5,1.5) [[Vazio, Alcapao, Vazio], [Vazio, Escada, Vazio], [Vazio, Vazio, Vazio]] 

inimigosValida1 = [ini1, ini2]
    
ini1 = Personagem
    { velocidade = (1,0)
    , tipo = Fantasma     
    , posicao = (2.5,1.5)   
    , direcao = Este   
    , tamanho = (1,1)   
    , emEscada = False
    , ressalta = True  
    , vida = 1      
    , pontos = 0     
    , aplicaDano = (False, 0) 
    }

ini2 = Personagem
    { velocidade = (1,0)
    , tipo = Fantasma     
    , posicao = (2.5,1.5)   
    , direcao = Este   
    , tamanho = (1,1)   
    , emEscada = False
    , ressalta = True  
    , vida = 1      
    , pontos = 0     
    , aplicaDano = (False, 0) 
    }

inimigosValida2 = [ini3]
 
ini3 = Personagem
    { velocidade = (1,0)
    , tipo = Fantasma     
    , posicao = (1.5,1.5)   
    , direcao = Este   
    , tamanho = (1,1)   
    , emEscada = False
    , ressalta = False  
    , vida = 2      
    , pontos = 0     
    , aplicaDano = (False, 0) 
    }   

colcValida1 = [(Moeda, (1.5,1.5))] 

colcValida2 = [(Martelo,(2.5,2.5))]

jogadorValida1 = Personagem
    { velocidade = (1,0)
    , tipo = Jogador     
    , posicao = (0.5,0.5)   
    , direcao = Este   
    , tamanho = (1,1)                
    , emEscada = False
    , ressalta = False  
    , vida = 3      
    , pontos = 0     
    , aplicaDano = (False, 0) 
    }
    
jogadorValida2 = Personagem
    { velocidade = (1,0)
    , tipo = Jogador     
    , posicao = (2.5,2.5)   
    , direcao = Este   
    , tamanho = (2,1)                
    , emEscada = False
    , ressalta = True  
    , vida = 3      
    , pontos = 0     
    , aplicaDano = (False, 0) 
    }

jogoValida2 = Jogo
    { mapa = mapaValida2       
    , inimigos = inimigosValida2     
    , colecionaveis = colcValida2
    , jogador = jogadorValida2       
    }


jogadorNValido = Personagem
    { velocidade = (0.0, 0.0),
      tipo = Jogador,
      posicao = (8.5, 6.5),
      direcao = Oeste,
      tamanho = (1.5, 1.5),
      emEscada = False,
      ressalta = False,
      vida = 5,
      pontos = 0,
      aplicaDano = (False, 0)
    }

jogadorValida3 = Personagem
    { velocidade = (1,0)
    , tipo = Jogador     
    , posicao = (0.5,0.5)   
    , direcao = Este   
    , tamanho = (1,1)                
    , emEscada = False
    , ressalta = True  
    , vida = 3      
    , pontos = 0     
    , aplicaDano = (False, 0) 
    }

jogo1 = Jogo
    { mapa = mapaValida1,
      inimigos = [ini1],
      colecionaveis = [],
      jogador = jogadorValida1
    }

jogo2 = Jogo
    { mapa = mapaValida1       
    , inimigos = [ini1, ini2]     
    , colecionaveis = []
    , jogador = jogadorValida1       
    }

jogo3 = Jogo
    { mapa = mapaValida1,
      inimigos = [ini1, ini3],
      colecionaveis = [],
      jogador = jogadorValida1  }

jogo4 = Jogo
    { mapa = mapaValida3       
    , inimigos = [ini1, ini2]     
    , colecionaveis = []
    , jogador = jogadorValida1       
    }

jogo5 = Jogo
    { mapa = mapaValida1       
    , inimigos = [ini2, ini3]     
    , colecionaveis = []
    , jogador = jogadorValida3       
    }

jogo6 = Jogo
    { mapa = mapaValida3       
    , inimigos = [ini1, ini2, ini3]     
    , colecionaveis = []
    , jogador = jogadorValida1       
    }


teste_Tarefa2_Valida = test [
       "Teste1: Jogo verifica todas as condições-True" ~: True ~=? valida jogoValida1,
       "Teste2: Jogo verifica todas as condições-False" ~: False ~=? valida jogoValida2,
       
       "Teste3: Jogo tem pelo menos 2 inimigos-False" ~: False ~=? valida jogo1,
       "Teste4: Jogo tem pelo menos 2 inimigos-True" ~: True ~=? valida jogo2,
       
       "Teste5: Fantasmas tem apenas 1 vida-False" ~: False ~=? valida jogo3,
       "Teste6: Fantasmas tem apenas 1 vida-True" ~: True ~=? valida jogo2,
       
       "Teste7: O mapa tem 'chao'-False" ~: False ~=? valida jogo4,
       "Teste8: O mapa tem 'chao'-True" ~: True ~=? valida jogo2,
       
       "Teste9: Inimigos tem propriedade 'ressalta' a True e o jogador a 'False'-False " ~: False ~=? valida jogo5,
       "Teste10: Inimigos tem propriedade 'ressalta' a True e o jogador a 'False'-True " ~: True ~=? valida jogoValida1,

       "Teste11: A posição inicial de um jogador nao pode coincidir com a posicao inicial de outro personagem-False " ~: False ~=? valida jogo6,
       "Teste12: A posição inicial de um jogador nao pode coincidir com a posicao inicial de outro personagem-True " ~: True ~=? valida jogoValida1]



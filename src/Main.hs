module Main where

import LI12324

import Tarefa1

import Tarefa2

import Tarefa3

import Tarefa4

import Graphics.Gloss

import Graphics.Gloss.Interface.Pure.Game

import System.Random


{- |

Nesta parte do trabalho temos como objetivo usar as funcionalidades e funções já elaboradas em outras Tarefas, juntamente da biblioteca Gloss para construir uma aplicação gráfica que permita o jogador jogar.

Para isso, usamos a função __play__ da biblioteca __Graphics.Gloss.Interface.Pure.Game__ e definimos:

* Um novo estado de jogo, chamado __GameState__, que vai representar todo o estado do jogo, sendo do tipo __Menu__ __TipoMenu__ __Jogo__ ou __InGame__ __Jogo__;
* Uma janela, do tipo __Display__, que nos vai permitir visualizar o jogo;
* A cor de fundo que a nossa janela vai tomar;
* O frame rate, ou seja, o número de frames por segundo que a janela vai mostrar;
* Um estado inicial, que vai ser do tipo __Menu__ __Inicial__ __jogoInicial1__;
* Uma função responsável por determinar como o estado do jogo será visualizado com gráficos 2D, ou seja, como se converte para um valor do tipo __Picture__. 

A essa função demos o nome __animar__ que converte __GameState__ em __Picture__.

Para a definir usamos duas funções auxiliares:

1.__animarMenu__, que é responsável por animar o diferentes tipos de menu que existem, sendo eles __Inicial__, __Pausa__, __Fim__ e __Comandos__;

2.__animarJogo__, que é responsável por animar o __mapa__, os __inimigos__, os __colecionaveis__ e o __jogador__ , que pertencem ao __Jogo__.

* Uma função responsável por atualizar o __GameState__ dependendo das ações do jogador, ou seja, dos possíveis __eventos__;

Demos-lhe o nome __reageEvento__ que atualiza o __GameState__.

A função é responsável pelas ações do jogador tanto no estado __InGame__ __Jogo__, quanto no estado __Menu__ __TipoMenu__ __Jogo__.

Além disso, usamos a função __atualiza__ da __Tarefa4__ que dadas certas ações do __jogador__ no estado __InGame__ __Jogo__ gera ações para o __jogador__ e __inimigos__.

* Uma função responsável por atualizar o __GameState__ em consequência da passagem do tempo.

Demos-lhe o nome __reageTempo__, na qual implementamos a função __movimenta__ da __Tarefa3__ que atualiza as posições dos personagens e trata de possíveis consequências.

= Argumentos da função play:
== Função Main:
prop> main :: IO ()
prop> main = do play dm white fr estadoInit animar reageEvento reageTempo

== Janela
prop> dm :: Display
prop> dm = InWindow "Donkey Kong" (700, 700) (0, 0)

== Frame Rate:
prop> fr :: Int
prop> fr = 60

== Estado Inicial:
prop> estadoInit :: GameState
prop> estadoInit = Menu Inicial jogoInicial1

== Função animar:
prop> animar :: GameState -> Picture
prop> animar (Menu tipoMenu jogo) = animarMenu tipoMenu jogo
prop> animar (InGame jogo) = animarJogo jogo

== Função reageEvento:
prop> reageEvento :: Event -> GameState -> GameState
prop> reageEvento evento (Menu h jogo) = eventosMenu evento (Menu h jogo)
prop> reageEvento evento (InGame jogo) = eventosEmJogoJogador evento (InGame novoJogo2)
prop>                where (InGame novoJogo2) = InGame (eventosEmJogoInimigos (jogo))

== Função reageTempo:
prop> reageTempo :: Float -> GameState -> GameState
prop> reageTempo t (Menu tipo jogo) = Menu tipo jogo
prop> reageTempo t (InGame jogo) = InGame (movimenta 20 (realToFrac t) jogo)

-}

-- Função principal.
main :: IO ()
main = do play dm white fr estadoInit animar reageEvento reageTempo
        

-- | Configurações da janela de visualização.
dm :: Display
dm = InWindow "Donkey Kong" (700, 700) (0, 0)

-- | Números de frames por segundo.
fr :: Int
fr = 60

-- | Estado inicial do jogo.
estadoInit :: GameState
estadoInit = Menu Inicial jogoInicial1

-- | Tipos de Menu 
data TipoMenu = Inicial | Pausa | Fim | Comandos deriving (Show, Eq, Read)

-- | Tipos de estado do jogo.
data GameState = Menu TipoMenu Jogo | InGame Jogo deriving (Show, Eq, Read)

-- | Jogo Inicial.
jogoInicial1 :: Jogo
jogoInicial1 = Jogo {
      mapa = mapaInicial1       
    , inimigos = inimigosInicial1     
    , colecionaveis = colcInicial1
    , jogador = jogadorInicial1   
          }

-- | Mapa Inicial.
mapaInicial1 = Mapa ((0.5, 5.5), Oeste) (0.5, 2.5) [[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
                                                 ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
                                                 ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
                                                 ,[Plataforma, Plataforma, Alcapao, Plataforma, Plataforma, Vazio, Plataforma, Plataforma, Escada, Plataforma]
                                                 ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
                                                 ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
                                                 ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]]
-- | Inimigos do Jogo Inicial.
inimigosInicial1 = [fantasma1, fantasma2]

-- | Colecionáveis do Jogo Inicial.
colcInicial1 = [(Moeda,(3.5,5.5))]

-- | Jogador Inicial.
jogadorInicial1 = Personagem
    { velocidade = (0,0)
    , tipo = Jogador     
    , posicao = (0.5,5.5)   
    , direcao = Oeste   
    , tamanho = (40,30)                
    , emEscada = False
    , ressalta = False  
    , vida = 5      
    , pontos = 0     
    , aplicaDano = (False, 0) 
    }

fantasma1 = Personagem
    { velocidade = (0,0)
    , tipo = Fantasma     
    , posicao = (1.5,2.5)   
    , direcao = Este   
    , tamanho = (40,30)   
    , emEscada = False
    , ressalta = True  
    , vida = 1      
    , pontos = 0     
    , aplicaDano = (True, 1000000000) 
    }

fantasma2 = Personagem
    { velocidade = (0,0)
    , tipo = Fantasma     
    , posicao = (3.5,2.5)   
    , direcao = Oeste   
    , tamanho = (40,30)   
    , emEscada = False
    , ressalta = True  
    , vida = 1      
    , pontos = 0     
    , aplicaDano = (True, 1000000000) 
    }

-- | Defenição do bloco __Plataforma__ em __Picture__.
plataforma :: Picture
plataforma = color orange (polygon [(0,0),(40,0),(40,40),(0,40)])

-- | Defenição do bloco __Alcapao__ em __Picture__.
alcapao :: Picture
alcapao = color azure (polygon [(0,0),(40,0),(40,40),(0,40)])

-- | Defenição do bloco __Escada__ em __Picture__.
escada :: Picture
escada = color rose (polygon [(0,0),(40,0),(40,40),(0,40)])

-- | Defenição do bloco __Vazio__ em __Picture__.
vazio :: Picture
vazio = blank

-- | Função auxiliar que passa de __Bloco__ para __Picture__.
blocoToPic :: Bloco -> Picture
blocoToPic Plataforma = plataforma
blocoToPic Alcapao = alcapao
blocoToPic Escada = escada
blocoToPic Vazio = vazio

-- | Defenição do colecionável __Moeda__ em __Picture__.
moeda :: Picture
moeda = color blue (ThickCircle 0 20)

-- | Defenição do colecionável __Martelo__ em __Picture__.
martelo :: Picture
martelo = color red (ThickCircle 0 20)

-- | Função auxiliar que passa de __Colecionavel__ para __Picture__.
colcToPic :: Colecionavel -> Picture
colcToPic Moeda = moeda
colcToPic Martelo = martelo


-- | Função para animar o estado do jogo.
animar :: GameState -> Picture
animar (Menu tipoMenu jogo) = animarMenu tipoMenu jogo
animar (InGame jogo) = animarJogo jogo

-- | Função para animar o menu.
animarMenu :: TipoMenu -> Jogo -> Picture
animarMenu Inicial jogo = animarMenuInicial Inicial         
animarMenu Fim jogo = animarMenuFim Fim jogo     
animarMenu Pausa jogo = animarMenuPausa Pausa
animarMenu Comandos jogo = animarMenuComandos Comandos        

-- | Função auxiliar para animar o menu inicial.
animarMenuInicial :: TipoMenu -> Picture
animarMenuInicial tipo = pictures [titulo, opcaoIniciar, opcaoComandos]
            where titulo = translate (-90) (50) $ scale 0.25 0.25 $ text "Inicio"
                  opcaoIniciar = translate (-140) 0 $ scale 0.25 0.25 $ text "1. Iniciar Jogo"
                  opcaoComandos = translate (-140) (-50) $ scale 0.25 0.25 $ text "2. Comandos"

-- | Função auxiliar para animar o menu pausa.
animarMenuPausa :: TipoMenu -> Picture
animarMenuPausa tipo = pictures [titulo, opcaoContinuar, opcaoFinalizar]
            where titulo = translate (-100) (50) $ scale 0.25 0.25 $ text "Pausa"
                  opcaoContinuar = translate (-150) 0 $ scale 0.25 0.25 $ text "1. Continuar Jogo"
                  opcaoFinalizar = translate (-150) (-50) $ scale 0.25 0.25 $ text "2. Finalizar Jogo"

-- | Função auxiliar para animar o menu final.
animarMenuFim :: TipoMenu -> Jogo -> Picture
animarMenuFim tipo jogo = pictures [titulo, nPontos,opcaoVoltar]
            where titulo = translate (-60) (50) $ scale 0.25 0.25 $ text "Fim!"
                  nPontos = translate (-150) 0 $ scale 0.25 0.25 $ text $ "Obteve " ++ (show (pontos jog)) ++ " pontos"
                  opcaoVoltar = translate (-280) (-50) $ scale 0.25 0.25 $ text "Press ENTER para voltar ao início"
                  jog = (jogador jogo)

-- | Função auxiliar para animar o menu contendo os comandos.
animarMenuComandos :: TipoMenu -> Picture
animarMenuComandos tipo = pictures [titulo, direita, esquerda, cima, baixo, saltar, pausa, opcaoRetornar]
            where titulo = translate (-140) (150) $ scale 0.25 0.25 $ text "Comandos"
                  direita = translate (-190) (100) $ scale 0.25 0.25 $ text "1. KeyRight -> Direita"
                  esquerda = translate (-190) (50) $ scale 0.25 0.25 $ text "2. KeyLeft -> Esquerda"
                  cima = translate (-190) (0) $ scale 0.25 0.25 $ text "3. KeyUp -> Cima"
                  baixo = translate (-190) (-50) $ scale 0.25 0.25 $ text "4. KeyDown -> Baixo"
                  saltar = translate (-190) (-100) $ scale 0.25 0.25 $ text "5. KeySpace -> Saltar"
                  pausa = translate (-190) (-150) $ scale 0.25 0.25 $ text "6. 'p' -> Pausa"
                  opcaoRetornar = translate (-190) (-200) $ scale 0.25 0.25 $ text "7. Press ENTER para retornar"
                  



-- | Função para animar o jogo.
animarJogo :: Jogo -> Picture
animarJogo (Jogo (Mapa a b blc) ini colc jog) = translate (-200) (60) $ pictures ( (animarMapa (0,0) blc) ++ (translateInimigos ini) ++ (translateColc colc) ++ [translate (realToFrac i) (realToFrac j) estrela, translate (realToFrac x) (realToFrac y) jogador])
                      where jogador :: Picture
                            jogador = color red $ polygon [(0,0),(0,30),(40,30),(40,0)]
                            (c,l) = tamanho jog
                            (x,y) = posicao jog
                            (i,j) = b
                            estrela :: Picture
                            estrela =color yellow (ThickCircle 0 20)

-- | Função para animar uma linha da matriz que representa o mapa.
desenharLinha :: Int -> [Bloco] -> [Picture]
desenharLinha _ [] = []
desenharLinha x (h:t) = (translate (fromIntegral(x*40)) 0 (blocoToPic h)):(desenharLinha (x+1) t)

-- | Função para animar a matriz que representa o mapa.
animarMapa :: (Int,Int) -> [[Bloco]] -> [Picture]
animarMapa _ [] = []
animarMapa (x,y) (h:t) = (translate 0 (fromIntegral(y*(-40))) (pictures (desenharLinha x h))):(animarMapa (x,y+1) t)

-- | Função para animar os __inimigos__ do __Jogo__.
translateInimigos :: [Personagem] -> [Picture]
translateInimigos [] = []
translateInimigos (h:t) = translate (realToFrac x) (realToFrac y) hp : translateInimigos t
                where hp :: Picture
                      hp = color violet $ polygon [(0,0),(0,30),(40,30),(40,0)]
                      (c,l) = tamanho h
                      (x,y) = posicao h

-- | Função para animar os __colecionaveis__ do __Jogo__.
translateColc :: [(Colecionavel, Posicao)] -> [Picture]
translateColc [] = []
translateColc (h:t) = translate (realToFrac x) (realToFrac y) (colcToPic (fst h)) : translateColc t
                where (x,y) = snd h


-- | Função para lidar com as ações do jogador.
reageEvento :: Event -> GameState -> GameState
reageEvento evento (Menu h jogo) = eventosMenu evento (Menu h jogo)
reageEvento evento (InGame jogo) = eventosEmJogoJogador evento (InGame novoJogo1)
               where (InGame novoJogo1) = InGame (eventosEmJogoInimigos (jogo))


-- | Função que adiciona ações aleatórias aos Inimigos.
eventosEmJogoInimigos :: Jogo -> Jogo
eventosEmJogoInimigos jogo = novoJogo
    where semente = mkStdGen 40
          acoesIni = criaAcoes semente (length (inimigos jogo))
          novoJogo = atualiza acoesIni Nothing jogo

-- | Função auxiliar de __eventosEmJogoInimigos__.
criaAcoes :: StdGen -> Int -> [Maybe Acao]
criaAcoes semente numInimigos = replicate numInimigos (criaAcaoIndiv semente)

e :: Int
e = 0

d :: Int
d = 3

-- | Função auxiliar de __eventosEmJogoInimigos__.
criaAcaoIndiv :: StdGen -> Maybe Acao
criaAcaoIndiv semente = case randomR (e, d) semente of
                       (0, _) -> (Just Subir) 
                       (1, _) -> (Just Descer) 
                       (2, _) -> (Just AndarDireita) 
                       (3, _) -> (Just AndarEsquerda) 
                       _ -> Nothing


-- | Função para lidar com as ações do jogador quando este está em algum __Menu__.
eventosMenu :: Event -> GameState -> GameState
eventosMenu (EventKey (Char '1') Down _ _) (Menu Inicial jogo) = let (Menu Inicial jogo) = estadoInit in InGame jogoInicial1 
eventosMenu (EventKey (Char '2') Down _ _) (Menu Inicial jogo) = let (Menu Inicial jogo) = estadoInit in (Menu Comandos jogoInicial1)
eventosMenu (EventKey (SpecialKey KeyEnter) Down _ _) (Menu Comandos jogo) = estadoInit
eventosMenu (EventKey (SpecialKey KeyEnter) Down _ _) (Menu Fim jogo) = estadoInit
eventosMenu (EventKey (Char '1') Down _ _) (Menu Pausa jogo) = InGame jogo
eventosMenu (EventKey (Char '2') Down _ _) (Menu Pausa jogo) = (Menu Fim jogo)
eventosMenu _ gameState = gameState


-- | Função para lidar com as ações do jogador quando este está em algum __Menu__.
eventosEmJogoJogador :: Event -> GameState -> GameState
eventosEmJogoJogador (EventKey (SpecialKey KeySpace) Down _ _) (InGame jogo) = (InGame (atualiza ([Nothing]) (Just Saltar) jogo))
eventosEmJogoJogador (EventKey (SpecialKey KeySpace) Up _ _) (InGame jogo) = (InGame (atualiza ([Nothing]) (Just Parar) jogo))
eventosEmJogoJogador (EventKey (SpecialKey KeyUp) Down _ _) (InGame jogo) = (InGame (atualiza ([Nothing]) (Just Subir) jogo))
eventosEmJogoJogador (EventKey (SpecialKey KeyUp) Up _ _) (InGame jogo) = (InGame (atualiza ([Nothing]) (Just Parar) jogo))
eventosEmJogoJogador (EventKey (SpecialKey KeyDown) Down _ _) (InGame jogo) = (InGame (atualiza ([Nothing]) (Just Descer) jogo))
eventosEmJogoJogador (EventKey (SpecialKey KeyDown) Up _ _) (InGame jogo) = (InGame (atualiza ([Nothing]) (Just Parar) jogo))
eventosEmJogoJogador (EventKey (SpecialKey KeyLeft) Down _ _) (InGame jogo) = (InGame (atualiza ([Nothing]) (Just AndarEsquerda) jogo))
eventosEmJogoJogador (EventKey (SpecialKey KeyLeft) Up _ _) (InGame jogo) = (InGame (atualiza ([Nothing]) (Just Parar) jogo))
eventosEmJogoJogador (EventKey (SpecialKey KeyRight) Down _ _) (InGame jogo) = (InGame (atualiza ([Nothing]) (Just AndarDireita) jogo))
eventosEmJogoJogador (EventKey (SpecialKey KeyRight) Up _ _) (InGame jogo) = (InGame (atualiza ([Nothing]) (Just Parar) jogo))
eventosEmJogoJogador (EventKey (Char 'p') Down _ _) (InGame jogo) = (Menu Pausa jogo)
eventosEmJogoJogador _ s = s


-- | Função que calcula as novas posições dos personagens e respetivas consequências conforme passa o tempo.
reageTempo :: Float -> GameState -> GameState
reageTempo t (Menu tipo jogo) = Menu tipo jogo
reageTempo t (InGame jogo) = InGame (movimenta 20 (realToFrac t) jogo)

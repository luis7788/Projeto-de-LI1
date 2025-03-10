module Main where

import Test.HUnit

import Tarefa1spec

import Tarefa2spec

import Tarefa3spec

import Tarefa4spec

main :: IO ()
main = runTestTTAndExit $ test [teste_Tarefa1_colisaoHitHit
                              , teste_Tarefa1_ColisoesParede, teste_Tarefa1_ColisoesPersonagens
                              , teste_Tarefa2_Valida
                              , teste_Tarefa3_EfeitoGravidade, teste_Tarefa3_AnimaPers
                              , teste_Tarefa3_TiposColisoes, teste_Tarefa3_Movimenta
                              , teste_Tarefa4_Atualiza]


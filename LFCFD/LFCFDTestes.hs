module LFCFDTestes where 

import LFCFD

import Test.HUnit

v5 = Valor 5

let1 = Let "x" (Valor 5) (Lambda "y" (Soma (Ref "x") (Ref "y")))

aplicacao = Let "x" (Valor 5) (Aplicacao (Lambda "y" (Soma (Ref "x") (Ref "y"))) (Valor 3))

teste1 = TestCase (assertEqual "avaliar 5" (VInt 5) (avaliar v5 []))

teste2 = TestCase (assertEqual "avaliar Let x = 5 in (\\y -> x + y) 3" (VInt 8) (avaliar aplicacao []))

todosOsTestes = TestList [ teste1
                         , teste2
                         ]

executarTestes = runTestTT todosOsTestes

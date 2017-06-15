-- | A linguagem LFCFD suporta tanto 
-- expressoes identificadas (LET) quanto 
-- identificadores e funcoes de alta ordem
-- (com o mecanismo de expressoes lambda).
-- As substituicoes sao postergadas. 

module LFCFD where 

type Id = String

-- o ambiente de substituicoes
-- postergadas (Deferred Substitutions).
-- nesse caso, o ambiente corresponde a uma
-- lista entre Identificadores e Valores, onde
-- um valor eh uma expressao com valor inteiro
-- ou uma expressao lambda.

type DefrdSub = [(Id, ValorE)]
 
-- Nessa versao da linguagem, o interpretador precisa
-- retornar valores de um tipo especial, ValorE, que
-- podem ser ou um valor inteiro (VInt Int) ou um
-- closure, que mantem um ambiente de substituicoes
-- postergadas no escopo de uma expressao lambda. Ou
-- seja, avaliar uma expressao lambda no contexto:
--
-- let x = 5 in (\y -> x + y) deve retornar o
-- closure Closure y (x + y) [(x,5)]


data ValorE = VInt Int                         -- apenas um valor inteiro 
            | Closure Id Expressao DefrdSub    -- um closure, usado no retorno da avaliacao de uma expressao lambda
 deriving(Show, Eq)

data Expressao = Valor Int
               | Soma Expressao Expressao
               | Subtracao Expressao Expressao 
               | Multiplicacao Expressao Expressao
               | Divisao Expressao Expressao 
               | Let Id Expressao Expressao       
               | Ref Id
               | Lambda Id Expressao
               | Aplicacao Expressao Expressao   
 deriving(Show, Eq)

-- | O interpretador da linguagem LFCFD
-- (funcao 'avaliar') precisa ser ajustado, uma vez que
-- o tipo de retorno nao pode ser simplesmente
-- um inteiro ou uma expressao. Com substituicoes postergadas,
-- nessa linguagem o retorno precisa ser um ValorE, conforme 
-- discutido anteriormente. 

avaliar :: Expressao -> DefrdSub -> ValorE
avaliar (Valor n)            _ = VInt n
avaliar (Soma e d)          ds = avaliarExpBin e d (+) ds
avaliar (Subtracao e d)     ds = avaliarExpBin e d (-) ds 
avaliar (Multiplicacao e d) ds = avaliarExpBin e d (*) ds
avaliar (Divisao e d)       ds = avaliarExpBin e d div ds
avaliar (Let v e c)         ds = avaliar c ((v,avaliar e ds):ds)
avaliar (Ref v)             ds = pesquisar v ds
avaliar (Lambda a c)        ds = Closure a c ds
avaliar (Aplicacao e1 e2)   ds =
  let v = avaliar e1 ds
  in case v of
     (Closure a c ds') -> avaliar c ((a, avaliar e2 ds):ds')
     otherwise -> error "Tentando aplicar uma expressao que nao eh uma funcao anonima"
    
     

-- | Realiza uma pesquisa por uma determinada
-- variaval no ambiente de substituicoes postergadas. 
pesquisar :: Id -> DefrdSub -> ValorE
pesquisar v [] = error "Variavel nao declarada."
pesquisar v ((i,e):xs)
 | v == i = e
 | otherwise = pesquisar v xs
  
-- | Avalia uma expressao binaria.
avaliarExpBin :: Expressao -> Expressao -> (Int -> Int -> Int) -> DefrdSub -> ValorE
avaliarExpBin e d op ds = VInt (op ve vd)
 where
  (VInt ve) = avaliar e ds 
  (VInt vd) = avaliar d ds

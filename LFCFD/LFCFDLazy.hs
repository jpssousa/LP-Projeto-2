-- | A linguagem LFCFD suporta tanto 
-- expressoes identificadas (LET) quanto 
-- identificadores e funcoes de alta ordem
-- (com o mecanismo de expressoes lambda).
-- As substituicoes sao postergadas. 

module LFCFDLazy where 

type Id = String

-- o ambiente de substituicoes
-- postergadas (Deferred Substitutions).
-- nesse caso, o ambiente corresponde a uma
-- lista entre Identificadores e Valores, onde
-- um valor eh uma expressao com valor inteiro
-- ou uma expressao lambda.

type Env = [(Id, ValorE)]
 
-- Nessa versao da linguagem, o interpretador precisa
-- retornar valores de um tipo especial, ValorE, que
-- podem ser ou um valor inteiro (VInt Int) ou um
-- closure, que mantem um ambiente de substituicoes
-- postergadas no escopo de uma expressao lambda. Ou
-- seja, avaliar uma expressao lambda no contexto:
--
-- let x = 5 in (\y -> x + y) deve retornar o
-- closure Closure y (x + y) [(x,5)]


data ValorE = VInt Int                
            | FClosure Id Expressao Env
            | EClosure Expressao Env
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

avaliar :: Expressao -> Env -> ValorE
avaliar (Valor n)            _ = VInt n
avaliar (Soma e d)          env = avaliarExpBin e d (+) env
avaliar (Subtracao e d)     env = avaliarExpBin e d (-) env 
avaliar (Multiplicacao e d) env = avaliarExpBin e d (*) env
avaliar (Divisao e d)       env = avaliarExpBin e d div env
avaliar (Let v e c)         env = avaliar (Aplicacao (Lambda v c) e) env
avaliar (Ref v)             env = pesquisar v env
avaliar (Lambda a c)        env = FClosure a c env
avaliar (Aplicacao e1 e2)   env =
  let
    v = avaliacaoStrict (avaliar e1 env)
    e = EClosure e2 env
  in case v of
     (FClosure a c env') -> avaliar c ((a, e):env')
     otherwise -> error "Tentando aplicar uma expressao que nao eh uma funcao anonima"
    

avaliacaoStrict :: ValorE -> ValorE     
avaliacaoStrict (EClosure e env) = avaliacaoStrict (avaliar e env)
avaliacaoStrict e = e


-- | Realiza uma pesquisa por uma determinada
-- variaval no ambiente de substituicoes postergadas. 
pesquisar :: Id -> Env -> ValorE
pesquisar v [] = error "Variavel nao declarada."
pesquisar v ((i,e):xs)
 | v == i = e
 | otherwise = pesquisar v xs
  
-- | Avalia uma expressao binaria.
avaliarExpBin :: Expressao -> Expressao -> (Int -> Int -> Int) -> Env -> ValorE
avaliarExpBin e d op env = VInt (op ve vd)
 where
  (VInt ve) = avaliar e env 
  (VInt vd) = avaliar d env

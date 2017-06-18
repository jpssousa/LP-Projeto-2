
module LFCFDLazy where

type Id = String

type Env = [(Id, ValorE)]


data Expressao = Value Int
            | Soma Expressao Expressao
            | Subtracao Expressao Expressao
            | Multiplicacao Expressao Expressao
            | Divisao  Expressao Expressao
            | Let    Id Expressao Expressao
            | Lambda Id Expressao
            | Aplicacao  Expressao Expressao
            | If0  Expressao Expressao Expressao
            | Ref Id
    deriving (Show, Eq)


data ValorE = VInt Int
            | FClosure Id Expressao Env
            | EClosure Expressao Env
    deriving (Show, Eq)


avaliar :: Expressao -> Env -> ValorE
avaliar (Value n)                 _   = VInt n
avaliar (Soma lhs rhs)            env = avaliarBinExp lhs rhs (+) env
avaliar (Subtracao lhs rhs)       env = avaliarBinExp lhs rhs (-) env
avaliar (Multiplicacao lhs rhs)   env = avaliarBinExp lhs rhs (*) env
avaliar (Divisao lhs rhs)         env = avaliarBinExp lhs rhs div env
avaliar (Ref id)                  env = avaliacaoStrict $ pesquisar id env
avaliar (Lambda id exp)           env = (FClosure id exp env)
avaliar (Let id ascExp bodyExp)   env = avaliar (Aplicacao (Lambda id bodyExp) ascExp) env

avaliar (Aplicacao funExp argExp) env =
    case fun_val of
        (FClosure id expr env') -> avaliar expr ((id, arg_val):env')
        otherwise -> error "Applying non-anonymous function."
    where
        fun_val = avaliacaoStrict (avaliar funExp env)
        arg_val = EClosure argExp env

avaliar (If0 test pass fail)      env =
    if (isZero (avaliar test env))
        then avaliar pass env
        else avaliar fail env


-- avalia expressão de teste para um If0
isZero :: ValorE -> Bool
isZero (VInt n)
    | n == 0 = True
    | otherwise = False


-- avaliação seguindo estratégia strict
avaliacaoStrict :: ValorE -> ValorE
avaliacaoStrict (EClosure expr env) = avaliacaoStrict (avaliar expr env)
avaliacaoStrict exp = exp


-- pesquisa identificador mapeado a um valor em um abiente de substituições postergadas
pesquisar :: Id -> Env -> ValorE
pesquisar id [] = error ("Undeclared variable \"" ++ id ++ "\".")
pesquisar id ((i, e):xs)
    | id == i = e
    | otherwise = pesquisar id xs


-- avalia expressão binária
avaliarBinExp :: Expressao -> Expressao -> (Int -> Int -> Int) -> Env -> ValorE
avaliarBinExp lhs rhs op env = VInt (op vl vr)
    where
        (VInt vl) = avaliacaoStrict (avaliar lhs env)
        (VInt vr) = avaliacaoStrict (avaliar rhs env)

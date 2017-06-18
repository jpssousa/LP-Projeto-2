
module LFCFD_develop where

type Id = String

type Env = [(Id, CFAE_L_Value)]


data CFAE_L = Value Int
            | Plus CFAE_L CFAE_L
            | Subt CFAE_L CFAE_L
            | Mult CFAE_L CFAE_L
            | Div  CFAE_L CFAE_L
            | Let    Id CFAE_L CFAE_L
            | Lambda Id CFAE_L
            | App  CFAE_L CFAE_L
            | If0  CFAE_L CFAE_L CFAE_L
            | Ref Id
    deriving (Show, Eq)


data CFAE_L_Value = NumV Int
                  | ClosureV Id CFAE_L Env
                  | ExprV CFAE_L Env
    deriving (Show, Eq)


eval :: CFAE_L -> Env -> CFAE_L_Value
eval (Value n)       _   = NumV n
eval (Plus lhs rhs)  env = evalBinExp lhs rhs (+) env
eval (Subt lhs rhs)  env = evalBinExp lhs rhs (-) env
eval (Mult lhs rhs)  env = evalBinExp lhs rhs (*) env
eval (Div lhs rhs)   env = evalBinExp lhs rhs div env
eval (Ref id)        env = strictEval $ lookUp id env
eval (Lambda id exp) env = (ClosureV id exp env)
eval (Let id ascExp bodyExp) env = eval (App (Lambda id bodyExp) ascExp) env
eval (App funExp argExp)   env =
    case fun_val of
        (ClosureV id expr env') -> eval expr ((id, arg_val):env')
        otherwise -> error "Applying non-anonymous function."
    where
        fun_val = strictEval (eval funExp env)
        arg_val = ExprV argExp env
eval (If0 test pass fail) env =
    if (isZero (eval test env))
        then eval pass env
        else eval fail env


-- avalia expressão de teste para um If0
isZero :: CFAE_L_Value -> Bool
isZero (NumV n)
    | n == 0 = True
    | otherwise = False


-- avaliação seguindo estratégia strict
strictEval :: CFAE_L_Value -> CFAE_L_Value
strictEval (ExprV expr env) = strictEval (eval expr env)
strictEval exp = exp


-- pesquisa identificador mapeado a um valor em um abiente de substituições postergadas
lookUp :: Id -> Env -> CFAE_L_Value
lookUp id [] = error ("Undeclared variable \"" ++ id ++ "\".")
lookUp id ((i, e):xs)
    | id == i = e
    | otherwise = lookUp id xs


-- avalia expressão binária
evalBinExp :: CFAE_L -> CFAE_L -> (Int -> Int -> Int) -> Env -> CFAE_L_Value
evalBinExp lhs rhs op env = NumV (op vl vr)
    where
        (NumV vl) = strictEval (eval lhs env)
        (NumV vr) = strictEval (eval rhs env)

type Value = Int
type Var   = Int
type Stack = [Value]
type Frame = [Value]
type Code = [Instruction]

data Instruction
  = PUSH Value
  | LOAD Var
  | STORE Var
  | OP (Value -> Value -> Value)
  | UOP (Value -> Value)
  | IF (Value -> Value -> Bool) Code
  | DUP
  | SWAP
  | POP
  | NOP
  | RETURN


load :: Var -> Frame -> Value
load _ []       = 0
load 0 (v : _)  = v
load n (_ : vs) = load (n - 1) vs

store :: Var -> Value -> Frame -> Frame
store 0 v []       = [v]
store 0 v (_ : vs) = v : vs
store n v []       = 0 : store (n - 1) v []
store n v (w : vs) = w : store (n - 1) v vs

run :: Code -> Frame -> Value
run = aux []
  where
    aux :: Stack -> Code -> Frame -> Value
    aux (v : [])     (RETURN : _)   _  = v
    aux vs           (PUSH v : is)  fr = aux (v : vs) is fr
    aux vs           (LOAD x : is)  fr = aux (load x fr : vs) is fr
    aux (v : vs)     (STORE x : is) fr = aux vs is (store x v fr)
    aux (w : v : vs) (OP f : is)    fr = aux (f v w : vs) is fr
    aux (v : vs) (UOP f : is)    fr = aux (f v : vs) is fr
    aux (w : v : vs) (IF p is : _)  fr | p v w = aux vs is fr
    aux (_ : _ : vs) (IF _ _ : is)  fr = aux vs is fr
    aux (v : vs) (DUP : is) fr = aux (v : v : vs) is fr
    aux (v : vs) (POP : is) fr = aux vs is fr
    aux (w : v : vs) (SWAP : is) fr = aux (v : w : vs) is fr
    aux vs (NOP : is)  fr = aux vs is fr


-- esempi
fattoriale :: Code
fattoriale = init
  where
    init = PUSH 1 :
           STORE res :
           loop
    loop = LOAD n :
           PUSH 0 :
           IF (==) fine :
           LOAD n :
           LOAD res :
           OP (*) :
           STORE res :
           LOAD n :
           PUSH 1 :
           OP (-) :
           STORE n :
           loop
    fine = LOAD res :
           RETURN : []
    n    = 0
    res  = 1

fibonacci :: Code
fibonacci = init
  where
    init = PUSH 0 :
           STORE m :
           PUSH 1 :
           STORE n :
           loop
    loop = LOAD k :
           PUSH 0 :
           IF (==) fine :
           LOAD n :
           LOAD n :
           LOAD m :
           OP (+) :
           STORE n :
           STORE m :
           LOAD k :
           PUSH 1 :
           OP (-) :
           STORE k :
           loop
    fine = LOAD m :
           RETURN : []
    k    = 0
    m    = 1
    n    = 2

--esercizi
mcd :: Code
mcd = init 
    where
        init =  LOAD b :
                LOAD a :
                IF (<) swap :
                loop
        loop =  LOAD b :
                PUSH 0 :
                IF (==) end:
                LOAD a :
                LOAD b : 
                OP mod :
                LOAD b :
                STORE a :
                STORE b :
                loop
        swap =  LOAD b :
                LOAD a :
                STORE b :
                STORE a :
                loop
        end =   LOAD a :
                RETURN : []
        --variable name
        a = 0
        b = 1

ineg :: Code
ineg = UOP (\x -> 0 - x)
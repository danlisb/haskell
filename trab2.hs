-- Definição das árvore sintática para representação dos programas:
data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E
      | Igual E E  -- verifica se duas expressões aritméticas são iguais
   deriving(Eq,Show)

data C = While B C
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
    | Throw -- gera uma exceção
    | Try C C -- Try C1 C2 --tenta executar C1, caso ocorra exceção, executa o catch (C2). Caso não ocorra exceção em C1, C2 nunca é executado
    | ThreeTimes C   ---- Executa o comando C 3 vezes
    | DoWhile C B --- DoWhile C B: executa C enquanto B é verdadeiro
    | Loop C E      ---- Loop E C: executa E vezes o comando C 
    | Assert B C --- Assert B C: caso B seja verdadeiro, executa o comando C
    | ExecWhile E E C -- ExecWhile E1 E2 C: Enquanto a expressão E1 for menor que a expressão E2, executa C 
    | DAtrrib E E E E -- Dupla atribuição: recebe duas variáveis "e1" e "e2" e duas expressões "e3" e "e4". Faz e1:=e3 e e2:=e4.
   deriving(Eq,Show)

-----------------------------------------------------
-----
----- As próximas funções, servem para manipular a memória (sigma)
-----
------------------------------------------------


--- A próxima linha de código diz que o tipo memória é equivalente a uma lista de tuplas, onde o
--- primeiro elemento da tupla é uma String (nome da variável) e o segundo um Inteiro
--- (conteúdo da variável):
type Memoria = [(String,Int)]

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",0)]


--- A função procuraVar recebe uma memória, o nome de uma variável e retorna o conteúdo
--- dessa variável na memória. Exemplo:
---
--- *Main> procuraVar exSigma "x"
--- 10


procuraVar :: Memoria -> String -> Int
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVar ((s,i):xs) v
  | s == v     = i
  | otherwise  = procuraVar xs v


--- A função mudaVar, recebe uma memória, o nome de uma variável e um novo conteúdo para essa
--- variável e devolve uma nova memória modificada com a varíável contendo o novo conteúdo. A
--- chamada
---
--- *Main> mudaVar exSigma "temp" 20
--- [("x",10),("temp",20),("y",0)]
---
---
--- essa chamada é equivalente a operação exSigma[temp->20]

mudaVar :: Memoria -> String -> Int -> Memoria
mudaVar [] v n = error ("Variavel " ++ v ++ " nao definida no estado")
mudaVar ((s,i):xs) v n
  | s == v     = (s,n):xs
  | otherwise  = (s,i): mudaVar xs v n


-- Função auxiliar para verificar se uma expressão é um valor (número)
ehValorE :: E -> Bool
ehValorE (Num _) = True
ehValorE _       = False
-------------------------------------
---


--- Completar os casos comentados das seguintes funções:
---
---------------------------------

smallStepE :: (E, Memoria) -> (E, Memoria)

smallStepE (Var x, s)                  = (Num (procuraVar s x), s)

smallStepE (Num x, s)                  = (Num x, s)

smallStepE (Soma (Num n1) (Num n2), s) = (Num (n1 + n2), s)
smallStepE (Soma (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Soma (Num n) el, sl)
smallStepE (Soma e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Soma el e2,sl)

smallStepE (Mult (Num n1) (Num n2), s) = (Num (n1 * n2), s)
smallStepE (Mult (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                          in (Mult (Num n) el, sl)
smallStepE (Mult e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                          in (Mult el e2,sl)

smallStepE(Sub (Num n1) (Num n2), s)   = (Num (n1 - n2), s)
smallStepE (Sub (Num n) e, s)          = let (el,sl) = smallStepE (e,s)
                                         in (Sub (Num n) el, sl)
smallStepE (Sub e1 e2,s)               = let (el,sl) = smallStepE (e1,s)
                                         in (Sub el e2,sl)


smallStepB :: (B,Memoria) -> (B, Memoria)

smallStepB (Not TRUE, s)  = (FALSE, s)
smallStepB (Not FALSE, s) = (TRUE, s)
smallStepB (Not b, s)     = let (b', s') = smallStepB (b, s)
                             in (Not b', s')

smallStepB (And TRUE b2, s)  = (b2, s)
smallStepB (And FALSE b2, s) = (FALSE, s)
smallStepB (And b1 b2, s)    = let (b1', s') = smallStepB (b1, s)
                                in (And b1' b2, s')

smallStepB (Or TRUE b2, s)   = (TRUE, s)
smallStepB (Or FALSE b2, s)  = (b2, s)
smallStepB (Or b1 b2, s)     = let (b1', s') = smallStepB (b1, s)
                                in (Or b1' b2, s')

smallStepB (Leq (Num n1) (Num n2), s) = (if n1 <= n2 then TRUE else FALSE, s)
smallStepB (Leq (Num n) e2, s)        = let (e2', s') = smallStepE (e2, s)
                                         in (Leq (Num n) e2', s')
smallStepB (Leq e1 e2, s)             = let (e1', s') = smallStepE (e1, s)
                                         in (Leq e1' e2, s')

 -- recebe duas expressões aritméticas e devolve um valor booleano dizendo se são iguais
smallStepB (Igual (Num n1) (Num n2), s) = (if n1 == n2 then TRUE else FALSE, s)
smallStepB (Igual (Num n) e2, s)        = let (e2', s') = smallStepE (e2, s)
                                           in (Igual (Num n) e2', s')
smallStepB (Igual e1 e2, s)             = let (e1', s') = smallStepE (e1, s)
                                           in (Igual e1' e2, s')


smallStepC :: (C,Memoria) -> (C,Memoria)

smallStepC (If TRUE c1 c2, s)  = (c1, s)
smallStepC (If FALSE c1 c2, s) = (c2, s)
smallStepC (If b c1 c2, s)     = let (b', s') = smallStepB (b, s)
                                 in (If b' c1 c2, s')

smallStepC (Seq Skip c2, s) = (c2, s)
smallStepC (Seq c1 c2, s)   = let (c1', s') = smallStepC (c1, s)
                              in (Seq c1' c2, s')

smallStepC (Atrib (Var x) (Num n), s) = (Skip, mudaVar s x n)
smallStepC (Atrib (Var x) e, s)       = let (e', s') = smallStepE (e, s)
                                       in (Atrib (Var x) e', s')

smallStepC (While b c, s) = (If b (Seq c (While b c)) Skip, s)

smallStepC (Throw, s) = (Throw, s) -- permanece em Throw (propaga exceção)

 -- Try C1 C2 --tenta executar C1, caso ocorra exceção, executa o catch (C2). Caso não ocorra exceção em C1, C2 nunca é executado
 -- gera uma exceção
smallStepC (Try Throw c2, s) = (c2, s)
smallStepC (Try Skip c2, s)  = (Skip, s)
smallStepC (Try c1 c2, s)    = let (c1', s') = smallStepC (c1, s)
                               in case c1' of
                                    Throw -> (Try Throw c2, s')
                                    _     -> (Try c1' c2, s')

smallStepC (ThreeTimes c, s) = (Seq c (Seq c c), s) -- Executa o comando C 3 vezes

smallStepC (DoWhile c b, s) = (Seq c (While b c), s) -- DoWhile C B: executa C enquanto B é verdadeiro

-- Loop E C: executa E vezes o comando C 
smallStepC (Loop c (Num 0), s) = (Skip, s)
smallStepC (Loop c (Num n), s) = (Seq c (Loop c (Num (n - 1))), s)
smallStepC (Loop c e, s)       = let (e', s') = smallStepE (e, s)
                                 in (Loop c e', s')

-- Assert B C: caso B seja verdadeiro, executa o comando C
smallStepC (Assert TRUE c, s)  = (c, s)
smallStepC (Assert FALSE c, s) = (Throw, s)
smallStepC (Assert b c, s)     = let (b', s') = smallStepB (b, s)
                                 in (Assert b' c, s')

 -- ExecWhile E1 E2 C: Enquanto a expressão E1 for menor que a expressão E2, executa C 
smallStepC (ExecWhile (Num n1) (Num n2) c, s)
  | n1 < n2  = (Seq c (ExecWhile (Num (n1 + 1)) (Num n2) c), s)
  | otherwise = (Skip, s)

smallStepC (ExecWhile e1 e2 c, s)
  | not (ehValorE e1) = let (e1', s') = smallStepE (e1, s)
                        in (ExecWhile e1' e2 c, s')
smallStepC (ExecWhile e1 e2 c, s)
  | not (ehValorE e2) = let (e2', s') = smallStepE (e2, s)
                        in (ExecWhile e1 e2' c, s')

-- Dupla atribuição: recebe duas variáveis "e1" e "e2" e duas expressões "e3" e "e4". Faz e1:=e3 e e2:=e4.
-- Caso final: ambas variáveis e expressões já são valores
smallStepC (DAtrrib (Var x1) (Var x2) (Num v1) (Num v2), s) =
  (Skip, mudaVar (mudaVar s x1 v1) x2 v2)

-- Reduz e3 (expressão da primeira atribuição)
smallStepC (DAtrrib (Var x1) (Var x2) e3 e4, s)
  | not (ehValorE e3) = let (e3', s') = smallStepE (e3, s)
                        in (DAtrrib (Var x1) (Var x2) e3' e4, s')

-- Reduz e4 (expressão da segunda atribuição)
smallStepC (DAtrrib (Var x1) (Var x2) (Num v1) e4, s)
  | not (ehValorE e4) = let (e4', s') = smallStepE (e4, s)
                        in (DAtrrib (Var x1) (Var x2) (Num v1) e4', s')


----------------------
--  INTERPRETADORES
----------------------

--- Interpretador para Expressões Aritméticas:
isFinalE :: E -> Bool
isFinalE (Num n) = True
isFinalE _       = False


interpretadorE :: (E,Memoria) -> (E, Memoria)
interpretadorE (e,s) = if isFinalE e then (e,s) else interpretadorE (smallStepE (e,s))

--- Interpretador para expressões booleanas


isFinalB :: B -> Bool
isFinalB TRUE    = True
isFinalB FALSE   = True
isFinalB _       = False

-- Descomentar quanto a função smallStepB estiver implementada:
interpretadorB :: (B,Memoria) -> (B, Memoria)
interpretadorB (b,s) = if isFinalB b then (b,s) else interpretadorB (smallStepB (b,s))


-- Interpretador da Linguagem Imperativa
isFinalC :: C -> Bool
isFinalC Skip    = True
isFinalC _       = False

-- Descomentar quando a função smallStepC estiver implementada:

interpretadorC :: (C,Memoria) -> (C, Memoria)
interpretadorC (c,s) = if isFinalC c then (c,s) else interpretadorC (smallStepC (c,s))


--------------------------------------
---
--- Exemplos de programas para teste
---
--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS DE PROGRAMAS QUE USEM 
--- OS COMANDOS NOVOS PRINCIPALMENTE O TRATAMENTO DE EXCEÇÕES
--

progTry1 :: C
progTry1 = Try
  (Seq
    (Atrib (Var "x") (Num 1))
    (Seq Throw (Atrib (Var "x") (Num 999))) -- esse nunca executa
  )
  (Atrib (Var "x") (Num 100)) -- catch: executado após throw
-- ao final, "x" deve valer 100.

-- Se x <= 5, y := 42. Caso contrário, lança Throw.
progAssert1 :: C
progAssert1 = Assert (Leq (Var "x") (Num 5)) (Atrib (Var "y") (Num 42))

progThreeTimes :: C
progThreeTimes =
  ThreeTimes (Atrib (Var "x") (Soma (Var "x") (Num 1)))

-- Executa x := x+1 até x > 3. Garante pelo menos uma execução.
progDoWhile :: C
progDoWhile =
  DoWhile
    (Atrib (Var "x") (Soma (Var "x") (Num 1)))
    (Leq (Var "x") (Num 3))

-- Executa y := y + 2, 3 vezes
progLoop :: C
progLoop =
  Loop (Atrib (Var "y") (Soma (Var "y") (Num 2))) (Num 3)

-- Enquanto x < 5, incrementa y e x também é incrementado implicitamente dentro de ExecWhile.
progExecWhile :: C
progExecWhile =
  ExecWhile (Var "x") (Num 5) (Atrib (Var "y") (Soma (Var "y") (Num 1)))

-- Atribui x := x+1 e y := 10. 
progDAtrrib :: C
progDAtrrib =
  DAtrrib (Var "x") (Var "y") (Soma (Var "x") (Num 1)) (Num 10)

-- x := 1 executa, depois Throw, então x := 999 é ignorado.
progThrowInSeq :: C
progThrowInSeq =
  Seq
    (Atrib (Var "x") (Num 1))
    (Seq
      Throw
      (Atrib (Var "x") (Num 999)) -- esse nunca é executado
    )

-- Assert falha dentro de Try → Catch é executado
--  Se x != 100, y := 42.
progAssertFailCatch :: C
progAssertFailCatch =
  Try
    (Assert (Igual (Var "x") (Num 100)) (Atrib (Var "y") (Num 999))) -- falha se x ≠ 100
    (Atrib (Var "y") (Num 42)) -- catch

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]


---
--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse
--- programa já é possível rodar com a implementação que fornecida:

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

---
--- para rodar:
-- A função smallStepE anda apenas um passo na avaliação da Expressão

-- *Main> smallStepE (progExp1, exSigma)
-- (Soma (Num 3) (Soma (Num 10) (Var "y")),[("x",10),("temp",0),("y",0)])

-- Note que no exemplo anterior, o (Var "x") foi substituido por (Num 10)

-- Para avaliar a expressão até o final, deve-se usar o interpretadorE:

-- *Main> interpretadorE (progExp1 , exSigma)
-- (Num 13,[("x",10),("temp",0),("y",0)])

-- *Main> interpretadorE (progExp1 , exSigma2)
-- (Num 6,[("x",3),("y",0),("z",0)])


--- Para rodar os próximos programas é necessário primeiro implementar as regras que faltam
--- e descomentar os respectivos interpretadores


---
--- Exemplos de expressões booleanas:


teste1 :: B
teste1 = Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3))

teste2 :: B
teste2 = Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3))


---
-- Exemplos de Programas Imperativos:

testec1 :: C
testec1 = Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y")))
               (Atrib (Var "y") (Var "z"))

fatorial :: C
fatorial = Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1)))))
-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
      |Div E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E    -- menor ou igual
      | Igual E E  -- verifica se duas expressões aritméticas são iguais
   deriving(Eq,Show)

data C = While B C
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
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

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]

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
  | s == v     = ((s,n):xs)
  | otherwise  = (s,i): mudaVar xs v n


-------------------------------------
---
--- Completar os casos comentados das seguintes funções:
---
---------------------------------

ebigStep :: (E,Memoria) -> Int
ebigStep (Var x,s) = procuraVar s x
ebigStep (Num n,s) = n
ebigStep (Soma e1 e2,s)  = ebigStep (e1,s) + ebigStep (e2,s)
ebigStep (Sub e1 e2,s)   = ebigStep (e1,s) - ebigStep (e2,s)
ebigStep (Mult e1 e2,s)  = ebigStep (e1,s) * ebigStep (e2,s)
ebigStep (Div e1 e2,s)
  | ebigStep (e2,s) == 0 = error "Divisão por zero"
  | otherwise            = ebigStep (e1,s) `div` ebigStep (e2,s)


bbigStep :: (B,Memoria) -> Bool
bbigStep (TRUE,s)  = True
bbigStep (FALSE,s) = False
bbigStep (Not b,s) = not (bbigStep (b,s))
bbigStep (And b1 b2,s ) = bbigStep (b1,s) && bbigStep (b2,s)
bbigStep (Or b1 b2,s )  = bbigStep (b1,s) || bbigStep (b2,s)
bbigStep (Leq e1 e2,s)  = ebigStep (e1,s) <= ebigStep (e2,s)
bbigStep (Igual e1 e2,s) = ebigStep (e1,s) == ebigStep (e2,s)


cbigStep :: (C,Memoria) -> (C,Memoria)
cbigStep (Skip,s) = (Skip,s)

cbigStep (If b c1 c2, s) =
  if bbigStep (b,s)
     then cbigStep (c1,s)
     else cbigStep (c2,s)

cbigStep (Seq c1 c2, s) =
  let (c1', s') = cbigStep (c1,s)
  in case c1' of
       Skip -> cbigStep (c2,s')
       _    -> (Seq c1' c2, s')

cbigStep (Atrib (Var x) e, s) =
  let v = ebigStep (e,s)
  in (Skip, mudaVar s x v)

cbigStep (While b c, s) =
  if bbigStep (b,s)
     then cbigStep (Seq c (While b c), s)
     else (Skip, s)

cbigStep (ThreeTimes c, s) =
  cbigStep (Seq c (Seq c c), s)

cbigStep (DoWhile c b, s) =
  let (c', s') = cbigStep (c,s)
  in if bbigStep (b,s')
        then cbigStep (DoWhile c b, s')
        else (Skip, s')

cbigStep (Loop c e, s) =
  let n = ebigStep (e,s)
  in if n <= 0
        then (Skip, s)
        else cbigStep (Seq c (Loop c (Sub e (Num 1))), s)

cbigStep (Assert b c, s) =
  if bbigStep (b,s)
     then cbigStep (c,s)
     else (Skip, s)

cbigStep (ExecWhile e1 e2 c, s) =
  let v1 = ebigStep (e1, s)
      v2 = ebigStep (e2, s)
  in if v1 < v2
     then let (_, s') = cbigStep (c, s)     -- executa o comando C atualizando a memória
          in cbigStep (ExecWhile e1 e2 c, s') -- repete com a memória atualizada
     else (Skip, s)


cbigStep (DAtrrib (Var x1) (Var x2) e1 e2, s) =
  let v1 = ebigStep (e1,s)
      v2 = ebigStep (e2,s)
      s1 = mudaVar s x1 v1
      s2 = mudaVar s1 x2 v2
  in (Skip, s2)


--------------------------------------
---
--- Exemplos de programas para teste
---
--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS DE PROGRAMAS QUE USEM:
--- * Loop
--- * Dupla Atribuição
--- * Do While
--- * Assert
--- * ExecWhile

-- Loop que soma 1 à variável "y" 5 vezes:
progLoop :: C
progLoop = Loop (Atrib (Var "y") (Soma (Var "y") (Num 1))) (Num 5) 

-- Troca os valores das variáveis "x" e "y" usando variável temporária "temp":
progDuplaAtrib :: C
progDuplaAtrib = 
  Seq (Atrib (Var "temp") (Var "x"))
      (DAtrrib (Var "x") (Var "y") (Var "y") (Var "temp"))

-- Soma 1 à variável "y" começando de 0 até que "y" seja igual a 3:
progDoWhile :: C
progDoWhile = 
  DoWhile 
    (Atrib (Var "y") (Soma (Var "y") (Num 1)))
    (Not (Igual (Var "y") (Num 3)))

-- Executa a atribuição apenas se x <= y:
progAssert :: C
progAssert = Assert (Leq (Var "x") (Var "y")) (Atrib (Var "z") (Num 999))

-- Esse programa simula a soma dos números de 1 a 5 usando ExecWhile.
--Ele faz: y = y + x de x = 1 até 5. Resultado final: y = 1 + 2 + 3 + 4 + 5 = 15
progExecWhile = ExecWhile (Var "x") (Num 15)
  (Seq 
    (Atrib (Var "y") (Soma (Var "y") (Var "x")))
    (Atrib (Var "x") (Soma (Var "x") (Num 1)))
  )


---
--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse
--- programa já é possível rodar com a implementação inicial  fornecida:

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

---
--- para rodar:
-- *Main> ebigStep (progExp1, exSigma)
-- 13
-- *Main> ebigStep (progExp1, exSigma2)
-- 6

--- Para rodar os próximos programas é necessário primeiro implementar as regras da semântica
---


---
--- Exemplos de expressões booleanas:

-- *Main> bbigStep (teste1, exSigma) -> True, porque 3+3 = 6 <= 2*3=6
teste1 :: B
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))

-- teste2 deve dar False com exSigma, pois 13 <= 6 é falso
teste2 :: B
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))


---
-- Exemplos de Programas Imperativos:

testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
               (Atrib (Var "y") (Var "z")))

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))

-- resumo para rodar os exemplos:
-- ebigStep (expressao, memoria)
-- bbigStep (expressao_booleana, memoria)
-- cbigStep (programa, memoria)
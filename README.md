# Como rodar exemplos do `trab1.hs`

Este projeto implementa uma linguagem imperativa simples com semântica big-step em Haskell.

---

Abra o terminal e digite:

```bash
ghci
:l trab1.hs
ebigStep (progExp1, exSigma)
-- Esperado: 13
bbigStep (teste1, exSigma)
-- Esperado: True
cbigStep (progDoWhile, exSigma)
-- Resultado: ((Skip,[("x",10),("temp",0),("y",3)]))
```

# Como rodar exemplos do `trab2.hs`

Este projeto implementa um interpretador baseado na semântica operacional *small-step* para uma linguagem imperativa estendida, com suporte a:
- Expressões aritméticas e booleanas
- Atribuições, sequenciamento, condicionais e laços
- Tratamento de exceções (`Throw`, `Try-Catch`)
- Comandos personalizados como `ThreeTimes`, `DoWhile`, `Loop`, `Assert`, `ExecWhile`, `DAtrrib`

---

## 📁 Estrutura do Projeto

O código-fonte está organizado em um único arquivo contendo:

- Definições de tipos de dados (`E`, `B`, `C`)
- Manipulação de memória (`Memoria`, `procuraVar`, `mudaVar`)
- Avaliação *small-step* e interpretadores
- Programas de exemplo com destaque para os comandos novos

  Abra o terminal e digite:

```bash
ghci
:l trab2.hs
interpretadorE (progExp1, exSigma)
interpretadorB (teste2, exSigma2)
interpretadorC (progTry1, exSigma)
Exemplo:
*Main> interpretadorC (progTry1, exSigma)
(Skip,[("x",100),("temp",0),("y",0)])
```

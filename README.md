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

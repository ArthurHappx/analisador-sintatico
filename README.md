# Analisador Sintático em Haskell

Projeto desenvolvido para a disciplina de Paradigmas de Linguagens de Programação, com foco educacional na implementação de um analisador léxico e sintático voltado para uma “sublinguagem” de Python.

## 👨‍💻 Integrantes

- Arthur Félix — `ArthurHappx`
- Daniel Damião — `DanielPDamiao`
- João Moitinho — `DevMoitinho`
- Arthur Rodrigues — `Arthyp`

## 📄 Especificação do trabalho

A descrição formal do projeto pode ser encontrada no arquivo:

➡️ [especificacao-projeto-plp.pdf](especificacao-projeto-plp.pdf)

---

## 🚀 Execução

```bash
cabal run analisador
>>[args]
```

args:
```bash
../path/arquivo.py          # análise normal
../path/arquivo.py -s       # análise e salva o resultado em ./ast_results
--help                      # mostra esta ajuda
-tests                      # executa testes internos e salva em ./test/Logs
exit()                      # encerra o programa
```

# Analisador Sintático em Haskell

![language-haskell](https://img.shields.io/badge/language-Haskell-purple) ![haskell2010](https://img.shields.io/badge/standard-Haskell2010-blueviolet) ![ghc](https://img.shields.io/badge/ghc-9.6.7-brightgreen) ![cabal](https://img.shields.io/badge/cabal-3.12.1.0-orange) ![version](https://img.shields.io/badge/version-v0.1.0-green) ![license](https://img.shields.io/badge/license-MIT-lightgrey)

Projeto desenvolvido para a disciplina de Paradigmas de Linguagens de Programação, com foco educacional na implementação de um analisador léxico e sintático voltado para uma “sublinguagem” de Python. 

🔗 Versão em Prolog: [analisador-sintatico-prolog](https://github.com/DanielPDamiao/analisador-sintatico) 

---

## 👨‍💻 Integrantes

- Arthur Félix — [`ArthurHappx`](https://github.com/ArthurHappx)
- Daniel Damião — [`DanielPDamiao`](https://github.com/DanielPDamiao)
- João Moitinho — [`DevMoitinho`](https://github.com/DevMoitinho)
- Arthur Rodrigues — [`Arthyp`](https://github.com/Arthyp)

---

## 📄 Especificação do trabalho

A descrição formal do projeto pode ser encontrada no arquivo:

➡️ [especificacao-projeto-plp.pdf](especificacao-projeto-plp.pdf)

---

## 🚀 Execução

Argumentos suportados:
```bash
cabal run analisador        # certifique-se de estar na pasta raiz
```
```bash
"Analisador Léxico-Sintático (v0.1.0.0)"
"Digite '--help' para main informações."
>>> [args]
```

Argumentos possíveis:
```bash
../path/arquivo.py          # análise normal, exibe ast visual no terminal
../path/arquivo.py -s       # analisa e salva o resultado em ./ast_results/arquivo_ast.txt
--help                      # mostra esta ajuda
-tests                      # executa testes internos e salva em ./test/Logs/arquivo_ast.txt
exit()                      # encerra o programa
```
<blockquote style="background-color: transparent; padding: 8px 14px; border-left: 4px solid rgba(0,0,0,0.35);">
📝 <strong>Observação:</strong><br>
O caminho (<code>path</code>) considerado na análise de arquivos é relativo à raiz do projeto.
</blockquote>
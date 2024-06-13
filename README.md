# Decaf 
[![CI](https://github.com/zhangjunphy/decaf/actions/workflows/haskell.yml/badge.svg)](https://github.com/zhangjunphy/decaf/actions/workflows/haskell.yml) 
[![Coverage](https://raw.githubusercontent.com/zhangjunphy/decaf/gh-pages/coverage/badge.svg)](https://zhangjunphy.github.io/decaf/coverage/hpc_index.html)
[![Haddock](https://img.shields.io/badge/Haddock-Decaf-blue?logo=haskell)](https://zhangjunphy.github.io/decaf/haddock/index.html)

An implementation of the Decaf language.

## Status
Currently scan/parse/cfg phases are implemented and should work for most cases.
`-t scan` generates the token stream. `-t parse` outputs the abstract syntex tree(AST) in text format. `-t cfg` generates a dot plot of the control flow graph(CFG), which could be furthur processed by the graphviz tool to give a graphical view of the CFG.


This implementation relies on [Alex](https://github.com/haskell/alex) for lexing and [Happy](https://github.com/haskell/happy) for parsing. The definition of lexmes are in [src/Lexer/Lex.x](src/Lexer/Lex.x). The grammar is in [src/Parser/Grammar.y](src/Parser/Grammar.y).


The parser and semantic might not be exactly conformant to the [decaf spec](https://cons.mit.edu/fa18/handout-pdfs/01-decaf-spec.pdf). Rather it should be a superset. All valid decaf programs should be compiled to code with the expected behavior. In addition the implementation should also be able to compile some other intuitively reasonable programs which might be invalid in the spec.

## TODOs
* Unoptimized code generation
* Compile to LLVM IR
* Other improvements
  * AST output format

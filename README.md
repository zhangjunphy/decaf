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


Curently the LLVM IR backend is mostly working. The compiler should be able to compile some simple code snippets. For example a "hellow world":
```
import printf;

void main() {
  printf("Hello world!\n");
}
```

Save the above code into a `hello_world.dcf`, then compile it with
```bash 
stack run -- decafc -t llvm hello_world.dcf > hello_world.ll
```

Then the generated LLVM IR can be further compiled and linked into an executable 
```bash
llc -filetype=obj hello_world.ll -o hello_world.o
clang hello_world.o -o hello_world
```

## Some random comments
Programming in Haskell is an interesting experience. The design of this language is dramatically different from most others. Unfortunately the tooling and the ecosystems are quite lacking. The Haskell language server is OK, however debugging tools are practically non-exist. You can debug simple scripts with an interactive session or `Debug.Trace` to print some information. But as the complexity of your program goes up, tools like GDB are really important to save some headaches. Also it seems many of the Haskell packages are either short of maintainers or having quality issues. The LLVM binding has been stagnant since a few years ago. It is hard to imagine this could happen for a language which was claimed to be good for compiler development. The GraphViz package which I used at some point had bugs when handling escape sequences, because of which I have to write my own graph drawing functions.

It is a pity to watch such a great language gradually fall into its current status. Initially the plan was to implement the compiler all the way through. But the shortcomings of this language is becoming more than annoying. After the LLVM IR backend this whole stuff is working like a compiler at least. So it is probably a good time to consider looking for another language which might be better for the task and restart there.

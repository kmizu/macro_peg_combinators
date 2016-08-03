## Macro PEG Combinators: A Macro PEG implementation
 
[![Gitter](https://badges.gitter.im/kmizu/macro_peg_combinators.svg)](https://gitter.im/kmizu/macro_peg?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)
[![Build Status](https://travis-ci.org/kmizu/macro_peg_combinators.png?branch=master)](https://travis-ci.org/kmizu/macro_peg_combinators)

Macro PEG Combinators is an implementation of Macro PEG by parser combinators. 

### Grammar of Macro PEG in Pseudo PEG

Note that spacing is ommited.

    Grammer <- Rule* ";";
    
    Rule <- Identifier ("(" Identifier ("," Identifer)* ")")? "=" Expression ";";
    
    Expression <- Sequence ("/" Sequence)*;
    
    Sequence <- Prefix+;
    
    Prefix <-  ("&" / "!") Suffix;
    
    Suffix <- Primary "+"
            /  Primary "*"
            /  Primary "?"
            /  Primary;
    
    Primary <- "(" Expression ")"
             /  Call
             / Identifier
             / StringLiteral
             / CharacterClass;
             
    Debug <- "Debug" "(" Expression ")";
    
    StringLiteral <- "\\" (!"\\" .) "\\";
    
    Call <- Identifier "(" Expression ("," Expression)* ")";
    
    Identifier <- [a-zA-Z_] ([a-zA-Z0-9_])*;
    
    CharacterClass <- "[" "^"? (!"[" .)+ "]"
    
### Release Note

### Usage

Note that the behaviour could change.

Add the following lines to your build.sbt file:

```scala
libraryDependencies += "com.github.kmizu" %% "macro_peg_combinators" % "0.0.1"
```

Then, you can use `Parsers` and `MacroPEGEvaluator` as the followings:

```scala
import com.github.kmizu.macro_peg_combinators._
```

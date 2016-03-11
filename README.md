Jawa [![Gitter](https://badges.gitter.im/sireum/jawa.svg)](https://gitter.im/sireum/jawa?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge) 
========================

This is official reporitory for the Jawa. Jawa is a subset of pilar IR, which are carefully choosen to represent java&dalvik bytecodes.

## Repository structure

```
jawa/
+--org.sireum                     Import all nesasarry projects or libs from Sireum. Used for support eclipse plugin dependency management.
+--sireum-jawa                    Core static analysis data structures, "*.class"&"*.pilar" file managing, class hierarchy, method body resolving, etc.
+--sireum-jawa-alir               ALl the flow related analysis for Pilar IR, including call graph building, control flow graph building, data flow analysis, data dependent analysis, points to algorithms, side effect analysis, etc.
+--sireum-jawa-compiler           Independent jawa compiler, which providing interactive nature for jawa parsing, which required for IDE kind of environment.
+--sireum-jawa-test               Test suite.
```

## How to contribute

To contribute to the Jawa Core, Jawa Alir, Jawa Compiler, Jawa Concurrent, please send us a [pull request](https://help.github.com/articles/using-pull-requests/#fork--pull) from your fork of this repository!

For more information on building and developing Amandroid, please also check out our [guidelines for contributing](CONTRIBUTING.md).
 
## What to contribute

If you don't know what to contribute, please check out our [challenges need to resolve](CHALLENGE.md).

#Jawa challenges

Before you take one of the challenges, please contact [`@fgwei`](https://github.com/fgwei) to let him aware. And put a mark (e.g., Resolving by [`@fgwei`](https://github.com/fgwei)) in the end of the challenge to avoid any situation of conflict. You can create a PR following guidance in [CONTRIBUTING.md](CONTRIBUTING.md) after you resolved it. 

##Continuous tasks

`#c1`. All the APIs should be documented. (Post by [`@fgwei`](https://github.com/fgwei))

`#c2`. Error handling in the code need to be cleaned. (Post by [`@fgwei`](https://github.com/fgwei))

`#c3`. Jawa documentations need to be created. (Post by [`@fgwei`](https://github.com/fgwei))(Resolving by [`@fgwei`](https://github.com/fgwei))

##sireum-jawa-alir
In package `org.sireum.jawa.alir.pta.reachingFactsAnalysis.model`:

- `#c4`. We need to add more models for java apis. (Post by [`@fgwei`](https://github.com/fgwei))
- `#c5`. Models actually sharing similar desings, the best way of doing it is designing a DSL to write the model in a simpler way and generate the model codes automatically. (Important!) (Post by [`@fgwei`](https://github.com/fgwei))
- `#c6`. API model need to be redesigned to input/output general datas, which allows multiple points-to analysis can share the same model, e.g., [`SuperSpark`](https://github.com/sireum/jawa/blob/master/sireum-jawa-alir/src/main/scala/org/sireum/jawa/alir/pta/suspark/InterproceduralSuperSpark.scala) and [`RFA`](https://github.com/sireum/jawa/tree/master/sireum-jawa-alir/src/main/scala/org/sireum/jawa/alir/pta/reachingFactsAnalysis).  (Post by [`@fgwei`](https://github.com/fgwei))

`#c7`. In package `org.sireum.jawa.alir.taintAnalysis`, we need to implement monotonic data flow analysis based on demand taint analysis, many situations need such analysis to get better performance. (Post by [`@fgwei`](https://github.com/fgwei))

##sireum-jawa-compiler
`#c8`. In package `org.sireum.jawa.sjc.codegen`, the JavaByteCodeGenerator.scala need to be heavily tested. (Post by [`@fgwei`](https://github.com/fgwei))

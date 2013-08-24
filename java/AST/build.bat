javac -d . Scanner.java Parser.java AST.java Taste.java
jar cfm Taste.jar Taste.manifest Taste/*.class
del Taste\*.class
rd Taste

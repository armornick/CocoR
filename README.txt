The Compiler Generator Coco/R
Hanspeter Mössenböck, Markus Löberbauer, Albrecht Wöß, University of Linz

Last update: July 25, 2011
Documentation | Coco/R for C#, Java, C++, F#, VB.Net, Oberon, other languages | Contributions | Cookbook | Tools | Mailing list

Coco/R is a compiler generator, which takes an attributed grammar of a source language and generates a scanner and a parser for this language. The scanner works as a deterministic finite automaton. The parser uses recursive descent. LL(1) conflicts can be resolved by a multi-symbol lookahead or by semantic checks. Thus the class of accepted grammars is LL(k) for an arbitrary k.

There are versions of Coco/R for different languages (see below). The latest versions from the University of Linz are those for C#, Java and C++, which can be downloaded from this site. An older (non-reentrant) version of Coco/R for C# and Java can be obtained from here.

Coco/R is distributed under the terms of the GNU General Public License (slightly extended). 


-------------------------------------------------
src: http://www.ssw.uni-linz.ac.at/Research/Projects/Coco/
# Deep Demand Analysis for Pointer-Based Data Structures in GHC’s Lazy Core Language

Optimizing a call-by-need lazy language (such as Haskell) for low memory footprint and fast execution comes with unique challenges. Existing strictness analyses perform well for operations on built-in base types (e.g. Int) but do not optimize pointer-based data structures particularly well. Computations over such data structures can cause memory-leaks when the intermediate results are kept as thunks, rather than normalized into values due to laziness. We plan to design and implement a program analysis on GHC’s core language (Core) to identify terms which are “deeply” normalized in all evaluations, thus allowing them to be partially normalized early—reducing memory footprint and in some cases execution time.

There are several potential challenges which we foresee. GHC Core, as a real-world intermediate language for an industrial-strength compiler, is somewhat complex. Additionally, higher-order program (as necessitated by the fact that Haskell is higher-order) are more complicated than those for first-order languages such as those we have thus far discussed in class. We hope to leverage existing work in analysis of object-oriented languages, which present many of the same challenges. Finally, we need to ensure that our analysis is conservative enough to preserve the non-strict semantics of the language—that is, it must be sound with respect to the non-optimized observable behavior of the source program.

Our general plan is to hook into GHC Core to produce a higher-order call graph for functions in a target module (ideally after aggressive inlining has taken place). For each pointer-based data structure taken as input to a function, we will generically derive a “shape” for that structure, and analyze the input call graph to determine the greatest shape which must be evaluated by the function (i.e. the prefix of the data structure which is demanded in all executions). This analysis will likely be based on the application of type-state analysis to a state-space inhabited by the lattice of (approximated) data structure shapes. If there is time for us to do so, we plan to heuristically infer loop invariants for (mutually) recursive functions, and also allow user annotations to provide checkable loop invariants when the optimization cannot infer them. According to the results of the analysis, we will generate modified Core functions that partially normalize their arguments on entry—and forward these modified Core terms to the next stage in the compilation pipeline. We should ensure that our analysis fires before the traditional strictness analysis so that it can take advantage of our output.

People to consult: Simon Peyton Jones, Joachim Breitner, José Manual Trilla, Antal Spector-Zabusky

Papers to read:

Theory and Practice of Demand Analysis in Haskell - https://www.microsoft.com/en-us/research/wp-content/uploads/2017/03/demand-jfp-draft.pdf

Demand Analysis - https://www.microsoft.com/en-us/research/wp-content/uploads/2006/07/demand-1.pdf

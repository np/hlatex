import qualified Language.LaTeX.Builder as B
import Language.LaTeX.Builder (section, subsection, paragraph)

latex = B._LaTeX
hlatex = B.textsc "HLaTeX" -- TODO make a proper symol and provide it in L.L.Builder
haskell = B.textsc "Haskell"
ast = B.textsc "AST"
math = «mathematics»
mathmode = «math-mode»
ltxcode = B.texttt . B.protect
ltxenv = B.texttt . B.protect

p = tell . B.para
em = B.emph

doc = do
  section «Introduction»

  p «{hlatex} is a library to generate {latex} documents using the {haskell} programming language.
     This library focus on providing a safe set of functions and constants that will built a kind of
     {ast} (abstract syntax tree). This {ast} could be subsequently checked and pretty-printed into
     a final {latex} document, ready to feed the {latex} processor.»

  p «The main {latex} advantage is enable nice typesetting of documents without giving up on a human
     readable/writable text format. However the success of {latex} is greatly due to its
     extensibility. By being a programming language, various extensions for all kind of needs
     appeared as packages. These packages provide highlevel typesetting facilities but are
     themself just using a more primitive set of typesetting facilities, that is they don't extend
     the internals of the system.»

  p «However as a programming language {latex} has a lot to envy to other languages. {tex} ---the
     underlying system of {latex}--- has a programming language, is a system based on macro
     expansion, lexcial conventions are moving (dependent of the evaluation), there is no support
     for data structures, a poor support for control structuros, and no type system.»

  p «Hopefully from the point of view of a {latex} generator like {hlatex}, there is no need
     for these programming features. Indeed {hlatex} focus on producing {latex} documents without
     macros definitions. Since {haskell} functions superseed {latex} macros, they could be used
     in replacement, the produced {latex} document will as if the expansion of these new macros
     was already done.»

  paragraph «Safety»

  p «By imposing this restriction ({latex} without macros), {hlatex} turns the hard problem of
     generating correct {latex} into a reasonable one. Indeed safety is one of the main goal
     of this library. Providing safety on the generator side has two big advantages. One of them
     is to avoid the need of looking at the generated {latex}, indeed if errors are not reported
     on the generator but on the generatad {latex} when processing it, one have to look at the
     generatad {latex} and infer where is the error in the generator.»

  p «In {hlatex} errors are classified in two categories. One category of errors are caught by
     the {haskell} type system like the usage of {math} symbols only in the {mathmode}, or the
     distinctions of inline commands like {ltxcode "\\textbf"} or {ltxcode "\\hspace»"}, block
     commands like {ltxenv "itemize"}, dimensions like constants in different units, or
     commands like {ltxcode "\\textwidth"}, and others distinctions made using types. All others
     detected errors are caught dynamically before generating the {latex} document, this emcompass
     for example veriications of the numbers of columns in the {ltxenv "tabular"} or
     {ltxenv "array"} environment, and a lot more as the library is being improved.»

  -- TODO cross-references and citations, math mode nestings like a_{b}_{c}...


  section «Design principles»

  subsection «Naming conventions»

  p «{hlatex} tries to stay close to the {latex} concepts and command names. This decision should
     help to find right function given a {latex} name, however this is not always possible since
     some commands are not valid {haskell} identifiers. {latex} names starting with a capital
     letter will have a leading underscore character, for example {ltxcode "\\Rightarrow"}
     is called {hcode "_Rightarrow"}. More precisely since {hlatex} is a {haskell} library, function
     names are not floating around but are packaged in modules, then the complete name of
     {ltxcode "\\Rightarrow"} is {hcode "Language.LaTeX.Builder.Math._Rightarrow"}, of course
     no one wants to use it fully qualified. Instead one commonly use {haskell}
     {em «qualified imports.»}, importing {hcode "Language.LaTeX.Builder.Math"} as simply
     {hcode "M"}, leading to {hcode "M._Rightarrow"} in a {hlatex} document. Likewise the
     {hcode "Language.LaTeX.Builder"} module is commonly imported as {hcode "B"}.»

  p «However some {latex} commands are far from being valid identifiers like {math} spacing
     commands ({ltxcode "\\!"}, {ltxcode "\\,"}, or {ltxcode "\\;"}) or accents in text mode
     ({ltxcode "\\'"}, {ltxcode "\\`"}, or {ltxcode "\\."}). In these cases, plain names
     have be chosen like {hcode "M.negthinspace"} or {hcode "B.acute"}. These names are
     chosen according there semantics and not according their symbol, so we use `acute'
     instead of `single quote' and `thinspace' instead of `comma'.»
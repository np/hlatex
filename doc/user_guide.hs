{-# LANGUAGE QuasiQuotes, OverloadedStrings, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -F -pgmF pphlatex #-}
import Language.LaTeX
import qualified Language.LaTeX.Builder as B
import qualified Language.LaTeX.Builder.Color as C
import qualified Language.LaTeX.Builder.Math as M
import Language.LaTeX.Builder.QQ hiding (tex)
import Control.Monad.Writer

latex = B._LaTeX
tex = B._TeX
hlatex = B.textsc "HLaTeX" -- TODO make a proper symol and provide it in L.L.Builder
haskell = B.textsc "Haskell"
ast = B.textsc "AST"
ghc = B.textsc "GHC"
math = «mathematics»
mathmode = «math-mode»
frquotes = B.texttt "frquotes"
ltxcode = B.texttt . B.protect
hcode = B.texttt . B.protect
ltxenv = B.texttt . B.protect

put :: ParItem -> ParItemW
put = tell

p = put . B.para
section = put . B.section
subsection = put . B.subsection
paragraph = put . B.paragraph
em = B.emph

todo = p . C.textcolor C.red

main = quickView testViewOpts "user_guide" root

root = B.root preamb body

preamb = B.documentclass (Just (B.pt 11)) (Just B.a4paper) B.book
     -- <> B.usepackage [B.optional "francais"] (B.pkgName "babel")

body = B.document $? do
  put B.tableofcontents
  doc

doc :: ParItemW
doc = do
  section «Introduction»

  p «{hlatex} is a library to generate {latex} documents using the {haskell} programming language.
     This library focus on providing a safe set of functions and constants that will build a kind of
     {ast} (abstract syntax tree). This {ast} could be subsequently checked and pretty-printed into
     a final {latex} document, ready to feed the {latex} processor.»

  p «The main {latex} advantage is to enable nice typesetting of documents without giving up on a
     human readable/writable text format. However the success of {latex} is greatly due to its
     extensibility. By being a programming language, various extensions for all kind of needs
     appeared as packages. These packages provide highlevel typesetting facilities but are
     themself just using a more primitive set of typesetting facilities, and so do not extend
     the internals of the system.»

  p «However as a programming language {latex} has a lot to envy to other languages. {tex} ---the
     underlying system of {latex}--- as a programming language, is a system based on macro
     expansion, lexical conventions are changing (dependent of the evaluation), there is no support
     for data structures, a poor support for control structuros, and no type system.»

  p «Hopefully from the point of view of a {latex} generator like {hlatex}, there is no need
     for these programming features. Indeed {hlatex} focus on producing {latex} documents without
     macros definitions. Since {haskell} functions superseed {latex} macros, they could be used
     in replacement, the produced {latex} document will be as if the expansion of these new macros
     was already done.»

  paragraph «Safety»

  p «By imposing this restriction ({latex} without macros), {hlatex} turns the hard problem of
     generating correct {latex} into a reasonable one. Indeed safety is one of the main goal
     of this library. Providing safety on the generator side has two big advantages. One of them
     is to avoid the need of looking at the generated {latex}, indeed if errors are not reported
     on the generator but on the generatad {latex}, one would have to look at the
     generatad {latex} and infer where is the error in the generator.»

  p «In {hlatex}, errors are classified in two categories. One category of errors are caught by
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
     help to find the right function given a {latex} name, however this is not always possible since
     some commands are not valid {haskell} identifiers. {latex} names starting with a capital
     letter will have a leading underscore character, for example {ltxcode "\\Rightarrow"}
     is called {hcode "_Rightarrow"}. More precisely since {hlatex} is a {haskell} library, function
     names are not floating around but are packaged in modules, then the complete name of
     {ltxcode "\\Rightarrow"} is {hcode "Language.LaTeX.Builder.Math._Rightarrow"}, of course
     no one wants to use it fully qualified. Instead one commonly use {haskell}
     {em «qualified imports»}, importing {hcode "Language.LaTeX.Builder.Math"} as simply
     {hcode "M"}, leading to {hcode "M._Rightarrow"} in a {hlatex} document. Likewise the
     {hcode "Language.LaTeX.Builder"} module is commonly imported as {hcode "B"}.»

  p «However some {latex} commands are far from being valid identifiers like {math} spacing
     commands ({ltxcode "\\!"}, {ltxcode "\\,"}, or {ltxcode "\\;"}) or accents in text mode
     ({ltxcode "\\'"}, {ltxcode "\\`"}, or {ltxcode "\\."}). In these cases, plain names
     have be chosen like {hcode "M.negthinspace"} or {hcode "B.acute"}. These names are
     chosen according there semantics and not according their symbol, so we use `acute'
     instead of `single quote' and `thinspace' instead of `comma'.»

  subsection «Commands with optional arguments»

  p «Since {haskell} does not support optional arguments, and that we do not want to add extra
     burden on the document writer we have choosen to expose two variants of some commands.
     Therefore {ltxcode "\\newline"} is exposed as {hcode "B.newline"} and to give the optional
     size argument one use the {hcode "B.newline'"} function as in {hcode "B.newline' (B.pt 42)"}.
     Another example is sectionning. Each sectionning command (like {ltxcode "\\chapter"} or
     {ltxcode "\\section"}) have an unnumbered version (like {ltxcode "\\chapter*"} or
     {ltxcode "\\section*"}), and can also take an optional argument that is the title that
     will appear in table of contents.  So the {hcode "B.chapter"} function only take the chapter
     title whereas the {hcode "B.chapter'"} function takes three arguments the first select the
     normal or the starred version, the second is the table of contents version as a
     {hcode "Maybe"} type and the third is the actual title.»

  section «Monoidal structure»

  subsection «FIXME» -- rappel

  p «A monoid is a very simple algebraic structure. To be a monoid one needs a set---that
     will be represented with a {haskell} type---; an associative composition operator---called
     {hcode "mappend"} in {haskell} and abbreviated by the {hcode "<>"} infix operator here;
     and a neutral element---called {hcode "mempty"} and abbreviated using unicode empty set symbol
     {B.math M.emptyset}. The neutral element is a left and right unit for the composition operator.»

  p «In computer systems a lot of structures are monoid instances. Like lists with the empty list
     and concatenation, integers with zero and addition, functions with identity and composition
     and a lot more.»

  p «The point of using this kind of abstraction instead of their concrete representations is to
     share generic functions over them. For instance composing a list of values can be done with
     the {hcode "mconcat"} function, another variant is included in this library called
     {hcode "mconcatMap"}.» -- also add mapNonEmpty when I would have found another name.

{-
  p «»
  p «»
  p «»
  p «»
  p «»
-}

  section «Lexical extensions and preprocessing»

  let pragmaOpts = "{-# OPTIONS_GHC -F -pgmF pphlatex #-}"
      pragmaLang = "{-# LANGUAGE QuasiQuotes #-}"
  subsection «Activating the extension»

  p «Using the following {ghc} pragma {hcode pragmaOpts}, and since this extension relies on
     quasi-quoting one also needs this pragma {hcode pragmaLang}.»

  subsection «French Quotes Extension»
  -- TODO
  p «{frquotes} is a lexical syntax extension enabling lightweight interpolated  
      literal piece of text. Once desugared this extension relies on the  
      quasi-quoting system provided by {ghc}.»

  subsection «Mempty Extension»
  -- TODO

  section «Injecting strings»
  subsection «The IsString class»
  subsection «The hstring function»
      -- maybe useless to say
  todo «note that hstring "" reduces to mempty»

  section «Sectioning»

  section «List like environment»
  todo "how to put a paritem between items"

  section «Declarations»
  todo «decls are side effects and {"{}"} are scopes
    in hlatex all declarations are explicitely scoped (effects don't   pass through <>)
    unsafe injection is still available though»

  section «Tabulars»
  todo «
     why lists
     format spec
     how put hline
     missing features
     »

  section «Paragraphs»
  todo «why strong paragraph distintinction
     no longer double endline meaning
     don't confuse para and paragraph
     »

  section «Math mode»
  subsection «No plain strings»
  subsection «No plain chars»
  todo "=> (M.a, M.b...), in fact yes as unicode support"
  todo "still low level (sum has only one arg)"
  todo "still the same meaning for <> Ligatures"
  todo "num class"

  section «Verbatim mode»
  subsection «Why not using the {latex} verbatim mode?»
  subsection «The protect function»
  todo «special characters
     Ligatures
     Work with fixed width fonts»
  todo "verb, M.verb, UTF8..."

  section «Figures»

  section «Bibliography, glossary and index»

  section «Making new commands»
  todo "L.L.Builder.Internal"

  section «Injecting Raw LaTeX»

  section «Pictures»

  section «Size units and pre-defineds dimensions»
  todo "normalisation, symbolic eval, num class"

  section «The LatexM monad»
  subsection «What for?»
  subsection «How hidden is it?»
  todo «
     a monad transformer would allow
       custom initial monad, more state,
       more control...
    »

  section «Using the Writer monad»

  section «Misc»
  subsection «Save bins»

  section «Packages»
  subsection «Automatic tracking»

  subsection «graphics»
  subsection «Listings equivalent»

  subsection «Adding new packages»

-- end
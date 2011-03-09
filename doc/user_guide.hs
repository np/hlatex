{-# LANGUAGE QuasiQuotes, OverloadedStrings, NoMonomorphismRestriction, UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -F -pgmF frquotes #-}
import Language.LaTeX
import qualified Language.LaTeX.Length as L
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
ascii = B.textsc "ASCII"

put :: ParItem -> ParItemW
put = tell

p = put . B.para
section = put . B.section
subsection = put . B.subsection
paragraph = put . B.paragraph
em = B.emph

todo = p . C.textcolor C.red

main = quickView myViewOpts { basedir = "examples"
                            , pdflatex = "pdflatex"
                            , pdfviewer = "evince" }
                 "user_guide" doc

doc = B.document (B.book (Just (L.pt 11)) (Just B.a4paper) []) preamb body

preamb = ø
     -- ⊕ B.usepackage [B.optional "francais"] (B.pkgName "babel")

body = execWriter $ do
  put B.tableofcontents

  section «Introduction»

  p «{hlatex} is a library to generate {latex} documents using the {haskell}
    programming language. This library focus on providing a safe set of functions
    and constants that will build a kind of {ast} (abstract syntax tree). This {ast}
    could be subsequently checked and pretty-printed into a final {latex} document,
    ready to feed the {latex} processor.»

  p «The main {latex} advantage is to enable nice typesetting of documents
    without giving up on a human readable/writable text format. However the success
    of {latex} is greatly due to its extensibility. By being a programming language,
    various extensions for all kind of needs appeared as packages. These packages
    provide highlevel typesetting facilities but are themself just using a more
    primitive set of typesetting facilities, and so do not extend the internals of
    the system.»

  p «However as a programming language {latex} has a lot to envy to other
    languages. {tex} ---the underlying system of {latex}--- as a programming
    language, is a system based on macro expansion, lexical conventions are changing
    (dependent of the evaluation), there is no support for data structures, a poor
    support for control structuros, and no type system.»

  p «Hopefully from the point of view of a {latex} generator like {hlatex}, there
    is no need for these programming features. Indeed {hlatex} focus on producing
    {latex} documents without macros definitions. Since {haskell} functions
    superseed {latex} macros, they could be used in replacement, the produced
    {latex} document will be as if the expansion of these new macros was already
    done.»

  paragraph «Safety»

  p «By imposing this restriction ({latex} without macros), {hlatex} turns the
    hard problem of generating correct {latex} into a reasonable one. Indeed safety
    is one of the main goal of this library. Providing safety on the generator side
    has two big advantages. One of them is to avoid the need of looking at the
    generated {latex}, indeed if errors are not reported on the generator but on the
    generatad {latex}, one would have to look at the generatad {latex} and infer
    where is the error in the generator.»

  p «In {hlatex}, errors are classified in two categories. One category of errors
    are caught by the {haskell} type system like the usage of {math} symbols only in
    the {mathmode}, or the distinctions of inline commands like {ltxcode "\\textbf"}
    or {ltxcode "\\hspace»"}, block commands like {ltxenv "itemize"}, dimensions
    like constants in different units, or commands like {ltxcode "\\textwidth"}, and
    others distinctions made using types. All others detected errors are caught
    dynamically before generating the {latex} document, this emcompass for example
    veriications of the numbers of columns in the {ltxenv "tabular"} or
    {ltxenv "array"} environment, and a lot more as the library is being improved.»
  -- TODO cross-references and citations, math mode nestings like a_{b}_{c}...

  section «Design principles»

  subsection «Naming conventions»

  p «{hlatex} tries to stay close to the {latex} concepts and command names. This
    decision should help to find the right function given a {latex} name, however
    this is not always possible since some commands are not valid {haskell}
    identifiers. {latex} names starting with a capital letter will have a leading
    underscore character, for example {ltxcode "\\Rightarrow"} is called
    {hcode "_Rightarrow"}. More precisely since {hlatex} is a {haskell} library,
    function names are not floating around but are packaged in modules, then
    the complete name of {ltxcode "\\Rightarrow"} is
    {hcode "Language.LaTeX.Builder.Math._Rightarrow"}, of course no one wants to
    use it fully qualified. Instead one commonly use {haskell}
    {em «qualified imports»}, importing {hcode "Language.LaTeX.Builder.Math"} as
    simply {hcode "M"}, leading to {hcode "M._Rightarrow"} in a {hlatex} document.
    Likewise the {hcode "Language.LaTeX.Builder"} module is commonly imported as
    {hcode "B"}.»

  p «However some {latex} commands are far from being valid identifiers like
    {math} spacing commands ({ltxcode "\\!"}, {ltxcode "\\,"}, or {ltxcode "\\;"})
    or accents in text mode ({ltxcode "\\'"}, {ltxcode "\\`"}, or {ltxcode "\\."}).
    In these cases, plain names have be chosen like {hcode "M.negthinspace"} or
    {hcode "B.acute"}. These names are chosen according there semantics and not
    according their symbol, so we use `acute' instead of `single quote' and
    `thinspace' instead of `comma'.»

  subsection «Commands with optional arguments»

  p «Since {haskell} does not support optional arguments, and that we do not want
    to add extra burden on the document writer we have choosen to expose two
    variants of some commands. Therefore {ltxcode "\\newline"} is exposed as
    {hcode "B.newline"} and to give the optional size argument one use the
    {hcode "B.newline'"} function as in {hcode "B.newline' (B.pt 42)"}. Another
    example is sectionning. Each sectionning command (like {ltxcode "\\chapter"} or
    {ltxcode "\\section"}) have an unnumbered version (like {ltxcode "\\chapter*"} or
    {ltxcode "\\section*"}), and can also take an optional argument that is the
    title that will appear in table of contents. So the {hcode "B.chapter"} function
    only take the chapter title whereas the {hcode "B.chapter'"} function takes
    three arguments the first select the normal or the starred version, the second
    is the table of contents version as a {hcode "Maybe"} type and the third is the
    actual title.»

  section «Monoidal structure»

  subsection «FIXME» -- rappel

  p «A monoid is a very simple algebraic structure. To be a monoid one needs a
    set---that will be represented with a {haskell} type---; an associative
    composition operator---called {hcode "mappend"} in {haskell} and abbreviated by
    the {hcode "⊕"} infix operator here; and a neutral element---called
    {hcode "mempty"} and abbreviated using unicode empty set symbol {{-TODO use ø-}B.math M.emptyset}.
    The neutral element is a left and right unit for the composition operator.»

  p «In computer systems a lot of structures are monoid instances. Like lists
    with the empty list and concatenation, integers with zero and addition,
    functions with identity and composition and a lot more.»

  p «The point of using this kind of abstraction instead of their concrete
    representations is to share generic functions over them. For instance composing
    a list of values can be done with the {hcode "mconcat"} function, another
    variants are included in the {hcode "Data.Foldable"} module like the
    {hcode "fold"} and {hcode "foldMap"} functions.»
    -- also add mapNonEmpty when I would have found another name.

{-
  p «»
  p «»
  p «»
  p «»
  p «»
-}

  section «Injecting strings»

  p «While the main job of {hlatex} is to give access to {latex} commands in
    {haskell}, generating documents using {haskell} needs also to cope with
    80% of the contents, namely the text.»

  p «Plain text can be converted to {latex} using the {hcode "hstring"} function.»

  subsection «The {hcode "hstring"} function»

  p «This function convert an {haskell} string to a piece of {latex}. While the
    semantics of this function is obvious for characters like `a' or `X', what
    the result for blanks, special characters and unicode characters.»

{-
  p «The semantics is clear, {hcode "hstring"} converts each character to a
    {}»
  -}

  todo «note that {hcode "hstring \"\""} reduces to {hcode "ø"}»

  subsection «The IsString class»

  section «Lexical extensions and preprocessing»

  subsection «Activating the extensions»

  let pragmaOpts = "{-# OPTIONS_GHC -F -pgmF frquotes #-}"
      pragmaLang = "{-# LANGUAGE QuasiQuotes, UnicodeSyntax #-}"

  p «Using the following {ghc} pragma {hcode pragmaOpts}, and since this
    extension relies on quasi-quoting one also needs this pragma
    {hcode pragmaLang}.»

  subsection «French Quotes Extension»

  p «{frquotes} is a lexical syntax extension enabling lightweight interpolated  
    literal piece of text. Once desugared this extension relies on the  
    quasi-quoting system provided by {ghc}.»

  let (frO, frC, brO, brC) = ("«", "»", "{", "}")
  p «Currently the French Quotes extension only desugar to litteral strings
    and monoid compositions. French Quotes are introduced using `{frO}' and
    closed using `{frC}'. Interpolation (injection of {haskell} code inside the
    quotes) is done using braces (`{brO}' and `{brC}'). These interpolated holes
    can contains an {haskell} expression including nested French Quotes,
    string, comments, braces, etc. The French Quotes characters can be used
    inside French Quotes but must be balanced.»

  subsection «Unicode Syntax Extension»

  p «This syntax extension allows to use `ø' and `⊕' in {haskell}
     code instead {hcode "mempty"} and {hcode "`mappend`"} respectively.
     For example one can write {hcode "ø ⊕ x ⊕ ø"} instead of
     {hcode "mempty `mappend` x `mappend` mempty"}.»
  -- TODO

  section «Sectioning»

  p «In {latex} sectioning is done by inserting separators commands. In {hlatex}
    all these commands are exported in a straightforward way. Most of these commands
    appears in two variants, for instance {ltxcode "\\section"} can be produced
    by {hcode "section"} and just waits for a {hcode "LatexItem"} as title,
    and {hcode "section'"} waits for two extra arguments the first is either
    {hcode "ø"} ({hcode "mempty"} in {ascii}) or {hcode "(★)"} ({hcode "star"} in {ascii})
    and the second is optional and represents the text to be used in the table of contents.»

  section «List like environment»
  todo "how to put a paritem between items"

  section «Declarations»

  p «In {latex} declarations are side effects.  For instance {ltxcode "\\small"}
    changes the current size of text to make it smaller, it is an effect on
    the size.  However the scope of declarations can be delimited using braces
    ({"{"} and {"}"}).»

-- "toto " ⊕ "\small" ⊕ " tata"
-- toto ⊕ {\small ⊕ tata} ⊕ tutu

  p «In {hlatex} all declarations are explicitely scoped.
    In other words the effects of declarations do not pass through concatenation
    ({hcode "⊕"}) of {latex} pieces.
    They are represented in {hlatex} with types {hcode "TexDecl"} and
    {hcode "MathDecl"} for the math mode.
    The unsafe injection from declarations to pieces of {latex} is still
    available though.»

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



  -- p (math "1" ⊕ "+" ⊕ "2")

  {-
  (&) fct arg = fct ⊕ M.parens arg 
  f arg = M.f & arg

  f M.x
  M.«1 + f({arg}) + Σ^i_(0..n)»
  -}

  subsection «No plain chars»
  todo "=> (M.a, M.b...), in fact yes as unicode support"
  todo "still low level (sum has only one arg)"
  todo "still the same meaning for ⊕ Ligatures"
  todo "num class"

  section «Verbatim mode»

  subsection «Why not using the {latex} verbatim mode?»

  p «{latex} verbatim mode is a hack.  It is a kind of global side-effect that
    changes the meaning of letters to turn off their original meaning and
    produce fixed width characters.  In result the verbatim mode interacts badly
    with other fancy environments.»

  p «In {haskell} this task is much simpler, indeed we do have literal strings.
    There is no need to change some existing meaning.  We just need a function
    from {hcode "String"} to {hcode "LatexItem"}.»
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

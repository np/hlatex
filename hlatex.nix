{ mkDerivation, base, base-unicode-symbols, containers, directory
, filepath, mtl, process, stdenv, template-haskell, transformers
, uniplate, utf8-string
}:
mkDerivation {
  pname = "hlatex";
  version = "0.3.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base-unicode-symbols containers directory filepath mtl process
    template-haskell transformers uniplate utf8-string
  ];
  description = "A library to build valid LaTeX files";
  license = stdenv.lib.licenses.bsd3;
}

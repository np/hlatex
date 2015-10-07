{ mkDerivation, base, base-unicode-symbols, containers, derive
, directory, filepath, frquotes, mtl, process, stdenv
, template-haskell, transformers, uniplate, utf8-string
}:
mkDerivation {
  pname = "hlatex";
  version = "0.3.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base-unicode-symbols containers derive directory filepath mtl
    process template-haskell transformers uniplate utf8-string
  ];
  executableHaskellDepends = [
    base base-unicode-symbols containers frquotes mtl transformers
  ];
  description = "A library to build valid LaTeX files";
  license = stdenv.lib.licenses.bsd3;
}

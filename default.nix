{ mkDerivation, array, async, base, bytestring, containers, deepseq
, ghc-prim, hashable, mtl, mtl-compat, stdenv, stm, text
, transformers, transformers-compat
}:
mkDerivation {
  pname = "protolude";
  version = "0.3.0";
  src = ./protolude;
  libraryHaskellDepends = [
    array async base bytestring containers deepseq ghc-prim hashable
    mtl mtl-compat stm text transformers transformers-compat
  ];
  homepage = "https://github.com/sdiehl/protolude";
  description = "A small prelude";
  license = stdenv.lib.licenses.mit;
}

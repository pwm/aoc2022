{ mkDerivation, base, containers, criterion, data-default-class
, fetchgit, heredoc, hspec, lib, optparse-applicative
, template-haskell, th-compat, th-lift, th-lift-instances
}:
mkDerivation {
  pname = "advent-of-code-ocr";
  version = "0.1.2.0";
  src = fetchgit {
    url = "https://github.com/pwm/advent-of-code-ocr";
    sha256 = "0lw5cqj5gpqkgsnvgi19vz12x90vzqc9dwkclf9vfp3yqwmn5139";
    rev = "9595bb86ecae5909ad17d6017d2d9d954bb07d5a";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers data-default-class heredoc template-haskell
    th-compat th-lift th-lift-instances
  ];
  executableHaskellDepends = [
    base containers data-default-class heredoc optparse-applicative
    template-haskell th-compat th-lift th-lift-instances
  ];
  testHaskellDepends = [
    base containers data-default-class heredoc hspec template-haskell
    th-compat th-lift th-lift-instances
  ];
  benchmarkHaskellDepends = [
    base containers criterion data-default-class heredoc
    template-haskell th-compat th-lift th-lift-instances
  ];
  homepage = "https://github.com/mstksg/advent-of-code-ocr#readme";
  description = "Parse Advent of Code ASCII art letters";
  license = lib.licenses.bsd3;
  mainProgram = "advent-of-code-ocr";
}

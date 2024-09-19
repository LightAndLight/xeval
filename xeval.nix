{ mkDerivation, base, bytestring, lib, mtl, optparse-applicative
, time, utf8-string, X11
}:
mkDerivation {
  pname = "xeval";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring mtl optparse-applicative time utf8-string X11
  ];
  license = lib.licenses.mit;
  mainProgram = "xeval";
}

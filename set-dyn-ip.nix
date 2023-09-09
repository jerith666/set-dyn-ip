{ mkDerivation, fetchFromGitHub, amazonka, amazonka-core, amazonka-route53, base
, lens, network, stdenv, lib
}:
mkDerivation {
  pname = "set-dyn-ip";
  version = "0.1.1.3";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    amazonka amazonka-core amazonka-route53 base lens network
  ];
  homepage = "http://matt.mchenryfamily.org";
  description = "reads an ip address as generated by client-ip-echo and sets it in Amazon AWS Route53";
  license = lib.licenses.lgpl3;
  maintainers = [ stdenv.lib.maintainers.jerith666 ];
}
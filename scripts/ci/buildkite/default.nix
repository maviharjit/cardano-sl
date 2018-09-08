let
  localLib = import ../../../lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, pkgs ? (import (localLib.fetchNixPkgs) { })
, buildTools ? with pkgs; [ git nix ]
}:

with pkgs.lib;
with pkgs;

let
  iohkPkgs = import ../../.. { inherit config system pkgs; };

  cache-s3 = stdenv.mkDerivation rec {
    name = "cache-s3-${version}";
    version = "v0.1.6";
    src = fetchurl {
      url = "https://github.com/fpco/cache-s3/releases/download/${version}/cache-s3-${version}-linux-x86_64.tar.gz";
      sha256 = "01qm6mg11g6kq3sfnnj1civmda35mbfmp1fym5yvqwbdsmqd0b19";
    };
    libPath = stdenv.lib.makeLibraryPath [
      stdenv.cc.cc.lib
      zlib
      gmp
    ];
    sourceRoot = ".";
    buildPhase = "true";
    installPhase = ''
      mkdir -p $out/bin
      install -m 0755 cache-s3 $out/bin
    '';
    postFixup = ''
      patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" $out/bin/cache-s3
      patchelf --set-rpath ${libPath} $out/bin/cache-s3
    '';
  };

  stackRebuild = runCommand "stack-rebuild" {} ''
    ${iohkPkgs.ghc.withPackages (ps: [ps.turtle ps.safe ps.transformers])}/bin/ghc -o $out ${./rebuild.hs}
  '';

in
  writeScript "stack-rebuild-wrapped" ''
    #!${stdenv.shell}
    export PATH=${lib.makeBinPath ([ cache-s3 stack coreutils ] ++ buildTools)}
    exec ${stackRebuild} "$@"
  ''

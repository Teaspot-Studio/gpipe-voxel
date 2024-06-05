{ pkgs ? import ./nix/pkgs.nix {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskell.compiler.ghc982
    pkgs.haskellPackages.cabal-install
    pkgs.haskellPackages.haskell-language-server #optional
    pkgs.glfw
    pkgs.xorg.libXcursor
    pkgs.xorg.libXrandr
    pkgs.xorg.libXi
    pkgs.xorg.libX11
    pkgs.xorg.libXxf86vm
    pkgs.xorg.libXinerama

  ];
}

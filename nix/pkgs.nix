# To update nix-prefetch-git https://github.com/NixOS/nixpkgs
import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs";
  rev = "7ed6b76fa036548b59621af20df5bc46b9b430e8";
  sha256  = "0qy2lg7m0q9hpc45y6ygw58f4s50v1qyh6g6q5v1zdcdzjnfp12r";
})
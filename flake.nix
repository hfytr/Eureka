{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  outputs = { self, nixpkgs, ... }: let
    system = "x86_64-linux";
  in {
    devShells."${system}".default = let
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
    in pkgs.mkShell {
      packages = with pkgs; [
        ghc
        cabal-install
        haskell-language-server
        haskellPackages.hlint
        haskellPackages.fourmolu
        haskellPackages.hoogle
      ];
    };
  };
}

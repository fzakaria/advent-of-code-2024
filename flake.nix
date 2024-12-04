{
  description = "Advent of code 2024";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
  outputs = {
    self,
    nixpkgs,
  }: let
    supportedSystems = ["x86_64-linux"];
    forAllSystems = f:
      nixpkgs.lib.genAttrs supportedSystems (system: f system);
    nixpkgsFor = forAllSystems (system:
      import nixpkgs {
        inherit system;
        overlays = [self.overlay];
      });
  in {
    overlay = final: prev: {};
    packages = forAllSystems (system: {});
    devShell = forAllSystems (system: let
      haskellPackages = nixpkgsFor.${system}.haskellPackages;
    in
      haskellPackages.shellFor {
        packages = p: [
        ];
        withHoogle = true;
        buildInputs = with haskellPackages; [
          haskell-language-server
          cabal-install
          ormolu
          hlint
        ];
      });
  };
}

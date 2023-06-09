{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  outputs = {
    self,
    nixpkgs,
  }: {
    devShells.x86_64-linux.default = with import nixpkgs {
      system = "x86_64-linux";
    };
      mkShell {
        buildInputs = [
          (pkgs.haskellPackages.ghcWithPackages (self: [self.random]))
          ormolu
          gnumake
        ];

        shellHook = ''
          zsh
          exit'';
      };
  };
}

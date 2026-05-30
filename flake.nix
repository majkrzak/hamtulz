{
  description = "hamtulz - Amateur Radio Utilities";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        ghc = pkgs.ghc;

        systemDeps = with pkgs; [
          libyaml
        ];

        hamtulz = pkgs.haskell.lib.buildStackProject {
          name = "hamtulz";
          inherit ghc;
          buildInputs = systemDeps;
          src = ./.;
        };

      in {
        packages.default = hamtulz;

        apps = {
          default = {
            type = "app";
            program = "${hamtulz}/bin/wsjtx2log";
          };
          wsjtx2log = {
            type = "app";
            program = "${hamtulz}/bin/wsjtx2log";
          };
          log2adif = {
            type = "app";
            program = "${hamtulz}/bin/log2adif";
          };
          check_log = {
            type = "app";
            program = "${hamtulz}/bin/check_log";
          };
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [ hamtulz ];
          buildInputs = with pkgs; [
            stack
            cabal-install
            ghc
            haskell-language-server
          ];
        };
      });
}

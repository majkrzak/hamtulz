{ ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      devShells.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          cargo
          rustc
          rustfmt
          clippy
        ];
      };
    };
}

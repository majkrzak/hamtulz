{ inputs, ... }:
{
  perSystem =
    { pkgs, ... }:
    let
      craneLib = inputs.crane.mkLib pkgs;
    in
    {
      packages.default = craneLib.buildPackage {
        pname = "hamtulz";
        version = "git";
        src = craneLib.cleanCargoSource ../.;
      };
    };
}

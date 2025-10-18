{
  description = "Flake environment for Arrowized FRP with Yampa";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, ... }: let
    forAllSystems = nixpkgs.lib.genAttrs [
      "aarch64-linux"
      "x86_64-linux"
    ];
  in rec {
    devShells = forAllSystems(system:
      let pkgs = import nixpkgs {
          inherit system;
        };
      in {
        default = pkgs.mkShell {
          packages = with pkgs; [ texliveFull texlivePackages.standalone ];
        };
      }
    );
  };
}

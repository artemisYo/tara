{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
  in {

    packages.${system} = {
      tara = pkgs.rustPlatform.buildRustPackage {
        pname = "tara";
        version = "0.0.1";
        src = ./.;
        cargoLock.lockFile = ./Cargo.lock;
      };
      default = self.packages.${system}.tara;
    };

    # packages.x86_64-linux.default = self.packages.x86_64-linux.hello;

  };
}

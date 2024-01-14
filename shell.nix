let
    fenix_rust = import "${fetchTarball "https://github.com/nix-community/fenix/archive/main.tar.gz"}/overlay.nix";
    pkgs = import <nixpkgs> { overlays = [ fenix_rust ]; };
in
pkgs.mkShell {
    nativeBuildInputs = with pkgs; [
        (fenix.stable.withComponents [
            "cargo"
            "clippy"
            "rust-src"
            "rustc"
            "rustfmt"
            "rust-analyzer"
        ])
        typst
    ];
}

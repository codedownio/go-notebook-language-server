{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix/9c5956641f45b6b02607e318485aad01c18e65b0";
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-25.11";

  outputs = { self, flake-utils, gitignore, haskellNix, nixpkgs }:
    flake-utils.lib.eachSystem ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"] (system:
      let
        compiler-nix-name = "ghc9122";

        overlays = [
          haskellNix.overlay

          # Set enableNativeBignum flag on compiler
          (import ./nix/overlays/native-bignum.nix { inherit compiler-nix-name; })

          # Configure hixProject
          (import ./nix/overlays/hix-project.nix { inherit compiler-nix-name gitignore system; })
        ];

        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };

        flake = (pkgs.hixProject compiler-nix-name).flake {};
        flakeStatic = (pkgs.pkgsCross.musl64.hixProject compiler-nix-name).flake {};

        # Build go-parser from source (stdlib only, no vendorHash needed)
        go-parser = pkgs.buildGoModule {
          pname = "go-parser";
          version = "0.1.0";
          src = ./go-parser;
          vendorHash = null;
          meta = with pkgs.lib; {
            description = "Go notebook code parser using go/scanner";
            license = licenses.bsd3;
            platforms = platforms.unix;
            mainProgram = "go-parser";
          };
        };

        mkWrapped = gnls: pkgs.runCommand "go-notebook-language-server-${gnls.version}-wrapped" {
          nativeBuildInputs = [ pkgs.makeWrapper ];
        } ''
          mkdir -p $out/bin
          makeWrapper ${gnls}/bin/go-notebook-language-server $out/bin/go-notebook-language-server \
            --prefix PATH : ${go-parser}/bin
        '';

      in
        {
          devShells = {
            default = pkgs.mkShell {
              NIX_PATH = "nixpkgs=${pkgs.path}";
              buildInputs = with pkgs; [
                haskell.compiler.ghc9122

                pcre
                zlib

                go-parser
              ];
            };
          };

          packages = (rec {
            inherit (pkgs) cabal2nix stack;

            default = static;

            inherit go-parser;

            dynamicWrapped = mkWrapped dynamic;
            staticWrapped = mkWrapped static;

            static = flakeStatic.packages."go-notebook-language-server:exe:go-notebook-language-server";
            dynamic = flake.packages."go-notebook-language-server:exe:go-notebook-language-server";

            nixpkgsPath = pkgs.writeShellScriptBin "nixpkgsPath.sh" "echo -n ${pkgs.path}";
          });

          inherit flake;
        }
    );
}

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
        flakeDarwin = (pkgs.pkgsCross.aarch64-darwin.hixProject compiler-nix-name).flake {};
        flakeAarch64Linux = (pkgs.pkgsCross.aarch64-multiplatform.hixProject compiler-nix-name).flake {};
        flakeStaticAarch64Linux = (pkgs.pkgsCross.aarch64-multiplatform-musl.hixProject compiler-nix-name).flake {};

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

        packageForGitHub' = systemToUse: cnls: pkgs.runCommand "go-notebook-language-server-${cnls.version}-${systemToUse}" {} ''
          name="go-notebook-language-server-${cnls.version}-${systemToUse}"

          mkdir -p to_zip
          cp -r ${cnls}/* to_zip
          mkdir -p $out
          tar -czvf $out/$name.tar.gz -C to_zip .
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
            darwin = flakeDarwin.packages."go-notebook-language-server:exe:go-notebook-language-server";
            aarch64Linux = let
              executable = flakeAarch64Linux.packages."go-notebook-language-server:exe:go-notebook-language-server";
              libs = pkgs.callPackage ./nix/dynamic-aarch64-closure.nix {
                inherit executable;
                executableName = "go-notebook-language-server";
                pkgsCross = pkgs.pkgsCross.aarch64-multiplatform;
              };
            in pkgs.runCommand "go-notebook-language-server-aarch64-dynamic" { passthru = { inherit (executable) version; }; } ''
                 mkdir -p $out/bin
                 cp -r ${executable}/bin/* $out/bin
                 cp -r ${libs}/lib/* $out/bin
               '';
            staticAarch64Linux = flakeStaticAarch64Linux.packages."go-notebook-language-server:exe:go-notebook-language-server";

            # Print a trivial PATH that we can use to run kernel and LSP tests, to ensure
            # they aren't depending on anything on the test machine's PATH.
            print-basic-path = pkgs.writeShellScriptBin "basic-path.sh" ''
              echo ${pkgs.lib.makeBinPath (with pkgs; [coreutils go go-parser])}
            '';

            grandCombinedGithubArtifacts = pkgs.symlinkJoin {
              name = "go-notebook-language-server-grand-combined-artifacts";
              paths = [
                (packageForGitHub' "x86_64-linux" self.packages.x86_64-linux.static)
                (packageForGitHub' "aarch64-linux" aarch64Linux)
                (packageForGitHub' "x86_64-darwin" self.packages.x86_64-darwin.dynamic)
                (packageForGitHub' "aarch64-darwin" self.packages.aarch64-darwin.dynamic)
              ];
            };

            nixpkgsPath = pkgs.writeShellScriptBin "nixpkgsPath.sh" "echo -n ${pkgs.path}";
          });

          inherit flake;
        }
    );
}

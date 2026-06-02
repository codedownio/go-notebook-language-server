{ compiler-nix-name
, gitignore
, system
}:

final: prev: {
  hixProject = compiler-nix-name:
    final.haskell-nix.hix.project {
      src = gitignore.lib.gitignoreSource ../../.;
      evalSystem = system;
      inherit compiler-nix-name;

      modules = [{
        packages.unix.components.library.configureFlags = [''-f os-string''];
        packages.directory.components.library.configureFlags = [''-f os-string''];
        packages.file-io.components.library.configureFlags = [''-f os-string''];

        packages.go-notebook-language-server.components.exes.go-notebook-language-server.dontStrip = false;
      }]
      # On macOS the dynamic build links libpcre/libiconv/libffi from /nix/store
      # paths that won't exist on an end user's machine, so dyld refuses to start
      # the binary. Repoint those at the system libraries / static-link libffi.
      ++ final.lib.optional final.stdenv.hostPlatform.isDarwin (import ../macos-modules.nix { pkgs = final; });
    };
}

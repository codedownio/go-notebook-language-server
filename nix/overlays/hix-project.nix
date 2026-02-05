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
      }];
    };
}

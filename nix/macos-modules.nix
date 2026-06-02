{ pkgs }:

{
  packages.go-notebook-language-server.components.exes.go-notebook-language-server.postInstall = ''
    ${builtins.readFile ./fix-dylib.sh}

    fix_dylib "$out/bin/go-notebook-language-server" libiconv.2.dylib libiconv.dylib
    fix_dylib "$out/bin/go-notebook-language-server" libpcre.1.dylib libpcre.dylib
    check_no_nix_refs "$out/bin/go-notebook-language-server"

    strip "$out/bin/go-notebook-language-server"
  '';

  packages.go-notebook-language-server.components.exes.go-notebook-language-server.configureFlags = let
    # Statically link libffi so the binary doesn't carry a dynamic dependency on a
    # /nix/store libffi.7.dylib that won't exist on an end user's machine. (The
    # system /usr/lib/libffi.dylib is too old to repoint to reliably.)
    #
    # Nixpkgs can't currently give us a cross-compiled x86_64-darwin libffi.a when
    # we're building on aarch64-darwin, so we bundle one in the repo for that target.
    # Detecting the build machine's arch here is deliberately difficult
    # (builtins.currentSystem is an "impure builtin"), so we key off the target.
    libffi = if pkgs.stdenv.targetPlatform.system == "x86_64-darwin"
             then "${../assets/libffi.a}"
             else "${pkgs.pkgsStatic.libffi}/lib/libffi.a";
  in
    [
      ''--ghc-options="-optl-Wl,-dead_strip -optl-Wl,-dead_strip_dylibs -optl-Wl,-force_load,${libffi}"''
    ];
}

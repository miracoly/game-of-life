{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {inherit system;};
      cmakeDeps = with pkgs; [
        llvmPackages_19.libcxx
        ncurses
        gtest
      ];
      cmakePrefix = pkgs.lib.concatStringsSep ":" (map (p: "${p}") cmakeDeps);
    in {
      devShells.default = pkgs.mkShell {
        nativeBuildInputs = with pkgs; [
          cmake
          clang-tools
          llvmPackages_19.clang
          ninja
        ];
        buildInputs = cmakeDeps;
        shellHook = ''
          export CMAKE_PREFIX_PATH=${cmakePrefix}:$CMAKE_PREFIX_PATH
          export CC=clang
          export CXX=clang++
        '';
      };
    });
}

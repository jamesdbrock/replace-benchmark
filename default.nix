{ nixpkgsSrc ? <nixpkgs>, pkgs ? import nixpkgsSrc { }, compiler ? null }:

let
  haskellPackages = if compiler == null then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

in haskellPackages.developPackage {
  root = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
  modifier = drv:
    with pkgs;
    haskell.lib.addBuildTools drv [ python3 linuxPackages.perf ];
}

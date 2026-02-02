{ pkgs, ... }:
{
  # Used to find the project root
  projectRootFile = "flake.nix";
  # Enable the terraform formatter
  programs.fourmolu.enable = true;
  programs.nixpkgs-fmt.enable = true;
  programs.cabal-fmt.enable = true;
  programs.hlint.enable = false;
  programs.mdformat.enable = true;
  programs.yamlfmt.enable = true;
  programs.toml-sort.enable = true;
  programs.beautysh.enable = true;
  # Override the default package
  programs.fourmolu.package = pkgs.haskellPackages.fourmolu;
  programs.fourmolu.ghcOpts = [
    "BangPatterns"
    "PatternSynonyms"
    "TypeApplications"
    "OverloadedRecordDot"
    "LambdaCase"
    "NamedFieldPuns"
    "NumericUnderscores"
    "RecordWildCards"
  ];
}

{ pkgs ? import <nixpkgs> {} }:
  pkgs.haskellPackages.callPackage ./ses-html.nix {}

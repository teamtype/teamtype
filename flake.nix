# SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
# SPDX-FileCopyrightText: 2026 blinry <mail@blinry.org>
#
# SPDX-License-Identifier: AGPL-3.0-or-later
{
  description = "Enables real-time co-editing of local text files.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-for-eslint.url = "github:nixos/nixpkgs/80d901ec0377e19ac3f7bb8c035201e2e098cc97";
    flake-parts.url = "github:hercules-ci/flake-parts";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      perSystem =
        {
          config,
          self',
          pkgs,
          lib,
          system,
          ...
        }:
        let
          runtimeDeps = with pkgs; [
            libgit2
          ];
          buildDeps = with pkgs; [
            pkg-config
          ];
          pkgsForEslint = import inputs.nixpkgs-for-eslint { inherit system; };
          devDeps = with pkgs; [
            cargo-deny
            git
            git-cliff
            just
            luaPackages.luacheck
            prettier
            reuse
            rustup
            stylua
            typos
            pkgsForEslint.eslint # TODO: Remove pkgsForEslint once we upgrade to eslint 10.
          ];

          workspaceToml = builtins.fromTOML (builtins.readFile ./Cargo.toml);
          cargoToml = builtins.fromTOML (builtins.readFile ./crates/teamtype/Cargo.toml);

          resolveManifestValue =
            key:
            if builtins.isAttrs cargoToml.package.${key} && cargoToml.package.${key} ? workspace then
              workspaceToml.workspace.package.${key}
            else
              cargoToml.package.${key};

          msrv = resolveManifestValue "rust-version";

          rustPlatform = pkgs.makeRustPlatform {
            cargo = pkgs.rust-bin.stable.latest.minimal;
            rustc = pkgs.rust-bin.stable.latest.minimal;
          };

          rustPackage =
            features:
            rustPlatform.buildRustPackage {
              name = resolveManifestValue "name";
              version = resolveManifestValue "version";
              src = ./.;
              cargoLock.lockFile = ./Cargo.lock;
              buildFeatures = features;
              buildInputs = runtimeDeps;
              nativeBuildInputs = buildDeps;
              env = {
                LIBGIT2_NO_VENDOR = 1;
              };
              doCheck = false;
            };

          mkDevShell = pkgs.mkShell {
            buildInputs = runtimeDeps;
            nativeBuildInputs = buildDeps ++ devDeps;
          };
        in
        {
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [ (import inputs.rust-overlay) ];
          };

          packages.teamtype = rustPackage [ ];
          packages.default = self'.packages.teamtype;
          devShells.default = mkDevShell;
          formatter = pkgs.nixfmt;
        };
    };
}

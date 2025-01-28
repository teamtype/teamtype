{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = {nixpkgs, ...}: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    devShells.${system}.default = pkgs.mkShell {
      #nativeBuildInputs = with pkgs; [
      #  openssl
      #];
      PKG_CONFIG_PATH = "${pkgs.openssl.dev}/lib/pkgconfig";
    };
  };
}

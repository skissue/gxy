{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };
  outputs = {
    self,
    nixpkgs,
  }: {
    packages.x86_64-linux.default = let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in
      pkgs.callPackage ./package.nix {};

    overlays.default = final: prev: [
      (final.callPackage ./package.nix {})
    ];

    nixosModules.default = ./module.nix;
  };
}

{
  description = "AirVPN Suite - OpenVPN 3 and WireGuard VPN suite for Linux";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.callPackage ./default.nix { };
          airvpn-suite = self.packages.${system}.default;
        }
      );

      overlays.default = final: prev: {
        airvpn-suite = final.callPackage ./default.nix { };
      };

      nixosModules.default = { config, lib, pkgs, ... }:
        let
          cfg = config.services.airvpn;
        in
        {
          options.services.airvpn = {
            enable = lib.mkEnableOption "AirVPN Bluetit daemon";

            package = lib.mkOption {
              type = lib.types.package;
              default = self.packages.${pkgs.system}.default;
              description = "The AirVPN Suite package to use.";
            };
          };

          config = lib.mkIf cfg.enable {
            environment.systemPackages = [ cfg.package ];

            systemd.packages = [ cfg.package ];

            systemd.services.bluetit.wantedBy = [ "multi-user.target" ];

            services.dbus.packages = [ cfg.package ];
          };
        };
    };
}

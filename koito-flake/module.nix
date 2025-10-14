{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.services.koito;
in {
  options.services.koito = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable Koito, a self-hosted music scrobbler";
    };
    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.koito;
      description = "Package to use for Koito";
    };
    allowedHosts = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = ["${cfg.bindAddress}:${toString cfg.port}"];
      description = "List of allowed hosts for Koito server";
    };
    bindAddress = lib.mkOption {
      type = lib.types.str;
      default = "localhost";
      description = "Address to bind the Koito server to";
    };
    port = lib.mkOption {
      type = lib.types.int;
      default = 4110;
      description = "Port for the Koito server";
    };
  };

  config = lib.mkIf cfg.enable {
    users = {
      users.koito = {
        group = "koito";
        isSystemUser = true;
      };
      groups.koito = {};
    };

    services.postgresql = {
      enable = true;
      ensureDatabases = ["koito"];
      ensureUsers = [
        {
          name = "koito";
          ensureDBOwnership = true;
        }
      ];
    };

    systemd.services.koito = {
      description = "Koito server";
      after = ["network.target"];
      wantedBy = ["multi-user.target"];
      environment = {
        KOITO_DATABASE_URL = "postgresql://koito@localhost/koito";
        KOITO_ALLOWED_HOSTS = lib.concatStringsSep "," cfg.allowedHosts;
        KOITO_BIND_ADDRESS = cfg.bindAddress;
        KOITO_PORT = toString cfg.port;
        KOITO_CONFIG_DIR = "/var/lib/koito";
      };
      serviceConfig = {
        ExecStart = "${cfg.package}/bin/koito";
        Restart = "on-failure";
        User = "koito";
        Group = "koito";
        # Koito uses the working directory to find its frontend files.
        WorkingDirectory = "${cfg.package}";
        StateDirectory = "koito";
      };
    };
  };
}

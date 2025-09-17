{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    anisette-v3-server = {
      url = "github:Dadoum/anisette-v3-server";
      flake = false;
    };
  };

  outputs = {
    self,
    nixpkgs,
    anisette-v3-server,
  }: let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
  in {
    packages.x86_64-linux.default = pkgs.buildDubPackage {
      pname = "anisette-v3-server";
      version = "unstable-${pkgs.lib.substring 0 8 anisette-v3-server.lastModifiedDate}";

      src = anisette-v3-server.outPath;

      patches = [./sources.patch];

      nativeBuildInputs = with pkgs; [pkg-config makeWrapper];

      buildInputs = with pkgs; [openssl zlib];

      dubLock = ./dub-lock.json;

      installPhase = ''
        runHook preInstall

        mkdir -p $out/bin
        cp anisette-v3-server $out/bin/

        wrapProgram $out/bin/anisette-v3-server \
            --prefix LD_LIBRARY_PATH : ${with pkgs; lib.makeLibraryPath [libplist]}

        runHook postInstall
      '';
    };
  };
}

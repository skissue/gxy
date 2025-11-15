{pkgs ? import <nixpkgs> {}}:
with pkgs;
  mkShell {
    nativeBuildInputs = [rustc cargo pkg-config];
    buildInputs = [rustfmt openssl.dev sqlx-cli];
    LD_LIBRARY_PATH = lib.makeLibraryPath [
      openssl
    ];
    DATABASE_URL = "sqlite:todos.db";
  }

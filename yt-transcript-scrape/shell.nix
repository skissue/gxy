{pkgs ? import <nixpkgs> {}}:
with pkgs;
  mkShell {
    buildInputs = [uv];
    shellHook = ''
      source .venv/bin/activate
    '';
  }

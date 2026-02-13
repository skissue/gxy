{pkgs ? import <nixpkgs> {}}:
with pkgs;
  mkShell {
    buildInputs = [uv];
    shellHook = ''
      source .venv/bin/activate
      export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt
      export PYTORCH_ALLOC_CONF=expandable_segments:True
      export TORCH_ROCM_AOTRITON_ENABLE_EXPERIMENTAL=1
      export PYTORCH_TUNABLEOP_ENABLED=1
      export MIOPEN_FIND_MODE=FAST
    '';
  }

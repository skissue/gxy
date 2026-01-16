{
  lib,
  buildGoModule,
  fetchFromGitHub,
  pkg-config,
  vips,
  callPackage,
}:
buildGoModule (finalAttrs: {
  pname = "Koito";
  version = "0.1.4";

  src = fetchFromGitHub {
    owner = "gabehf";
    repo = "Koito";
    tag = "v${finalAttrs.version}";
    hash = "sha256-PGxDtKIznvIdg9ACtksKA56oJPMp68Grzjvl98SEDL8=";
  };

  vendorHash = "sha256-e/gU29rPQUY+eugQxnjbb8UCJ3K4KCtRqJzBl5eFNxg=";

  passthru.ui = callPackage ./ui.nix {koito = finalAttrs.finalPackage;};

  nativeBuildInputs = [
    pkg-config
  ];

  buildInputs = [vips];

  subPackages = ["cmd/api"];

  ldflags = [
    "-s"
    "-w"
  ];

  postInstall = ''
    mv $out/bin/api $out/bin/koito
    ln -s ${finalAttrs.passthru.ui} $out/client
  '';

  doCheck = false;

  meta = {
    homepage = "https://github.com/gabehf/Koito";
    description = "Koito is a modern, themeable scrobbler that you can use with any program that scrobbles to ListenBrainz";
    longDescription = ''
      Koito is a modern, themeable ListenBrainz-compatible scrobbler for self-hosters who want control over their data and insights into their listening habits. It supports relaying to other compatible scrobblers, so you can try it safely without replacing your current setup.
    '';
    license = lib.licenses.mit;
    mainProgram = "koito";
  };
})

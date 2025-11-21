{
  koito,
  stdenv,
  fetchYarnDeps,
  yarnConfigHook,
  yarnBuildHook,
  nodejs,
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "${koito.pname}-client";
  inherit (koito) version;

  src = "${koito.src}/client";

  yarnOfflineCache = fetchYarnDeps {
    yarnLock = "${finalAttrs.src}/yarn.lock";
    hash = "sha256-Z1mWNi55agNDvnTTt7cfVi7pQCBpQCACxVmyPvYAVHs=";
  };

  nativeBuildInputs = [
    yarnConfigHook
    yarnBuildHook
    
    # Needed for executing package.json scripts
    nodejs
  ];

  installPhase = ''
    mkdir $out
    cp -r build/ $out
    cp -r public/ $out
  '';

  meta =
    (removeAttrs koito.meta ["mainProgram"])
    // {
      description = "${koito.meta.description} - Client";
    };
})

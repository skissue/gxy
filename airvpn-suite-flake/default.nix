{
  lib,
  stdenv,
  fetchFromGitLab,
  fetchFromGitHub,
  pkg-config,
  openssl,
  dbus,
  libxml2,
  cryptopp,
  curl,
  lz4,
  zlib,
  xz,
  zstd,
  fmt,
}:

let
  openvpn3-airvpn = fetchFromGitHub {
    owner = "AirVPN";
    repo = "openvpn3-airvpn";
    rev = "5d12f464715c3e7d91b8d910f50fcace092e2002";
    hash = "sha256-43ARVXNPZ7vsz+7CXruV0/4x6/og7fDXBzq1aTvIrOE=";
  };

  asio = fetchFromGitHub {
    owner = "chriskohlhoff";
    repo = "asio";
    rev = "asio-1-30-2";
    hash = "sha256-g+ZPKBUhBGlgvce8uTkuR983unD2kbQKgoddko7x+fk=";
  };
in
stdenv.mkDerivation {
  pname = "airvpn-suite";
  version = "2.0.0";

  src = fetchFromGitLab {
    owner = "AirVPN";
    repo = "AirVPN-Suite";
    rev = "master";
    hash = "sha256-PWJVgiRxs0hEUzDWicWodnFsbkeQZoi8toiqoj7EZJA=";
  };

  patches = [
    ./nixos-path-lookup.patch
    ./nixos-kernel-modules-path.patch
  ];

  nativeBuildInputs = [
    pkg-config
  ];

  buildInputs = [
    openssl
    dbus
    libxml2
    cryptopp
    curl
    lz4
    zlib
    xz
    zstd
    fmt
  ];

  enableParallelBuilding = true;

  preBuild = ''
    mkdir -p obj

    export OPENVPN3="${openvpn3-airvpn}"
    export ASIO="${asio}"
  '';

  buildPhase = ''
    runHook preBuild

    DBUS_CFLAGS="$(pkg-config --cflags dbus-1)"
    DBUS_LIBS="$(pkg-config --libs dbus-1)"
    XML2_CFLAGS="$(pkg-config --cflags libxml-2.0)"

    COMMON_CXXFLAGS="-fwhole-program -Ofast -Wall -Wno-sign-compare -Wno-unused-parameter -std=c++20 -flto=auto -Wl,--no-as-needed -Wunused-local-typedefs -Wunused-variable -Wno-shift-count-overflow -Wno-format-security -Wno-maybe-uninitialized -Wno-stringop-truncation -Wno-use-after-free"
    SSL_DEF="-DUSE_OPENSSL"
    SSL_LIBS="-lssl -lcrypto"

    echo "=== Compiling C sources ==="
    for f in execproc.c loadmod.c wireguard.c; do
      echo "Compiling $f"
      gcc -Wall -c -o obj/''${f%.c}.o src/$f
    done

    echo "=== Building bluetit ==="
    g++ $COMMON_CXXFLAGS -pthread \
      $SSL_DEF \
      -DMAKING_BLUETIT \
      -DUSE_ASIO -DASIO_STANDALONE -DASIO_NO_DEPRECATED \
      -I$ASIO/asio/include \
      -DHAVE_LZ4 \
      -I$OPENVPN3 -I$OPENVPN3/openvpn \
      $DBUS_CFLAGS $XML2_CFLAGS \
      src/bluetit.cpp \
      src/vpnclient.cpp \
      src/dbusconnector.cpp \
      src/network.cpp \
      src/dnsmanager.cpp \
      src/netfilter.cpp \
      src/rcparser.cpp \
      src/optionparser.cpp \
      src/trafficsplit.cpp \
      src/base64.cpp \
      src/airvpntools.cpp \
      src/airvpnmanifest.cpp \
      src/airvpnserver.cpp \
      src/airvpnuser.cpp \
      src/countrycontinent.cpp \
      src/airvpnservergroup.cpp \
      src/cipherdatabase.cpp \
      src/airvpnserverprovider.cpp \
      src/wireguardclient.cpp \
      src/semaphore.cpp \
      $OPENVPN3/openvpn/crypto/data_epoch.cpp \
      obj/execproc.o obj/loadmod.o obj/wireguard.o \
      -lcryptopp -lcurl -lxml2 $SSL_LIBS -llz4 -lz -llzma -lzstd \
      $DBUS_LIBS \
      -o bluetit

    echo "=== Building goldcrest ==="
    g++ $COMMON_CXXFLAGS -pthread \
      $DBUS_CFLAGS $XML2_CFLAGS \
      src/goldcrest.cpp \
      src/dbusconnector.cpp \
      src/airvpntools.cpp \
      src/rcparser.cpp \
      src/optionparser.cpp \
      $DBUS_LIBS \
      -o goldcrest

    echo "=== Building hummingbird ==="
    g++ $COMMON_CXXFLAGS -pthread \
      $SSL_DEF \
      -DUSE_ASIO -DASIO_STANDALONE -DASIO_NO_DEPRECATED \
      -I$ASIO/asio/include \
      -DHAVE_LZ4 \
      -I$OPENVPN3 -I$OPENVPN3/openvpn \
      src/hummingbird.cpp \
      src/vpnclient.cpp \
      src/network.cpp \
      src/dnsmanager.cpp \
      src/netfilter.cpp \
      src/wireguardclient.cpp \
      src/airvpntools.cpp \
      src/optionparser.cpp \
      src/semaphore.cpp \
      $OPENVPN3/openvpn/crypto/data_epoch.cpp \
      obj/execproc.o obj/loadmod.o obj/wireguard.o \
      $SSL_LIBS -llz4 -lz -llzma -lzstd \
      -o hummingbird

    echo "=== Building cuckoo ==="
    g++ $COMMON_CXXFLAGS -pthread \
      $XML2_CFLAGS \
      src/cuckoo.cpp \
      src/optionparser.cpp \
      src/airvpntools.cpp \
      obj/execproc.o \
      -lxml2 \
      -o cuckoo

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    mkdir -p $out/share/dbus-1/system.d
    mkdir -p $out/share/bash-completion/completions
    mkdir -p $out/lib/systemd/system

    # Install binaries
    install -m755 bluetit $out/bin/
    install -m755 goldcrest $out/bin/
    install -m755 hummingbird $out/bin/
    install -m755 cuckoo $out/bin/
    install -m755 AirVPN-Suite/bin/airsu $out/bin/

    # Install D-Bus configuration
    install -m644 AirVPN-Suite/etc/dbus-1/system.d/*.conf $out/share/dbus-1/system.d/

    # Install bash completions
    install -m644 AirVPN-Suite/etc/bash-completion/airsu $out/share/bash-completion/completions/
    install -m644 AirVPN-Suite/etc/bash-completion/cuckoo $out/share/bash-completion/completions/
    install -m644 AirVPN-Suite/etc/bash-completion/goldcrest $out/share/bash-completion/completions/
    install -m644 AirVPN-Suite/etc/bash-completion/hummingbird $out/share/bash-completion/completions/

    # Install systemd services with correct paths
    substitute AirVPN-Suite/etc/systemd/system/bluetit.service $out/lib/systemd/system/bluetit.service \
      --replace-fail "/sbin/bluetit" "$out/bin/bluetit"
    install -m644 AirVPN-Suite/etc/systemd/system/bluetit-suspend.service $out/lib/systemd/system/
    install -m644 AirVPN-Suite/etc/systemd/system/bluetit-resume.service $out/lib/systemd/system/

    runHook postInstall
  '';

  meta = with lib; {
    description = "AirVPN's OpenVPN 3 and WireGuard suite for Linux";
    longDescription = ''
      AirVPN Suite is a collection of applications providing VPN connectivity
      to AirVPN servers and generic OpenVPN/WireGuard servers. Includes:
      - Bluetit: D-Bus controlled system daemon
      - Goldcrest: Command line client for Bluetit
      - Hummingbird: Standalone VPN client
      - Cuckoo: Traffic split tool
      - Airsu: Switch user tool for traffic splitting
    '';
    homepage = "https://gitlab.com/AirVPN/AirVPN-Suite";
    license = licenses.gpl3Plus;
    platforms = platforms.linux;
    maintainers = [ ];
    mainProgram = "goldcrest";
  };
}

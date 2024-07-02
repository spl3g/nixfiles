{ lib
, stdenv
, fetchurl
, dpkg
, wrapGAppsHook3
, makeWrapper
, glib
, libX11
, libXext
, libdrm
, libxcb
, libXcomposite
, libXdamage
, libXfixes
, libXrandr
, libxkbcommon
, cairo
, gtk3
, pango
, cups
, expat
, dbus
, nspr
, nss
, mesa
, atk
, alsa-lib
, libGL
, vulkan-loader
, wayland
, systemd
}:

stdenv.mkDerivation rec {
  pname = "hydralauncher";
  version = "2.0.2";

  src = fetchurl {
    url = "https://github.com/hydralauncher/hydra/releases/download/v${version}/hydralauncher_${version}_amd64.deb";
    sha256 = "1zy6qpfd8x24gfw7jzkpla0ayj9l6pxqwpjprdlzz83wwrzb5imk";
  };

  unpackPhase = "dpkg-deb -x $src .";

  nativeBuildInputs = [
    dpkg
    wrapGAppsHook3
    makeWrapper
  ];

  buildInputs = [
    stdenv.cc.cc.lib
    glib
    libX11
    libXext
    libdrm
    libxcb
    libXcomposite
    libXdamage
    libXfixes
    libXrandr
    libxkbcommon
    cairo
    gtk3
    pango
    cups
    expat
    dbus
    nspr
    nss
    mesa
    atk
    alsa-lib
    libGL
    vulkan-loader
    wayland
    systemd
  ];

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    cp -r opt $out
    cp -r usr $out
    ln -s $out/opt/Hydra/hydralauncher $out/bin

    source "${makeWrapper}/nix-support/setup-hook"
    wrapProgram $out/bin/${pname} \
        --add-flags "\''${NIXOS_OZONE_WL:+\''${WAYLAND_DISPLAY:+--enable-features=UseOzonePlatform --ozone-platform=wayland}}"

    runHook postInstall
  '';

  postFixup = ''
    pushd $out/opt/Hydra
    patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" hydralauncher
    patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" chrome_crashpad_handler
    patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" chrome-sandbox
    patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" resources/hydra-download-manager/hydra-download-manager
    for file in $(find . -type f \( -name hydra-download-manager -o -name hydralauncher -o -name \*.so\* \) ); do
      patchelf --set-rpath "${lib.makeLibraryPath buildInputs}:$(patchelf --print-rpath $file)" $file
    done

    rm libvulkan.so.1
    ln -s -t . "${lib.getLib vulkan-loader}/lib/libvulkan.so.1"
    popd
  '';
}

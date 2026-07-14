{
  pkgs,
  config,
  ...
}: {
  # You can import other NixOS modules here
  imports = [
    # If you want to use modules your own flake exports (from modules/nixos):
    # outputs.nixosModules.example

    # Or modules from other flakes (such as nixos-hardware):
    # inputs.hardware.nixosModules.common-cpu-amd
    # inputs.hardware.nixosModules.common-ssd

    ../general.nix
    ./hardware-configuration.nix
    ../nixosModules/powerbutton.nix
    ../nixosModules/docker.nix
    ../nixosModules/greetd.nix
    ./disk-config.nix
  ];

  # from nixosModules
  pbutton.disable = true;
  greetd.command = "Hyprland";

  programs.gamescope = {
    enable = true;
    capSysNice = false;
  };

  time.timeZone = "Europe/Moscow";
  networking.hostName = "ltrr-mini";
  networking.hosts = {
    "127.0.0.1" = ["mr.local" "local.oneln.ru"];
    "127.0.0.3" = ["local-api.oneln.ru"];
  };

  services.sshd.enable = true;

  networking.firewall.allowedUDPPorts = [16261 16262];

  security.pki.certificates = [
    ''
      -----BEGIN CERTIFICATE-----
      MIIDazCCAlOgAwIBAgIUbyizxOOSWxlvjptsiaCVuOcri+IwDQYJKoZIhvcNAQEL
      BQAwPDEdMBsGA1UEChMUU3Vid2F5RXhpdCBUcnVzdCBMTEMxGzAZBgNVBAMTElN1
      YndheUV4aXQgUm9vdCBDQTAgFw0yNTAzMjMxNjM2MDBaGA8yMDU1MDMxNjE2MzYz
      MFowPDEdMBsGA1UEChMUU3Vid2F5RXhpdCBUcnVzdCBMTEMxGzAZBgNVBAMTElN1
      YndheUV4aXQgUm9vdCBDQTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEB
      ANmGfcIyimPIb8a3iZwqAtVFQDT6DUxjP+iDHZkeJQcWA1GWpCHwf6gJVSLOivew
      wWfQVHdf7Tl+ZiHmjhx5S2fViPxhu+uxyA7hIGnJiBja6COyF1nKXG98A2sZyKWh
      JKLVhiAQOEhWR+DgNiJ4uzjrHX/mTTm4JH78tmGIJUEtP5V7vKHmA9+KejuWbLYr
      uhogNrPC6QEVL/EtZYukDFV/u5i5nJT2x2B7z2tENFPcO2KmzmYYTCKZN6ZTPIgQ
      jgwlhaik47bPwTmvS9fCRcV6TmMXHhjPW1qNCOX3P9P+99BcNaMeFjr/Cok9IJZg
      efEmaYV64Llowc5Nhtp21hkCAwEAAaNjMGEwDgYDVR0PAQH/BAQDAgEGMA8GA1Ud
      EwEB/wQFMAMBAf8wHQYDVR0OBBYEFKjVAY0vbJiv1L90OUY+CtwtsMqMMB8GA1Ud
      IwQYMBaAFKjVAY0vbJiv1L90OUY+CtwtsMqMMA0GCSqGSIb3DQEBCwUAA4IBAQAw
      2yqEGnYjKj6mJXZ7pLNgr38WD+SQHYnkQkXEkz9hPJkQfbw8KJYmhsYTysjm8YsW
      DD2vXB9tMgkIceF3D/AhLvBSllaziGWkT1jYnlbmaP14fqInDAU37pObVvXnOyKK
      eH6JRvHID14f0h1oi3kCX6ePlzx46I/mdKGPf0xnlSTMvTXPJjZ4oUqv9pghN4/N
      ZxqttSnc9R72mFa9Vqp1cEBvfqi2/u9ShCUMuRnMljXjQIwBlLP/nfoK/fsRyXsL
      H+eY1Eq5rBptBO2NRxGP4rWFUjwy6Eb7r4is3+NFSIM8dYnmKtrbnqGN/VBYmO0n
      zOj30i1NZg2qlNUojFwF
      -----END CERTIFICATE-----
    ''
    ''
      -----BEGIN CERTIFICATE-----
      MIID7TCCAtWgAwIBAgIUWR+TcUn3HRFGabER+zatjhBEfKYwDQYJKoZIhvcNAQEL
      BQAwPDEdMBsGA1UEChMUU3Vid2F5RXhpdCBUcnVzdCBMTEMxGzAZBgNVBAMTElN1
      YndheUV4aXQgUm9vdCBDQTAeFw0yNTAzMjMxNjM2MDJaFw00NTAzMTgxNjM2MzJa
      MEQxHTAbBgNVBAoTFFN1YndheUV4aXQgVHJ1c3QgTExDMSMwIQYDVQQDExpTdWJ3
      YXlFeGl0IEludGVybWVkaWF0ZSBDQTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCC
      AQoCggEBALkvGrUzuezbf0uQsDd7bSSC5LYlW1vD6Ep41T5iSwjKwv0fGihVDK5Z
      Xn4Tn12nc/caWMu6DDSdUHzbmjbkaX1mFAlZAKO3g3VVFcDnlInsDTMhxp0DQVYJ
      C2r3b8sncLfhXWLnIy0+lBtnlnZm3atTg9Gbq2w9rvhOPImDQIsYsCc43p2/nYcZ
      hNDipJDVSU4+YpeheUlSEtYhzzRvFIcs0YcdetRQv6jdBw5loX1k4oRLlPXMGS0q
      0OgkFcWYOYkVB7DGANvms23/Y7i8yoj7wXWuR6I9p2WsS9f5wxHcDvBd7mMsaJQI
      KuoD0sgOdjxnLSQ0ZXG1y8KfdjS/aPECAwEAAaOB3jCB2zAOBgNVHQ8BAf8EBAMC
      AQYwDwYDVR0TAQH/BAUwAwEB/zAdBgNVHQ4EFgQU55vqV5OMfS8jNQXEh7Hn+JGm
      cqwwHwYDVR0jBBgwFoAUqNUBjS9smK/Uv3Q5Rj4K3C2wyowwQAYIKwYBBQUHAQEE
      NDAyMDAGCCsGAQUFBzAChiRodHRwOi8vdmF1bHQuZGVsdGEvdjEvcGtpX3Jvb3Rf
      Y2EvY2EwNgYDVR0fBC8wLTAroCmgJ4YlaHR0cDovL3ZhdWx0LmRlbHRhL3YxL3Br
      aV9yb290X2NhL2NybDANBgkqhkiG9w0BAQsFAAOCAQEABkfz+caRZaRPqL6zkFpW
      zQ+VxoWQzhH9po6HsWtVOaXh1IrkZbR4n5qLDKbg+QS7WqIRnVe2lpLqDVnE/jTD
      /qZGJTaj3n9eEOdd3ZiT5yXfrzTOSXyO9HVyQfg2EIQkFr9UucH45Y25nnT5LaRm
      qrBg13QmE4IODm5GeTXqEmWj4E8jBXFHAfVYXJ6jdaFTp7q6iLIVeE17yP7+8NCr
      9Va4a3JnMMa8d6DGR7+87RSnXyT9XSsToVMgd/dmkb01/JDp510xfQ0wKHPfepQL
      IMhXpLEIlpAr/IP0lF9oCCYN3NWy0XrQzgofeQkBdZL2SZ9QHKULEs8NV0xoJET8
      Cw==
      -----END CERTIFICATE-----
    ''
  ];

  virtualisation.waydroid.enable = true;
  services.upower.enable = true;

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "23.05";
}

{ pkgs, inputs, outputs, lib, config, ... }:

{
  nixpkgs = {
    overlays = [
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages
    ];
    config = {
      allowUnfree = true;
    };
  };

  nix = {
    # This will add each flake input as a registry
    # To make nix3 commands consistent with your flake
    registry = lib.mapAttrs (_: value: { flake = value; }) inputs;

    # This will additionally add your inputs to the system's legacy channels
    # Making legacy nix commands consistent as well, awesome!
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;

    gc = {
      automatic = true;
      dates = "weekly";
    };

    settings = {
      # Enable flakes and new 'nix' command
      experimental-features = "nix-command flakes";
      # Deduplicate and optimize nix store
      auto-optimise-store = true;

      trusted-users = [ "root" "@wheel" ];
    };
  };

  virtualisation.docker.enable = true;

  # services.nginx = {
  #   enable = true;
  #   virtualHosts."kcu.su" = {
  #     forceSSL = true;
  #     enableACME = true;
  #     locations."/xray" = {
  #       proxyPass = "http://127.0.0.1:42069";
  #       extraConfig = "proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
  #         proxy_set_header X-Forwarded-Proto $scheme;
  #         proxy_set_header Host $http_host;
  #         proxy_set_header X-Real-IP $remote_addr;
  #         proxy_set_header Range $http_range;
  #         proxy_set_header If-Range $http_if_range; 
  #         proxy_redirect off;";
  #     };
  #   };
  # };

  services.k3s = {
    enable = true;
    role = "server";
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = "notspl3g@duck.com";
  };
  
  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "24.04";
}

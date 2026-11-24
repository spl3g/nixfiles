{
  lib,
  self,
  pkgs,
  config,
  inputs,
  ...
}:

{
  imports = [
    self.nixosModules.services.nginxProxy
    self.nixosModules.system.nix
  ];
  
  services.openssh = {
    enable = true;
  };

  environment.systemPackages = map lib.lowPrio [
    pkgs.curl
    pkgs.gitMinimal
  ];

  users.users = {
    root = {
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDJ8UW1BXDGDmlaiARO3a9boTG8wknUyITMz0Z0OJpHx spleefer6@yandex.ru"
      ];
    };
  };

  age.secrets.wg-priv-key = {
    rekeyFile = ../../secrets/cloud/wg-priv.key.age;
  };
  networking.wg-quick = {
    interfaces.wg0 = {
      address = ["10.1.1.1/32"];
      listenPort = 51820;

      privateKeyFile = config.age.secrets.wg-priv-key.path;

      preUp = ''
        sysctl -w net.ipv4.ip_forward=1
        iptables -t nat -A POSTROUTING -s 10.1.1.3/32 -o ens3 -j MASQUERADE
      '';

      postDown = ''
        sysctl -w net.ipv4.ip_forward=0
        iptables -t nat -D POSTROUTING -s 10.1.1.3/32 -o ens3 -j MASQUERADE
      '';

      peers = [
        {
          allowedIPs = ["10.1.1.2/32"];
          publicKey = "kzXzxJu1AdcRI5UwtGOrN6WuTZYqJo++PYRrXdOu/lY=";
          persistentKeepalive = 25;
        }
        {
          allowedIPs = ["10.1.1.3/32"];
          publicKey = "hs6RlxKO7UCfTAXUhDlRiMqg3kuwyNaQdp0ABpM5chM=";
          persistentKeepalive = 25;
        }
        {
          allowedIPs = ["10.1.1.4/32"];
          publicKey = "kosSScP+EUjzL9PvjGPREuMusP1IX0GBYbCLvLOmUHg=";
          persistentKeepalive = 25;
        }
      ];
    };
  };

  networking.firewall.allowedTCPPorts = [
    # http
    80
    443

    50300 # slskd
    6667 # irc
  ];
  networking.firewall.allowedUDPPorts = [
    51820 # wg
  ];

  nginxProxy = {
    enable = true;
    serverIp = "10.1.1.4";
    ports = [
      {port = 443; proxyProtocol = true;}
      {port = 80; proxyProtocol = true;}
      50300
      6667
    ];
  };
}

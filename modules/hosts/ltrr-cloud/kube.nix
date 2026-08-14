{
  inputs,
  self,
  ...
}: {
  flake.nixosModules.ltrr-cloud = {
    pkgs,
    config,
    ...
  }: {
    age.secrets.kube-token = {
      rekeyFile = ../ltrr-block/secrets/kube-token.key.age;
    };

    networking.firewall.allowedUDPPorts = [
      8472
    ];

    services.k3s = {
      enable = true;
      role = "agent";
      serverAddr = "https://10.1.1.2:6443";
      tokenFile = config.age.secrets.kube-token.path;
      nodeIP = "10.1.1.1";
      nodeLabel = ["ltrr-cloud"];
      extraFlags = [
        "--flannel-iface wg0"
        "--kubelet-arg=register-with-taints=dedicated=ingress:NoSchedule"
        "--kubelet-arg=node-labels=ingress-node=true"
      ];
    };
  };
}
